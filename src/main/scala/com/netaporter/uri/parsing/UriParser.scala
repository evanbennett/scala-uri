package com.netaporter.uri.parsing

import scala.util.{Failure, Success}
import com.netaporter.uri._
import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.Parameters._
import org.parboiled2._

abstract class UriParser(val input: ParserInput, val c: UriConfig) extends Parser {

  val originalInput: String = input.getLine(1)

  def _segment: Rule1[Segment]

  /** Segment non-zero length. */
  def _segmentNz: Rule1[Segment]

  /** Segment non-zero length, no ':'s. */
  def _segmentNzNc: Rule1[Segment]

  val extractInt: String => Int = (num: String) =>
    num.toInt

  val extractScheme: String => Scheme = (scheme: String) =>
    Scheme(scheme)

  val extractUserInfo: (String, Option[String]) => UserInfo = (user: String, password: Option[String]) =>
    UserInfo(c.userInfoDecoder.decode(user, originalInput), password.map(c.userInfoDecoder.decode(_, originalInput)))

  val extractAuthority: (Option[UserInfo], String, Option[Int]) => Authority = (userInfo: Option[UserInfo], host: String, port: Option[Int]) =>
    Authority(userInfo, host, port)

  val extractSegment: String => StringSegment = (segment: String) =>
    StringSegment(c.pathDecoder.decode(segment, originalInput))

  val extractAbsolutePath: (Segment, Seq[Segment]) => AbsolutePath = (firstSegment: Segment, segments: Seq[Segment]) =>
    AbsolutePath(firstSegment +: segments.toVector)

  val extractEmptyAbsolutePath: (String) => AbsolutePath = (emptyString: String) =>
    EmptyAbsolutePath

  val extractRootlessPath: (Segment, Seq[Segment]) => RootlessPath = (firstSegment: Segment, segments: Seq[Segment]) =>
    RootlessPath.option(firstSegment +: segments.toVector).get

  val extractParam: (String, Option[String]) => Parameter = (key: String, value: Option[String]) =>
    Parameter(key, value)

  val extractQuery: Seq[Parameter] => Query = (params: Seq[Parameter]) =>
    Query(params.map(c.queryDecoder.decodeParameter(_, originalInput)))

  val extractFragment: String => Fragment = (fragment: String) =>
    Fragment(c.fragmentDecoder.decode(fragment, originalInput))
}

object UriParser {
  def parse(s: String, c: UriConfig) = {
    val parser =
      if (c.matrixParams) new DefaultUriParser(s, c) with MatrixParamSupport
      else                new DefaultUriParser(s, c)
    parser._uri.run() match {
      case Success(uri) =>
        uri
      case Failure(pe@ParseError(position, _, formatTraces)) =>
        throw new java.net.URISyntaxException(s, "Invalid URI could not be parsed. " + formatTraces, position.index)
      case Failure(e) =>
        throw e
    }
  }

  /**
   * NOTE: This was not working properly.
   *   For some reason it does not require the starting '?'.
   *   It does not end with `EOI` and so parses until of the string that matches and then skips the rest.
   */
  @deprecated("Use `parse` instead, ensuring the starting '?'.", "1.0.0")
  def parseQuery(s: String, c: UriConfig) = {
    val withQuestionMark = if (s.head == '?') s else "?" + s
    val parser = new DefaultUriParser(withQuestionMark, c)
    parser._query.run() match {
      case Success(query) =>
        query
      // NOTE: I cannot make this fail with a `ParseError`. Even this passes: "query#Key=queryValue"
      //case Failure(pe@ParseError(position, _, formatTraces)) =>
      //  throw new java.net.URISyntaxException(s, "Invalid URI could not be parsed. " + formatTraces, position.index)
      case Failure(e) =>
        throw e
    }
  }
}
