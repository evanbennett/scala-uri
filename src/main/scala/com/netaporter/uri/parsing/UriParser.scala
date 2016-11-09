package com.netaporter.uri.parsing

import scala.util.Failure
import org.parboiled2._
import com.netaporter.uri._
import com.netaporter.uri.decoding.UriDecoder
import com.netaporter.uri.Parameters._
import org.parboiled2.ParseError
import scala.util.Success
import scala.util.Failure

@deprecated("Convert to the new DSL.", "1.0.0")
trait UriParser {

  protected val originalInput: String
  implicit protected val config: UriConfig

  @deprecated("Convert to the new DSL.", "1.0.0")
  def pathDecoder: UriDecoder
  @deprecated("Convert to the new DSL.", "1.0.0")
  def queryDecoder: UriDecoder
  @deprecated("Convert to the new DSL.", "1.0.0")
  def fragmentDecoder: UriDecoder

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _pathSegment: Rule1[PathPart]

  @deprecated("Convert to the new DSL.", "1.0.0")
  val extractInt = (num: String) =>
    num.toInt

  @deprecated("Convert to the new DSL.", "1.0.0")
  val extractUserInfo = (user: String, pass: Option[Option[String]]) =>
    UserInfo(pathDecoder.decode(user, originalInput), pass.map(_.fold("")(pathDecoder.decode(_, originalInput))))

  @deprecated("Convert to the new DSL.", "1.0.0")
  val extractAuthority = (userInfo: Option[UserInfo], host: String, port: Option[String]) =>
    Authority(userInfo.map(_.user), userInfo.flatMap(_.pass), host, port.map(_.toInt))

  @deprecated("Convert to the new DSL.", "1.0.0")
  val extractFragment = (x: String) =>
    fragmentDecoder.decode(x, originalInput)

  @deprecated("Convert to the new DSL.", "1.0.0")
  val extractQueryString = (tuples: ParamSeq) =>
    QueryString(tuples.toVector.map(queryDecoder.decodeTuple(_, originalInput)))

  @deprecated("Convert to the new DSL.", "1.0.0")
  val extractPathPart = (pathPart: String) => {
    val decodedPathPart = pathDecoder.decode(pathPart, originalInput)
    StringPathPart(decodedPathPart)
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  val extractPathParts = (pp: Seq[PathPart]) =>
    pp.toVector

  @deprecated("Convert to the new DSL.", "1.0.0")
  val extractTuple = (k: String, v: String) =>
    k -> Some(v)

  @deprecated("Convert to the new DSL.", "1.0.0")
  val extractTok = (k: String) => (k -> None): (String, Option[String])

  /**
   * Used to made parsing easier to follow
   */
  @deprecated("Convert to the new DSL.", "1.0.0")
  case class Authority(user: Option[String], password: Option[String], host: String, port: Option[Int])
  @deprecated("Convert to the new DSL.", "1.0.0")
  case class UserInfo(user: String, pass: Option[String])
}

@deprecated("Convert to the new DSL.", "1.0.0")
object UriParser {

  @deprecated("Use `ParseUri.apply` instead.", "1.0.0")
  def parse(s: String, config: UriConfig) = {
    val parser =
      if (config.matrixParameterParsing) new DefaultUriParser(s, config) with MatrixParamSupport
      else                               new DefaultUriParser(s, config)

    parser._uri.run() match {
      case Success(uri) =>
        uri

      case Failure(pe @ ParseError(position, _, formatTraces)) =>
        throw new java.net.URISyntaxException(s, "Invalid URI could not be parsed. " + formatTraces, position.index)

      case Failure(e) =>
        throw e
    }
  }

  /**
   * THEON: This was not working properly:
   *   It does not provide `MatrixParameterParsing` support.
   *   It does not require the starting '?'.
   *   It does not end with `EOI`, so it parses until the string does not match and then skips the rest.
   */
  @deprecated("Use `ParseUri.apply` instead, ensuring the starting '?' and noting the `Uri` return type.", "1.0.0")
  def parseQuery(s: String, config: UriConfig) = {
    val withQuestionMark = if (s.head == '?') s else "?" + s
    val parser = new DefaultUriParser(withQuestionMark, config)

    parser._queryString.run() match {
      case Success(queryString) =>
        queryString

      case Failure(pe @ ParseError(position, _, formatTraces)) =>
        throw new java.net.URISyntaxException(s, "Invalid URI could not be parsed. " + formatTraces, position.index)

      case Failure(e) =>
        throw e
    }
  }
}
