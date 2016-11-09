package com.netaporter.uri.parsing

import org.parboiled2._
import com.netaporter.uri._
import Parameters._

@deprecated("Convert to the new DSL.", "1.0.0")
class DefaultUriParser(val input: ParserInput, conf: UriConfig) extends Parser with UriParser {

  protected val originalInput = input.getLine(1)
  implicit protected val config = conf

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _scheme: Rule1[String] = rule {
    capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum | anyOf("+-.")))
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _host_name: Rule1[String] = rule {
    capture(oneOrMore(!anyOf(":/?") ~ ANY))
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _userInfo: Rule1[UserInfo] = rule {
    capture(oneOrMore(!anyOf(":/?@") ~ ANY)) ~ optional(":" ~ optional(capture(oneOrMore(!anyOf("@") ~ ANY)))) ~ "@" ~> extractUserInfo
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _port: Rule1[String] = rule {
    ":" ~ capture(oneOrMore(CharPredicate.Digit))
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _authority: Rule1[Authority] = rule {
    ((optional(_userInfo) ~ _host_name ~ optional(_port)) | (push[Option[UserInfo]](None) ~ _host_name ~ optional(_port))) ~> extractAuthority
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _pathSegment: Rule1[PathPart] = rule {
    capture(zeroOrMore(!anyOf("/?#") ~ ANY)) ~> extractPathPart
  }

  /**
   * A sequence of path parts that MUST start with a slash
   */
  @deprecated("Convert to the new DSL.", "1.0.0")
  def _abs_path: Rule1[Vector[PathPart]] = rule {
    zeroOrMore("/" ~ _pathSegment) ~> extractPathParts
  }

  /**
   * A sequence of path parts optionally starting with a slash
   */
  @deprecated("Convert to the new DSL.", "1.0.0")
  def _rel_path: Rule1[Vector[PathPart]] = rule {
    optional("/") ~ zeroOrMore(_pathSegment).separatedBy("/") ~> extractPathParts
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _queryParam: Rule1[Param] = rule {
    capture(zeroOrMore(!anyOf("=&#") ~ ANY)) ~ "=" ~ capture(zeroOrMore(!anyOf("&#") ~ ANY)) ~> extractTuple
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _queryTok: Rule1[Param] = rule {
    capture(zeroOrMore(!anyOf("=&#") ~ ANY)) ~> extractTok
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _queryString: Rule1[QueryString] = rule {
    "?" ~ zeroOrMore(_queryParam | _queryTok).separatedBy("&") ~> extractQueryString
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _fragment: Rule1[String] = rule {
    "#" ~ capture(zeroOrMore(ANY)) ~> extractFragment
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _abs_uri: Rule1[Uri] = rule {
    _scheme ~ "://" ~ optional(_authority) ~ _abs_path ~ optional(_queryString) ~ optional(_fragment) ~> extractAbsUri
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _protocol_rel_uri: Rule1[Uri] = rule {
    "//" ~ optional(_authority) ~ _abs_path ~ optional(_queryString) ~ optional(_fragment) ~> extractProtocolRelUri
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _rel_uri: Rule1[Uri] = rule {
    _rel_path ~ optional(_queryString) ~ optional(_fragment) ~> extractRelUri
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _uri: Rule1[Uri] = rule {
    (_abs_uri | _protocol_rel_uri | _rel_uri) ~ EOI
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  val extractAbsUri = (scheme: String, authority: Option[Authority], pp: Seq[PathPart], qs: Option[QueryString], f: Option[String]) =>
    extractUri(
      scheme = Some(scheme),
      authority = authority,
      pathParts = pp,
      query = qs,
      fragment = f
    )

  @deprecated("Convert to the new DSL.", "1.0.0")
  val extractProtocolRelUri = (authority: Option[Authority], pp: Seq[PathPart], qs: Option[QueryString], f: Option[String]) =>
    extractUri(
      authority = authority,
      pathParts = pp,
      query = qs,
      fragment = f
    )

  @deprecated("Convert to the new DSL.", "1.0.0")
  val extractRelUri = (pp: Seq[PathPart], qs: Option[QueryString], f: Option[String]) =>
    extractUri(
      pathParts = pp,
      query = qs,
      fragment = f
    )

  @deprecated("Convert to the new DSL.", "1.0.0")
  def extractUri(scheme: Option[String] = None,
                 authority: Option[Authority] = None,
                 pathParts: Seq[PathPart],
                 query: Option[QueryString],
                 fragment: Option[String]) =
    Uri(
      scheme = scheme,
      user = authority.flatMap(_.user),
      password = authority.flatMap(_.password),
      host = authority.map(_.host),
      port = authority.flatMap(_.port),
      pathParts = pathParts,
      query = query.getOrElse(null),
      fragment = fragment
    )

  @deprecated("Convert to the new DSL.", "1.0.0")
  def pathDecoder = conf.pathDecoder
  @deprecated("Convert to the new DSL.", "1.0.0")
  def queryDecoder = conf.queryDecoder
  @deprecated("Convert to the new DSL.", "1.0.0")
  def fragmentDecoder = conf.fragmentDecoder
}
