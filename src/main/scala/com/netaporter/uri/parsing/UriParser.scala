package com.netaporter.uri.parsing

import scala.util.{Failure, Success}
import com.netaporter.uri._
import com.netaporter.uri.config.UriConfig
import UriParser._
import org.parboiled2._

class UriParser(val input: ParserInput, val c: UriConfig) extends Parser {

  val originalInput: String = input.getLine(1)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  protected val extractInt: String => Int = (num: String) =>
    num.toInt

  protected val extractScheme: String => Scheme = (scheme: String) =>
    Scheme(scheme)

  protected val extractUserInfo: (String, Option[String]) => UserInfo = (user: String, password: Option[String]) =>
    UserInfo(c.userInfoDecoder.decode(user, originalInput), password.map(c.userInfoDecoder.decode(_, originalInput)))

  protected val extractRegisteredNameHost: (String) => Host = (registeredName: String) =>
    Host(c.hostDecoder.decode(registeredName, originalInput))

  protected val extractIpv4AddressHost: (String) => Host = (ipv4Address: String) =>
    Host(ipv4Address = ipv4Address)

  protected val extractIpLiteralHost: (String) => Host = (ipLiteralAddress: String) =>
    Host(ipLiteralAddress = ipLiteralAddress)

  protected val extractAuthority: (Option[UserInfo], Option[Host], Option[Int]) => Authority = (userInfo: Option[UserInfo], host: Option[Host], port: Option[Int]) =>
    Authority(userInfo, host, port)

  protected val extractSegment: String => StringSegment = (segment: String) =>
    StringSegment(c.pathDecoder.decode(segment, originalInput))

  protected val extractAbsolutePath: (Segment, Seq[Segment]) => AbsolutePath = (firstSegment: Segment, segments: Seq[Segment]) =>
    AbsolutePath(firstSegment +: segments.toVector)

  protected val extractEmptyAbsolutePath: (String) => AbsolutePath = (emptyString: String) =>
    EmptyAbsolutePath

  protected val extractRootlessPath: (Segment, Seq[Segment]) => RootlessPath = (firstSegment: Segment, segments: Seq[Segment]) =>
    RootlessPath.option(firstSegment +: segments.toVector).get

  protected val extractParam: (String, Option[String]) => Parameter = (key: String, value: Option[String]) =>
    Parameter(key, value)

  protected val extractQuery: Seq[Parameter] => Query = (params: Seq[Parameter]) =>
    Query(params.map(c.queryDecoder.decodeParameter(_, originalInput)))

  protected val extractFragment: String => Fragment = (fragment: String) =>
    Fragment(c.fragmentDecoder.decode(fragment, originalInput))

  // - - - - - - - - - -

  protected val extractAbsoluteUri = (scheme: Scheme, authority: Authority, path: Option[AbsolutePath], query: Option[Query]) =>
    AbsoluteUri(scheme, authority, path, query)

  protected val extractSchemeWithAuthorityAndFragmentUri = (scheme: Scheme, authority: Authority, path: Option[AbsolutePath], query: Option[Query], fragment: Fragment) =>
    SchemeWithAuthorityAndFragmentUri(scheme, authority, path, query, fragment)

  protected val extractSchemeWithAbsolutePathUri = (scheme: Scheme, path: AbsolutePath, query: Option[Query], fragment: Option[Fragment]) =>
    SchemeWithAbsolutePathUri(scheme, path, query, fragment)

  protected val extractSchemeWithRootlessPathUri = (scheme: Scheme, path: RootlessPath, query: Option[Query], fragment: Option[Fragment]) =>
    SchemeWithRootlessPathUri(scheme, path, query, fragment)

  protected val extractSchemeWithQueryUri = (scheme: Scheme, query: Query, fragment: Option[Fragment]) =>
    SchemeWithQueryUri(scheme, query, fragment)

  protected val extractSchemeWithFragmentUri = (scheme: Scheme, fragment: Fragment) =>
    SchemeWithFragmentUri(scheme, fragment)

  protected val extractSchemeUri = (scheme: Scheme) =>
    SchemeUri(scheme)

  protected val extractAuthorityRelativeReference = (authority: Authority, path: Option[AbsolutePath], query: Option[Query], fragment: Option[Fragment]) =>
    AuthorityRelativeReference(authority, path, query, fragment)

  protected val extractAbsolutePathRelativeReference = (path: AbsolutePath, query: Option[Query], fragment: Option[Fragment]) =>
    AbsolutePathRelativeReference(path, query, fragment)

  protected val extractRootlessPathRelativeReference = (path: RootlessPath, query: Option[Query], fragment: Option[Fragment]) =>
    RootlessPathRelativeReference(path, query, fragment)

  protected val extractQueryRelativeReference = (query: Query, fragment: Option[Fragment]) =>
    QueryRelativeReference(query, fragment)

  protected val extractFragmentRelativeReference = (fragment: Fragment) =>
    FragmentRelativeReference(fragment)

  protected val extractEmptyRelativeReference = (emptyString: String) =>
    EmptyRelativeReference

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  protected def _scheme: Rule1[Scheme] = rule {
    capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum | anyOf("+-."))) ~ ":" ~> extractScheme
  }

  @deprecated("Being made `protected`.", "1.0.0")
  def _userInfo: Rule1[UserInfo] = rule {
    capture(oneOrMore(USER)) ~ optional(":" ~ capture(zeroOrMore(USER_INFO))) ~ "@" ~> extractUserInfo
  }

  protected def _decimalOctet: Rule0 = rule {
    ("25" ~ anyOf("012345")) | ("2" ~ anyOf("01234") ~ CharPredicate.Digit) | ("1" ~ 2.times(CharPredicate.Digit)) | (CharPredicate.Digit19 ~ CharPredicate.Digit) | CharPredicate.Digit
  }

  protected def _ipv4Address: Rule0 = rule {
    4.times(_decimalOctet).separatedBy(".")
  }

  protected def _hextet: Rule0 = rule {
    (1 to 4).times(CharPredicate.HexDigit)
  }

  protected def _ls32: Rule0 = rule { // Least significant 32-bits of IPv6 address
    2.times(_hextet).separatedBy(":") | _ipv4Address
  }

  protected def _ipv6Address: Rule0 = rule {
                                                                          (6.times(_hextet ~ ":") ~ _ls32) |
                                                                   ("::" ~ 5.times(_hextet ~ ":") ~ _ls32) |
                                               (optional(_hextet) ~ "::" ~ 4.times(_hextet ~ ":") ~ _ls32) |
    (optional(optional((1 to 2).times(_hextet).separatedBy(":"))) ~ "::" ~ 3.times(_hextet ~ ":") ~ _ls32) |
    (optional(optional((1 to 3).times(_hextet).separatedBy(":"))) ~ "::" ~ 2.times(_hextet ~ ":") ~ _ls32) |
    (optional(optional((1 to 4).times(_hextet).separatedBy(":"))) ~ "::" ~ _hextet ~ ":" ~ _ls32) |
    (optional(optional((1 to 5).times(_hextet).separatedBy(":"))) ~ "::" ~ _ls32) |
    (optional(optional((1 to 6).times(_hextet).separatedBy(":"))) ~ "::" ~ _hextet) |
    (optional(optional((1 to 7).times(_hextet).separatedBy(":"))) ~ "::")
  }

  protected def _ipvFutureAddress: Rule0 = rule {
    anyOf("vV") ~ oneOrMore(CharPredicate.HexDigit) ~ "." ~ oneOrMore(UNRESERVED ++ SUB_DELIMS ++ ":")
  }

  protected def _registeredName: Rule0 = rule {
    (REGISTERED_NAME -- '.') ~ optional((1 to 254).times(REGISTERED_NAME)) // TODO: I do not know how to implement this here: Cannot contain ".."
  }

  protected def _host: Rule1[Host] = rule {
    capture("[" ~ (_ipvFutureAddress | _ipv6Address) ~ "]") ~> extractIpLiteralHost |
    capture(_ipv4Address) ~> extractIpv4AddressHost |
    capture(_registeredName) ~> extractRegisteredNameHost
  }

  protected def _port: Rule1[Int] = rule {
    ":" ~ capture(oneOrMore(CharPredicate.Digit)) ~> extractInt
  }

  protected def _authority: Rule1[Authority] = rule {
    "//" ~ optional(_userInfo) ~ optional(_host) ~ optional(_port) ~> extractAuthority
  }

  protected def _segment: Rule1[Segment] = rule {
    capture(zeroOrMore(SEGMENT)) ~> extractSegment
  }

  /** Segment non-zero length. */
  protected def _segmentNz: Rule1[Segment] = rule {
    capture(oneOrMore(SEGMENT)) ~> extractSegment
  }

  /** Segment non-zero length, no ':'s. */
  protected def _segmentNzNc: Rule1[Segment] = rule {
    capture(oneOrMore(SEGMENT -- ':')) ~> extractSegment
  }

  /** A sequence of segments that MUST start with a slash. (Must follow an authority.) */
  protected def _pathAbEmpty: Rule1[AbsolutePath] = rule {
    "/" ~ _segment ~ zeroOrMore("/" ~ _segment) ~> extractAbsolutePath
  }

  /** A sequence of segments that MUST start with a '/' but not "//" otherwise it would be confused for an authority. */
  protected def _pathAbsolute: Rule1[AbsolutePath] = rule {
    "/" ~ _segmentNz ~ zeroOrMore("/" ~ _segment) ~> extractAbsolutePath |
    "/" ~ capture("") ~> extractEmptyAbsolutePath
  }

  /**  A sequence of segments that MUST NOT start with a slash, and the first segment MUST NOT contain any ':' otherwise to would be confused for a scheme. */
  protected def _pathNoScheme: Rule1[RootlessPath] = rule {
    _segmentNzNc ~ zeroOrMore("/" ~ _segment) ~> extractRootlessPath
  }

  /**  A sequence of segments that MUST NOT start with a slash. */
  protected def _pathRootless: Rule1[RootlessPath] = rule {
    _segmentNz ~ zeroOrMore("/" ~ _segment) ~> extractRootlessPath
  }

  // `_pathEmpty` is supported by making a path optional.

  protected def _queryParam: Rule1[Parameter] = rule {
    capture(oneOrMore(QUERY_PARAMETER_KEY)) ~ optional("=" ~ capture(zeroOrMore(QUERY_PARAMETER_VALUE))) ~> extractParam
  }

  // TODO: Implemented empty query parameter removal, consistent with matrix parameters. Updated GithubIssueTests #65 examples 1, 4, 5, and 6.
  @deprecated("Being made `protected`.", "1.0.0")
  def _query: Rule1[Query] = rule {
    "?" ~ zeroOrMore("&") ~ zeroOrMore(_queryParam).separatedBy(oneOrMore("&")) ~ zeroOrMore("&") ~> extractQuery
  }

  protected def _fragment: Rule1[Fragment] = rule {
    "#" ~ capture(zeroOrMore(FRAGMENT)) ~> extractFragment
  }

  // - - - - - - - - - -

  protected def _absoluteUri: Rule1[Uri] = rule {
    _scheme ~ _authority ~ optional(_pathAbEmpty) ~ optional(_query) ~> extractAbsoluteUri
  }

  protected def _schemeWithAuthorityAndFragmentUri: Rule1[Uri] = rule {
    _scheme ~ _authority ~ optional(_pathAbEmpty) ~ optional(_query) ~ _fragment ~> extractSchemeWithAuthorityAndFragmentUri
  }

  protected def _schemeWithAbsolutePathUri: Rule1[Uri] = rule {
    _scheme ~ _pathAbsolute ~ optional(_query) ~ optional(_fragment) ~> extractSchemeWithAbsolutePathUri
  }

  protected def _schemeWithRootlessPathUri: Rule1[Uri] = rule {
    _scheme ~ _pathRootless ~ optional(_query) ~ optional(_fragment) ~> extractSchemeWithRootlessPathUri
  }

  protected def _schemeWithQueryUri: Rule1[Uri] = rule {
    _scheme ~ _query ~ optional(_fragment) ~> extractSchemeWithQueryUri
  }

  protected def _schemeWithFragmentUri: Rule1[Uri] = rule {
    _scheme ~ _fragment ~> extractSchemeWithFragmentUri
  }

  protected def _schemeUri: Rule1[Uri] = rule {
    _scheme ~> extractSchemeUri
  }

  protected def _authorityRelativeReference: Rule1[Uri] = rule {
    _authority ~ optional(_pathAbEmpty) ~ optional(_query) ~ optional(_fragment) ~> extractAuthorityRelativeReference
  }

  protected def _absolutePathRelativeReference: Rule1[Uri] = rule {
    _pathAbsolute ~ optional(_query) ~ optional(_fragment) ~> extractAbsolutePathRelativeReference
  }

  protected def _rootlessPathRelativeReference: Rule1[Uri] = rule {
    _pathNoScheme ~ optional(_query) ~ optional(_fragment) ~> extractRootlessPathRelativeReference
  }

  protected def _queryRelativeReference: Rule1[Uri] = rule {
    _query ~ optional(_fragment) ~> extractQueryRelativeReference
  }

  protected def _fragmentRelativeReference: Rule1[Uri] = rule {
    _fragment ~> extractFragmentRelativeReference
  }

  protected def _emptyRelativeReference: Rule1[Uri] = rule {
    capture("") ~> extractEmptyRelativeReference
  }

  def uri: Rule1[Uri] = rule {
    (_schemeWithAuthorityAndFragmentUri | _absoluteUri | _schemeWithAbsolutePathUri | _schemeWithRootlessPathUri | _schemeWithQueryUri | _schemeWithFragmentUri | _schemeUri |
      _authorityRelativeReference | _absolutePathRelativeReference | _rootlessPathRelativeReference | _queryRelativeReference | _fragmentRelativeReference | _emptyRelativeReference) ~ EOI
  }
}

object UriParser {

  // Define CharPredicates as specified in http://tools.ietf.org/html/rfc3986
  val GEN_DELIMS = ":/?#[]@"
  val SUB_DELIMS = "!$&'()*+,;="
  val RESERVED = GEN_DELIMS ++ SUB_DELIMS

  val UNRESERVED = CharPredicate.AlphaNum ++ "-._~"
  val PERCENT_ENCODED = CharPredicate.HexDigit ++ '%'
  val PCHAR = UNRESERVED ++ PERCENT_ENCODED ++ SUB_DELIMS ++ ":@"

  val USER_INFO = UNRESERVED ++ PERCENT_ENCODED ++ SUB_DELIMS ++ ':'
  val USER = USER_INFO -- ':'
  val REGISTERED_NAME = UNRESERVED ++ PERCENT_ENCODED ++ SUB_DELIMS
  val SEGMENT = PCHAR
  val QUERY = PCHAR ++ "/?"
  val QUERY_PARAMETER_KEY = QUERY -- "&="
  val QUERY_PARAMETER_VALUE = QUERY -- "&"
  val FRAGMENT = PCHAR ++ "/?"

  private def parser(string: String, c: UriConfig) = {
      c.delimiterParsing match {
        case false =>
          c.matrixParams match {
            case false => new UriParser(string, c)
            case true =>  new UriParser(string, c) with MatrixParamSupport
          }
        case true =>
          c.matrixParams match {
            case false => new UriParser(string, c) with DelimiterParsing
            case true =>  new UriParser(string, c) with MatrixParamSupportWithDelimiterParsing
          }
      }
  }

  private def getValueOrThrowException[T](parserTry: scala.util.Try[T], string: String) = {
    parserTry match {
      case Success(uri) =>
        uri
      case Failure(pe@ParseError(position, _, formatTraces)) =>
        throw new java.net.URISyntaxException(string, "Invalid URI could not be parsed. " + formatTraces, position.index)
      case Failure(e) =>
        throw e
    }
  }

  @deprecated("Use `parseUri` instead.", "1.0.0")
  def parse(s: String, c: UriConfig) = parseUri(s, c)

  // TODO: Default parsing is now RFC3986 compliant. Delimiter parsing can be enabled through `c`.
  def parseUri(string: String, c: UriConfig): Uri =
    getValueOrThrowException(parser(string, c).uri.run(), string)

  /**
   * NOTE: This was not working properly:
   *   It does not provide `MatrixParamSupport`.
   *   For some reason it does not require the starting '?'.
   *   It does not end with `EOI`, so it parses until the string does not match and then skips the rest.
   */
  @deprecated("Use `uri` instead, ensuring the starting '?' and noting the `Uri` return type.", "1.0.0")
  def parseQuery(s: String, c: UriConfig) = {
    val withQuestionMark = if (s.head == '?') s else "?" + s
    val parser = new UriParser(withQuestionMark, c)
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
