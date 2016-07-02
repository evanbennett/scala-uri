package com.netaporter.uri.parsing

import com.netaporter.uri._
import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.Parameters._
import org.parboiled2._

class DefaultUriParser(input: ParserInput, c: UriConfig) extends UriParser(input, c) {

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def _scheme: Rule1[Scheme] = rule {
    capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum | anyOf("+-."))) ~ ":" ~> extractScheme
  }

  def _userInfo: Rule1[UserInfo] = rule {
    capture(oneOrMore(!anyOf("@:/?#") ~ ANY)) ~ optional(":" ~ capture(zeroOrMore(!anyOf("@:/?#") ~ ANY))) ~ "@" ~> extractUserInfo
  }

  def _host: Rule1[String] = rule {
    capture(oneOrMore(!anyOf(":/?#") ~ ANY))
  }

  def _port: Rule1[Int] = rule {
    ":" ~ capture(oneOrMore(CharPredicate.Digit)) ~> extractInt
  }

  def _authority: Rule1[Authority] = rule {
    "//" ~ optional(_userInfo) ~ _host ~ optional(_port) ~> extractAuthority
  }

  def _segment: Rule1[Segment] = rule {
    capture(zeroOrMore(!anyOf("/?#") ~ ANY)) ~> extractSegment
  }

  def _segmentNz: Rule1[Segment] = rule {
    capture(oneOrMore(!anyOf("/?#") ~ ANY)) ~> extractSegment
  }

  def _segmentNzNc: Rule1[Segment] = rule {
    capture(oneOrMore(!anyOf(":/?#") ~ ANY)) ~> extractSegment
  }

  /** A sequence of segments that MUST start with a slash. (Must follow an authority.) */
  def _pathAbEmpty: Rule1[AbsolutePath] = rule {
    "/" ~ _segment ~ zeroOrMore("/" ~ _segment) ~> extractAbsolutePath
  }

  /** A sequence of segments that MUST start with a '/' but not "//" otherwise it would be confused for an authority. */
  def _pathAbsolute: Rule1[AbsolutePath] = rule {
    "/" ~ _segmentNz ~ zeroOrMore("/" ~ _segment) ~> extractAbsolutePath | "/" ~ capture("") ~> extractEmptyAbsolutePath
  }

  /**  A sequence of segments that MUST NOT start with a slash, and the first segment MUST NOT contain any ':' otherwise to would be confused for a scheme. */
  def _pathNoScheme: Rule1[RootlessPath] = rule {
    _segmentNzNc ~ zeroOrMore("/" ~ _segment) ~> extractRootlessPath
  }

  /**  A sequence of segments that MUST NOT start with a slash. */
  def _pathRootless: Rule1[RootlessPath] = rule {
    _segmentNz ~ zeroOrMore("/" ~ _segment) ~> extractRootlessPath
  }

  // `_pathEmpty` is supported by making a path optional.

  def _queryParam: Rule1[Parameter] = rule {
    capture(oneOrMore(!anyOf("=&#") ~ ANY)) ~ optional("=" ~ capture(zeroOrMore(!anyOf("&#") ~ ANY))) ~> extractParam
  }

  // TODO: Implemented empty query parameter removal, consistent with matrix parameters. Updated GithubIssueTests #65 examples 1, 4, 5, and 6.
  def _query: Rule1[Query] = rule {
    "?" ~ zeroOrMore("&") ~ zeroOrMore(_queryParam).separatedBy(oneOrMore("&")) ~ zeroOrMore("&") ~> extractQuery
  }

  def _fragment: Rule1[Fragment] = rule {
    "#" ~ capture(zeroOrMore(!anyOf("#") ~ ANY)) ~> extractFragment
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def _absoluteUri: Rule1[Uri] = rule {
    _scheme ~ _authority ~ optional(_pathAbEmpty) ~ optional(_query) ~> extractAbsoluteUri
  }

  def _schemeWithAuthorityAndFragmentUri: Rule1[Uri] = rule {
    _scheme ~ _authority ~ optional(_pathAbEmpty) ~ optional(_query) ~ _fragment ~> extractSchemeWithAuthorityAndFragmentUri
  }

  def _schemeWithAbsolutePathUri: Rule1[Uri] = rule {
    _scheme ~ _pathAbsolute ~ optional(_query) ~ optional(_fragment) ~> extractSchemeWithAbsolutePathUri
  }

  def _schemeWithRootlessPathUri: Rule1[Uri] = rule {
    _scheme ~ _pathRootless ~ optional(_query) ~ optional(_fragment) ~> extractSchemeWithRootlessPathUri
  }

  def _schemeWithQueryUri: Rule1[Uri] = rule {
    _scheme ~ _query ~ optional(_fragment) ~> extractSchemeWithQueryUri
  }

  def _schemeWithFragmentUri: Rule1[Uri] = rule {
    _scheme ~ _fragment ~> extractSchemeWithFragmentUri
  }

  def _schemeUri: Rule1[Uri] = rule {
    _scheme ~> extractSchemeUri
  }

  def _authorityRelativeReference: Rule1[Uri] = rule {
    _authority ~ optional(_pathAbEmpty) ~ optional(_query) ~ optional(_fragment) ~> extractAuthorityRelativeReference
  }

  def _absolutePathRelativeReference: Rule1[Uri] = rule {
    _pathAbsolute ~ optional(_query) ~ optional(_fragment) ~> extractAbsolutePathRelativeReference
  }

  def _relativePathRelativeReference: Rule1[Uri] = rule {
    _pathNoScheme ~ optional(_query) ~ optional(_fragment) ~> extractRelativePathRelativeReference
  }

  def _queryRelativeReference: Rule1[Uri] = rule {
    _query ~ optional(_fragment) ~> extractQueryRelativeReference
  }

  def _fragmentRelativeReference: Rule1[Uri] = rule {
    _fragment ~> extractFragmentRelativeReference
  }

  def _emptyRelativeReference: Rule1[Uri] = rule {
    capture("") ~> extractEmptyRelativeReference
  }

  def _uri: Rule1[Uri] = rule {
    (_schemeWithAuthorityAndFragmentUri | _absoluteUri | _schemeWithAbsolutePathUri | _schemeWithRootlessPathUri | _schemeWithQueryUri | _schemeWithFragmentUri | _schemeUri |
      _authorityRelativeReference | _absolutePathRelativeReference | _relativePathRelativeReference | _queryRelativeReference | _fragmentRelativeReference | _emptyRelativeReference) ~ EOI
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  val extractAbsoluteUri = (scheme: Scheme, authority: Authority, path: Option[AbsolutePath], query: Option[Query]) =>
    AbsoluteUri(scheme, authority, path, query)

  val extractSchemeWithAuthorityAndFragmentUri = (scheme: Scheme, authority: Authority, path: Option[AbsolutePath], query: Option[Query], fragment: Fragment) =>
    SchemeWithAuthorityAndFragmentUri(scheme, authority, path, query, fragment)

  val extractSchemeWithAbsolutePathUri = (scheme: Scheme, path: AbsolutePath, query: Option[Query], fragment: Option[Fragment]) =>
    SchemeWithAbsolutePathUri(scheme, path, query, fragment)

  val extractSchemeWithRootlessPathUri = (scheme: Scheme, path: RootlessPath, query: Option[Query], fragment: Option[Fragment]) =>
    SchemeWithRootlessPathUri(scheme, path, query, fragment)

  val extractSchemeWithQueryUri = (scheme: Scheme, query: Query, fragment: Option[Fragment]) =>
    SchemeWithQueryUri(scheme, query, fragment)

  val extractSchemeWithFragmentUri = (scheme: Scheme, fragment: Fragment) =>
    SchemeWithFragmentUri(scheme, fragment)

  val extractSchemeUri = (scheme: Scheme) =>
    SchemeUri(scheme)

  val extractAuthorityRelativeReference = (authority: Authority, path: Option[AbsolutePath], query: Option[Query], fragment: Option[Fragment]) =>
    AuthorityRelativeReference(authority, path, query, fragment)

  val extractAbsolutePathRelativeReference = (path: AbsolutePath, query: Option[Query], fragment: Option[Fragment]) =>
    AbsolutePathRelativeReference(path, query, fragment)

  val extractRelativePathRelativeReference = (path: RootlessPath, query: Option[Query], fragment: Option[Fragment]) =>
    RelativePathRelativeReference(path, query, fragment)

  val extractQueryRelativeReference = (query: Query, fragment: Option[Fragment]) =>
    QueryRelativeReference(query, fragment)

  val extractFragmentRelativeReference = (fragment: Fragment) =>
    FragmentRelativeReference(fragment)

  val extractEmptyRelativeReference = (emptyString: String) =>
    EmptyRelativeReference
}
