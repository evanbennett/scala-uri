package com.netaporter.uri

import scala.language.implicitConversions
import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.encoding.{ChainedUriEncoder, UriEncoder}

/**
 * TODO:
 * Not currently supported:
 *  * `password` field - As this is deprecated by the RFC, there are no plans for support.
 *  * `RootlessPathRelativeReference` - Was implemented, but then removed as it created errors:
 *    implicit def stringToPathDsl(firstSegment: String): PathDsl = PathDsl(None, None, RootlessPath(firstSegment))
 *
 * DSL Rules:
 *  * The scheme and authority must be enclosed in parentheses, OR you cannot use operator notation.
 *    e.g. ("scheme" `://` "registeredName") // Note the surrounding parentheses and operator access.
 *      OR "scheme".`://` ("registeredName") // Note the '.' and argument parentheses.
 *  * All `Parameter`s must be enclosed in parentheses.
 *  * When starting with an package function, the argument must be enclosed in parentheses.
 *  * When using the methods without arguments in postfix notation, the statement must be terminated.
 *    e.g. Surrounded by parentheses OR followed by a ';' or blank line.
 */
package object dsl {

  @deprecated("Not needed anymore. `+` method added to `UriEncoder`.", "1.0.0")
  implicit def encoderToChainedEncoder(enc: UriEncoder) = ChainedUriEncoder(enc :: Nil)

  @deprecated("Convert to the new DSL.", "1.0.0")
  implicit def uriToUriDsl(uri: Uri) = new UriDsl(uri)

  @deprecated("Convert to the new DSL.", "1.0.0")
  implicit def stringToUri(s: String)(implicit c: UriConfig = UriConfig.DEFAULT) = Uri.parse(s)
  @deprecated("Convert to the new DSL.", "1.0.0")
  implicit def stringToUriDsl(s: String)(implicit c: UriConfig = UriConfig.DEFAULT) = new UriDsl(stringToUri(s))

  @deprecated("Convert to the new DSL.", "1.0.0")
  implicit def queryParamToUriDsl(keyValue: (String, Any))(implicit c: UriConfig = UriConfig.DEFAULT) = new UriDsl(EmptyRelativeReference.queryAppend(keyValue._1, keyValue._2))

  @deprecated("Convert to the new DSL.", "1.0.0")
  implicit def uriToString(uri: Uri)(implicit c: UriConfig = UriConfig.DEFAULT): String = uri.toString

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  sealed case class ParameterAndFragment(parameter: Parameter, fragment: Fragment)

  sealed case class SegmentAndQuery(segment: Segment, query: Query)

  sealed case class SegmentAndQueryAndFragment(segment: Segment, query: Query, fragment: Fragment)

  sealed case class SegmentAndFragment(segment: Segment, fragment: Fragment)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  implicit class StringToParameter(key: String) {

    def `=`(value: Any): Parameter = Parameter(key, value)
  }

  implicit def stringToAuthority(host: String): Authority = Authority(None, Host.parse(host), None)

  implicit def stringToStringSegment(segment: String): StringSegment = StringSegment(segment)

  implicit class StringToSomething(string: String) {

    def `@`(host: String): Authority = Authority(UserInfo.option(string), Host.parse(host), None)

    def `;`(firstMatrixParameter: Parameter): MatrixParametersSegment = StringSegment(string).append(firstMatrixParameter)

    def `;`(firstMatrixKey: String): MatrixParametersSegment = StringSegment(string).append(Parameter(firstMatrixKey))

    def ?(firstQueryParameter: Parameter): SegmentAndQuery = SegmentAndQuery(StringSegment(string), Query(firstQueryParameter))

    def ?(firstQueryKey: String): SegmentAndQuery = SegmentAndQuery(StringSegment(string), EmptyQuery.append(firstQueryKey))

    def ?#(fragment: String): SegmentAndQueryAndFragment = SegmentAndQueryAndFragment(StringSegment(string), EmptyQuery, Fragment(fragment))

    def `#`(fragment: String): SegmentAndFragment = SegmentAndFragment(StringSegment(string), Fragment(fragment))
  }

  implicit class IntToAuthority(port: Int) {

    def `:`(authority: Authority): Authority = authority.copy(port = Option(port))
  }

  implicit class MatrixParametersSegmentToSomething(segment: MatrixParametersSegment) {

    def `;`(nextMatrixParameter: Parameter): MatrixParametersSegment = segment.append(nextMatrixParameter)

    def `;`(nextMatrixKey: String): MatrixParametersSegment = segment.append(Parameter(nextMatrixKey))
  }

  implicit class SegmentToSomething(segment: Segment) {

    def ?(firstQueryParameter: Parameter): SegmentAndQuery = SegmentAndQuery(segment, Query(firstQueryParameter))

    def ?(firstQueryKey: String): SegmentAndQuery = SegmentAndQuery(segment, EmptyQuery.append(firstQueryKey))

    def ?#(fragment: String): SegmentAndQueryAndFragment = SegmentAndQueryAndFragment(segment, EmptyQuery, Fragment(fragment))

    def `#`(fragment: String): SegmentAndFragment = SegmentAndFragment(segment, Fragment(fragment))
  }

  implicit class SegmentAndQueryToSomething(segmentAndQuery: SegmentAndQuery) {

    def `#`(fragment: String): SegmentAndQueryAndFragment = SegmentAndQueryAndFragment(segmentAndQuery.segment, segmentAndQuery.query, Fragment(fragment))
  }

  implicit class ParameterToSomething(parameter: Parameter) {

    def `#`(fragment: String): ParameterAndFragment = ParameterAndFragment(parameter, Fragment(fragment))
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  implicit def stringToSchemeDsl(scheme: String): SchemeDsl = SchemeDsl(Scheme.option(scheme))

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  implicit def schemeDslToUri(schemeDsl: SchemeDsl): Uri = schemeDsl.toUri

  implicit def authorityDslToUri(authorityDsl: AuthorityDsl): Uri = authorityDsl.toUri

  implicit def pathDslToUri(pathDsl: PathDsl): Uri = pathDsl.toUri

  implicit def queryDslToUri(queryDsl: QueryDsl): Uri = queryDsl.toUri

  implicit def fragmentDslToUri(fragmentDsl: FragmentDsl): Uri = fragmentDsl.toUri

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // TODO: For completeness, the following could mix and match empties. (e.g. `///?` and `//#`, etc.)

  def `//`: AuthorityDsl = AuthorityDsl(None, EmptyAuthority)

  def `//`(authority: Authority): AuthorityDsl = AuthorityDsl(None, authority)

  def / : PathDsl = PathDsl(None, None, AbsolutePath(EmptySegment))

  def /(firstSegment: Segment): PathDsl = PathDsl(None, None, AbsolutePath(firstSegment))

  def ? : QueryDsl = QueryDsl(None, None, None, EmptyQuery)

  def ?(firstQueryParameter: Parameter): QueryDsl = QueryDsl(None, None, None, Query(firstQueryParameter))

  def ?(firstQueryKey: String): QueryDsl = QueryDsl(None, None, None, EmptyQuery.append(firstQueryKey))

  def `#`: FragmentDsl = FragmentDsl(None, None, None, None, Some(EmptyFragment))

  def `#`(fragment: String): FragmentDsl = FragmentDsl(None, None, None, None, Fragment.option(fragment))
}
