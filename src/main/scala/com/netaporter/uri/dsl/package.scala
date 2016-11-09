package com.netaporter.uri

import scala.language.implicitConversions
import com.netaporter.uri.encoding.{ ChainedUriEncoder, UriEncoder }

/**
 * THEON:
 *  * I am not sure of the problem that is being solved by the DSL.
 *    Due to this, I am not sure that the new functionality is correct/useful/complete.
 *    It used to:
 *     * implicitly convert from `String` to `Uri` which is simply done now by explicitly calling the `Uri(String)` constructor, which removes an import and implicit call and (to my thinking) makes the code easier to read
 *     * implicitly convert from `Uri` to `String` which has always been simply done be explicitly calling `toString`, which removes an import and implicit call and (to my thinking) makes the code easier to read
 *     * allow the addition of a string segment, query parameter and/or fragment, but provided no order, so you could potentially:
 *       "http://host#existingFragment" `#` "newfragment" / "newPathSegment1" ? ("key1" -> "value1") ? ("key2" -> None) / "newPathSegment2"
 *     * essentially provide a way to parse a `String` and then call methods on the resulting `Uri` in a short form
 *  * Converting the current README.md DSL exmaples to: the new code without DSL (import and implicits); the new DSL:
 *     * val uri = "http://theon.github.com" / "scala-uri"
 *       val uri = Uri("http://theon.github.com").appendSegment("scala-uri")
 *       val uri = ("http" `://` "theon.github.com") / "scala-uri"
 *     * val uri = "http://theon.github.com/scala-uri" ? ("p1" -> "one") & ("p2" -> 2) & ("p3" -> true)
 *       val uri = Uri("http://theon.github.com/scala-uri").queryAppend(Seq(("p1" -> "one"), ("p2" -> 2), ("p3" -> true)))
 *       val uri = ("http" `://` "theon.github.com") / "scala-uri" ? ("p1" `=` "one") & ("p2" `=` 2) & ("p3" `=` true)
 *     * val uri2 = "http://theon.github.com/scala-uri" ? ("param1" -> Some("1")) & ("param2" -> None)
 *       val uri2 = Uri("http://theon.github.com/scala-uri").queryAppend(Seq(("param1" -> Some("1")), ("param2" -> None)))
 *       val uri2 = ("http" `://` "theon.github.com") / "scala-uri" ? ("param1" `=` Some("1")) & ("param2" `=` None)
 *     * val uri = "http://theon.github.com/scala-uri" `#` "fragment"
 *       val uri = Uri("http://theon.github.com/scala-uri").withFragment("fragment")
 *       val uri = ("http" `://` "theon.github.com") / "scala-uri" `#` "fragment"
 *  * I would probably just use string interpolation and then parse, myself.
 *  * Not currently supported:
 *     * `UserPasswordUserInfo` - As this is deprecated in RFC 3986, there are no plans for support.
 *     * `RelativePathReference` - Was implemented, but then removed as it created errors:
 *    implicit def stringToPathDsl(firstSegment: String): PathDsl = PathDsl(None, None, RootlessPath(firstSegment))
 *
 * DSL Rules:
 *  * All strings must be in decoded form.
 *  * The scheme and authority must be enclosed in parentheses, OR you must not use operator notation.
 *    e.g. ("scheme" `://` "registeredName") // Note the surrounding parentheses and operator access.
 *      OR "scheme".`://`("registeredName") // Note the '.' and argument parentheses.
 *  * All `Parameter`s must be enclosed in parentheses.
 *  * When starting with a package function, the argument must be enclosed in parentheses.
 *  * When using the methods without arguments in postfix notation, the statement must be terminated.
 *    e.g. Surrounded by parentheses OR followed by a ';' or blank line.
 */
package object dsl {

  @deprecated("Not needed anymore. `+` method added to `UriEncoder`.", "1.0.0")
  implicit def encoderToChainedEncoder(enc: UriEncoder) = ChainedUriEncoder(enc :: Nil)

  @deprecated("Convert to the new DSL.", "1.0.0")
  implicit def stringToUriDsl(s: String)(implicit c: UriConfig = UriConfig.default) = new UriDsl(stringToUri(s)(c))
  @deprecated("Convert to the new DSL.", "1.0.0")
  implicit def uriToUriDsl(uri: Uri) = new UriDsl(uri)
  @deprecated("Convert to the new DSL.", "1.0.0")
  implicit def queryParamToUriDsl(kv: (String, Any))(implicit c: UriConfig = UriConfig.default) = new UriDsl(EmptyReference.queryAppend(kv._1, kv._2))

  @deprecated("Use `Uri.apply` instead.", "1.0.0")
  implicit def stringToUri(s: String)(implicit c: UriConfig = UriConfig.default) = Uri.parse(s)(c)
  @deprecated("Use `toString` instead.", "1.0.0")
  implicit def uriToString(uri: Uri)(implicit c: UriConfig = UriConfig.default): String = uri.toString(c)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // THEON: For completeness, the following could mix and match empties. (e.g. `///?` and `//#`, etc.)

  def `//`: AuthorityDsl = AuthorityDsl(None, EmptyAuthority)

  def `//`(authority: Authority): AuthorityDsl = AuthorityDsl(None, authority)

  def / : PathDsl = PathDsl(None, None, AbsolutePath(EmptySegment))

  def /(firstSegment: Segment): PathDsl = PathDsl(None, None, AbsolutePath(firstSegment))

  def ? : QueryDsl = QueryDsl(None, None, None, EmptyQuery)

  def ?(firstQueryParameter: Parameter): QueryDsl = QueryDsl(None, None, None, Query(firstQueryParameter))

  def ?(queryString: String): QueryDsl = QueryDsl(None, None, None, Query(queryString))

  def `#`: FragmentDsl = FragmentDsl(None, None, None, None, Some(EmptyFragment))

  def `#`(fragment: String): FragmentDsl = FragmentDsl(None, None, None, None, Fragment.option(fragment))

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  implicit def stringToSchemeDsl(scheme: String): SchemeDsl = SchemeDsl(Scheme.option(scheme))

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

  implicit class StringToSomething(string: String) {

    def `@`(host: String): Authority = Authority(StringUserInfo.option(string), Host.parse(host), None)

    def `;`(firstMatrixParameter: Parameter): MatrixParametersSegment = StringSegment(string).append(firstMatrixParameter)

    def `;`(firstMatrixKey: String): MatrixParametersSegment = StringSegment(string).append(Parameter(firstMatrixKey))

    def ?(firstQueryParameter: Parameter): SegmentAndQuery = SegmentAndQuery(StringSegment(string), Query(firstQueryParameter))

    def ?(queryString: String): SegmentAndQuery = SegmentAndQuery(StringSegment(string), Query(queryString))

    def ?#(fragment: String): SegmentAndQueryAndFragment = SegmentAndQueryAndFragment(StringSegment(string), EmptyQuery, Fragment(fragment))

    def `#`(fragment: String): SegmentAndFragment = SegmentAndFragment(StringSegment(string), Fragment(fragment))
  }

  implicit class IntToSomething(port: Int) {

//    def `:`(host: String)(implicit config: UriConfig): Authority = Authority(None, Host.parse(host), Option(port))

    def `:`(authority: Authority): Authority = authority.copy(port = Option(port))
  }

  implicit def stringToStringSegment(segment: String): StringSegment = StringSegment(segment)

  implicit class MatrixParametersSegmentToSomething(segment: MatrixParametersSegment) {

    def `;`(nextMatrixParameter: Parameter): MatrixParametersSegment = segment.append(nextMatrixParameter)

    def `;`(nextMatrixKey: String): MatrixParametersSegment = segment.append(Parameter(nextMatrixKey))
  }

  implicit class SegmentToSomething(segment: Segment) {

    def ?(firstQueryParameter: Parameter): SegmentAndQuery = SegmentAndQuery(segment, Query(firstQueryParameter))

    def ?(queryString: String): SegmentAndQuery = SegmentAndQuery(segment, Query(queryString))

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

  implicit def schemeDslToUri(schemeDsl: SchemeDsl)(implicit config: UriConfig): Uri = schemeDsl.toUri

  implicit def authorityDslToUri(authorityDsl: AuthorityDsl)(implicit config: UriConfig): Uri = authorityDsl.toUri

  implicit def pathDslToUri(pathDsl: PathDsl)(implicit config: UriConfig): Uri = pathDsl.toUri

  implicit def queryDslToUri(queryDsl: QueryDsl)(implicit config: UriConfig): Uri = queryDsl.toUri

  implicit def fragmentDslToUri(fragmentDsl: FragmentDsl)(implicit config: UriConfig): Uri = fragmentDsl.toUri
}
