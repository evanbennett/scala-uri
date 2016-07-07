package com.netaporter.uri

import scala.collection.GenTraversableOnce
import com.netaporter.uri.inet.PublicSuffixes
import com.netaporter.uri.parsing.UriParser
import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.Parameters._

/**
 * http://tools.ietf.org/html/rfc3986
 */
sealed abstract class Uri(val scheme: Option[Scheme], val authority: Option[Authority], val path: Option[Path], val query: Option[Query], val fragment: Option[Fragment]) {

  @deprecated("Use `scheme: Option[Scheme]` instead.", "1.0.0")
  def protocol: Option[String] = scheme.map(_.scheme)

  def userInfo: Option[UserInfo] = authority.flatMap(_.userInfo)

  def user: Option[String] = authority.flatMap(_.user)

  def password: Option[String] = authority.flatMap(_.password)

  def host: Option[String] = authority.flatMap(_.hostString)

  def hostParts: Seq[String] =
    host.map(h => h.split('.').toVector).getOrElse(Vector.empty)

  def subdomain: Option[String] = hostParts.headOption

  def publicSuffix: Option[String] =
    host.flatMap(host => PublicSuffixes.trie.longestMatch(host.reverse)).map(_.reverse)

  def publicSuffixes: Seq[String] =
    host.fold(Seq.empty[String])(host => PublicSuffixes.trie.matches(host.reverse)).map(_.reverse)

  def port: Option[Int] = authority.flatMap(_.port)

  @deprecated("Use `pathSegments` instead.", "1.0.0")
  def pathParts: Seq[PathPart] = pathSegments

  def pathSegments: Seq[Segment] = path.fold(Seq.empty[Segment])(_.segments)

  @deprecated("Use `pathSegmentOption` instead.", "1.0.0")
  def pathPartOption(name: String): Option[PathPart] = pathSegmentOption(name)

  def pathSegmentOption(existingSegment: String): Option[Segment] =
    path.map(_.segments).flatMap(_.find(_.segment == existingSegment))

  @deprecated("Use `pathSegment` instead.", "1.0.0")
  def pathPart(name: String): PathPart = pathSegment(name)

  def pathSegment(existingSegment: String): Segment =
    pathSegmentOption(existingSegment).head

  def matrixParameters(existingSegment: String): Seq[Parameter] =
    path.fold(Seq.empty[Parameter])(_.matrixParameters(existingSegment))

  @deprecated("Use `matrixParametersOfLastSegment` instead.", "1.0.0")
  def matrixParams: ParamSeq =
    path.fold(Seq.empty[Param])(path => path.segments.last match {
      case segment: MatrixParametersSegment => segment.params
      case _ => Seq.empty
    })

  def matrixParametersOfLastSegment: Seq[Parameter] =
    path.fold(Seq.empty[Parameter])(_.matrixParametersOfLastSegment)

  @deprecated("Use `query: Option[Query]` instead.", "1.0.0")
  def queryValue: QueryString = query.getOrElse(EmptyQueryString)

  def queryParameters: Seq[Parameter] =
    query.fold(Seq.empty[Parameter])(_.parameters)

  def queryValues(existingKey: String): Seq[Option[String]] =
    query.fold(Seq.empty[Option[String]])(_.values(existingKey))

  def queryValueFirst(existingKey: String): Option[String] =
    query.flatMap(_.valueFirst(existingKey))

  def queryMap: Map[String, Seq[String]] =
    query.fold(Map.empty[String, Seq[String]])(_.toMap)

  @deprecated("Use `fragment: Option[Scheme]` instead.", "1.0.0")
  def fragmentString: Option[String] = fragment.map(_.fragment)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def withScheme(uri: Uri): Uri = copy(scheme = uri.scheme)

  def withScheme(newScheme: Scheme = null): Uri = copy(scheme = Option(newScheme))

  /**
   * Copies this Uri but with the scheme set as the given value.
   *
   * @param newScheme the new scheme to set
   * @return a new Uri with the specified scheme
   */
  def withScheme(newScheme: String): Uri = withScheme(Scheme(newScheme))

  def withAuthority(uri: Uri): Uri = copy(authority = uri.authority)

  def withAuthority(newAuthority: Authority = null): Uri = copy(authority = Option(newAuthority))

  def withUserInfo(uri: Uri): Uri = withUserInfo(uri.userInfo.orNull)

  def withUserInfo(authority: Authority): Uri = withUserInfo(authority.userInfo.orNull)

  def withUserInfo(newUserInfo: UserInfo = null): Uri = {
    authority match {
      case None =>
        if (newUserInfo != null) throw new IllegalArgumentException("Cannot have a `userInfo` without a `host`.")
        this
      case Some(authority) =>
        withAuthority(authority.copy(userInfo = Option(newUserInfo)))
    }
  }

  def withUser(uri: Uri): Uri = withUser(uri.user.orNull)

  def withUser(authority: Authority): Uri = withUser(authority.user.orNull)

  def withUser(userInfo: UserInfo): Uri = withUser(userInfo.user)

  /**
   * Copies this Uri but with the user set as the given value.
   *
   * @param newUser the new user to set
   * @return a new Uri with the specified user
   */
  def withUser(newUser: String = null): Uri = {
    authority match {
      case None =>
        if (newUser != null) throw new IllegalArgumentException("Cannot have a `user` without a `host`.")
        this
      case Some(authority) =>
        withAuthority(authority.copy(userInfo = if (newUser == null) None else UserInfo.option(newUser, authority.userInfo.flatMap(_.password).orNull)))
    }
  }

  def withPassword(uri: Uri): Uri = withPassword(uri.password.orNull)

  def withPassword(authority: Authority): Uri = withPassword(authority.password.orNull)

  def withPassword(userInfo: UserInfo): Uri = withPassword(userInfo.password.orNull)

  /**
   * Copies this Uri but with the password set as the given value.
   *
   * @param newPassword the new password to set
   * @return a new Uri with the specified password
   */
  def withPassword(newPassword: String = null): Uri = {
    authority match {
      case None =>
        if (newPassword != null) throw new IllegalArgumentException("Cannot have a `password` without a `host`.")
        this
      case Some(authority) =>
        authority.userInfo match {
          case None =>
            if (newPassword != null) throw new IllegalArgumentException("Cannot have a `password` without a `host`.")
            this
          case Some(userInfo) =>
            copy(authority = Some(authority.copy(userInfo = UserInfo.option(userInfo.user, newPassword))))
        }
    }
  }

  def withHost(uri: Uri): Uri = withHost(uri.authority.flatMap(_.host))

  def withHost(authority: Authority): Uri = withHost(authority.host)

  def withHost(newHost: Option[Host]): Uri =
    copy(authority = if (authority.isEmpty) Authority.option(None, newHost, None) else authority.map(_.copy(host = newHost)))

  def withHost(newHost: Host): Uri = withHost(Option(newHost))

  /**
   * Copies this Uri but with the host set as the given value.
   *
   * @param newHost the new host to set
   * @return a new Uri with the specified host
   */
  def withHost(registeredName: String = null, ipv4Address: String = null, ipLiteralAddress: String = null): Uri =
    withHost(Host.option(registeredName, ipv4Address, ipLiteralAddress))

  def withPort(uri: Uri): Uri = withPort(uri.port.getOrElse(0))

  def withPort(authority: Authority): Uri = withPort(authority.port.getOrElse(0))

  /**
   * Copies this Uri but with the port set as the given value.
   *
   * @param newPort the new port to set
   * @return a new Uri with the specified port
   */
  def withPort(newPort: Int = 0): Uri = {
    if (authority.isEmpty && newPort == 0) this
    else if (authority.isEmpty) throw new IllegalArgumentException("Cannot have a `port` without a `host`.")
    else copy(authority = authority.map(_.copy(port = if (newPort == 0) None else Some(newPort))))
  }

  def withPath(uri: Uri): Uri = copy(path = uri.path)

  def withPath(newPath: Path = null): Uri = copy(path = Option(newPath))

  @deprecated("Use `appendMatrixParameter` instead.", "1.0.0")
  def addMatrixParam(pp: String, k: String, v: String): Uri = appendMatrixParameter(pp, k, v)

  /**
   * Append the matrix parameter key and value to any existing segment that matches `existingSegment`.
   */
  def appendMatrixParameter(existingSegment: String, key: String, value: Any = None): Uri =
    path.fold(this)(path => withPath(path.appendMatrixParameter(existingSegment, key, value)))

  /**
   * Append the parameter to any existing segment that matches `existingSegment`.
   */
  def appendMatrixParameter(existingSegment: String, parameter: Parameter): Uri =
    path.fold(this)(path => withPath(path.appendMatrixParameter(existingSegment, parameter)))

  @deprecated("Use `appendMatrixParameterToLastSegment` instead.", "1.0.0")
  def addMatrixParam(k: String, v: String): Uri = appendMatrixParameterToLastSegment(k, v)

  /**
   * If a `path` exists, append the matrix parameter key and value to the last segment.
   */
  def appendMatrixParameterToLastSegment(key: String, value: Any = None): Uri =
    path.fold(this)(path => withPath(path.appendMatrixParameterToLastSegment(key, value)))

  /**
   * If a `path` exists, append the matrix parameter to the last segment.
   */
  def appendMatrixParameterToLastSegment(parameter: Parameter): Uri =
    path.fold(this)(path => withPath(path.appendMatrixParameterToLastSegment(parameter)))

  def withQuery(uri: Uri): Uri = copy(query = uri.query)

  def withQuery(newQuery: Query = null): Uri = copy(query = Option(newQuery))

  /**
   * Replaces the existing query with a new set of query parameters
   */
  def withQuery(parameters: Seq[Parameter]): Uri = withQuery(Query(parameters))

  def withQuery(firstParameter: Parameter, otherParameters: Parameter*): Uri = withQuery(Query(firstParameter +: otherParameters))

  @deprecated("Use `queryAppend` instead.", "1.0.0")
  def addParam(name: String, value: Any): Uri = queryAppend(name, value)

  /**
   * Appends a new Query parameter key-value pair.
   *
   * TODO: Old behaviour has been removed, that was not consistent with parsing and constructing through `QueryString`:
   *   If the value for the Query parameter is None, then this Query parameter will not be rendered in calls to toString or toStringRaw.
   *
   * @param key key of the parameter
   * @param value value for the parameter
   * @return A new Uri with the new Query parameter
   */
  def queryAppend(key: String, value: Any = None): Uri =
    withQuery(query.fold(Query(Parameter(key, value)))(query => query.append(key, value)))

  @deprecated("Use `queryAppend` instead.", "1.0.0")
  def addParams(kvs: Seq[(String, Any)]): Uri = queryAppend(kvs)

  /**
   * Appends new query parameters.
   *
   * TODO: Old behaviour has been removed, that was not consistent with parsing and constructing through `QueryString`:
   *   If the value for the Query parameter is None, then this Query parameter will not be rendered in calls to toString or toStringRaw.
   */
  def queryAppend(parameters: Seq[(String, Any)]): Uri = {
    val newParameters = parameters.map {
      case (key, null) => Parameter(key, None)
      case (key, None) => Parameter(key, None)
      case (key, Some(value)) => Parameter(key, Option(value.toString))
      case (key, value) => Parameter(key, Option(value.toString))
    }
    withQuery(query.fold(Query(newParameters))(query => query.append(newParameters)))
  }

  /**
   * Appends new query parameters.
   */
  def queryAppend(parameters: Parameter*)(implicit di: DummyImplicit): Uri =
    withQuery(query.fold(Query(parameters))(query => query.append(parameters)))

  /**
   * Appends new query parameters from another query.
   */
  def queryAppend(otherQuery: Query): Uri =
    withQuery(query.fold(otherQuery)(query => query.appendParameters(otherQuery)))

  @deprecated("Use `queryMapParameters` instead.", "1.0.0")
  def mapQuery(f: Param => Param): Uri =
    query.fold(this)(query => withQuery(query.mapParams(f)))

  /**
   * Transforms the Query by applying the specified Function to each Query Parameter
   *
   * @param f A function that returns a new Parameter when applied to each Parameter
   */
  def queryMapParameters(f: Parameter => Parameter): Uri =
    query.fold(this)(query => withQuery(query.mapParameters(f)))

  @deprecated("Use `queryFlatMapParameters` instead.", "1.0.0")
  def flatMapQuery(f: Param => GenTraversableOnce[Param]): Uri =
    query.fold(this)(query => withQuery(query.flatMapParams(f)))

  /**
   * Transforms the Query by applying the specified Function to each Query Parameter
   *
   * @param f A function that returns a collection of Parameters when applied to each parameter
   */
  def queryFlatMapParameters(f: Parameter => GenTraversableOnce[Parameter]): Uri =
    query.fold(this)(query => withQuery(query.flatMapParameters(f)))

  @deprecated("Use `queryMapKeys` instead.", "1.0.0")
  def mapQueryNames(f: String => String): Uri = queryMapKeys(f)

  /**
   * Transforms the Query by applying the specified Function to each Query Parameter key
   *
   * @param f A function that returns a new Parameter key when applied to each Parameter key
   */
  def queryMapKeys(f: String => String): Uri =
    query.fold(this)(query => withQuery(query.mapKeys(f)))

  /**
   * Transforms the Query by applying the specified Function to each Query Parameter value
   *
   * NOTE: This ignores `None` values, so you CANNOT transform them.
   *
   * @param f A function that returns a new Parameter value when applied to each Parameter value
   */
  @deprecated("Use `queryMapValues(Option[String] => Option[String])` instead.", "1.0.0")
  def mapQueryValues(f: String => String): Uri =
    query.fold(this)(query => withQuery(query.mapParamValues(f)))

  /**
   * Transforms the Query by applying the specified Function to each Query Parameter value
   *
   * @param f A function that returns a new Parameter value when applied to each Parameter value
   */
  def queryMapValues(f: Option[String] => Option[String]): Uri =
    query.fold(this)(query => withQuery(query.mapValues(f)))

  @deprecated("Use `queryFilterParameters` instead.", "1.0.0")
  def filterQuery(f: Param => Boolean): Uri =
    query.fold(this)(query => withQuery(query.filterParams(f)))

  /**
   * Removes any query parameters that return false when applied to the given Function.
   */
  def queryFilterParameters(f: Parameter => Boolean): Uri =
    query.fold(this)(query => withQuery(query.filterParameters(f)))

  @deprecated("Use `queryFilterKeys` instead.", "1.0.0")
  def filterQueryNames(f: String => Boolean): Uri = queryFilterKeys(f)

  /**
   * Removes any query parameters that return false when their key is applied to the given Function.
   */
  def queryFilterKeys(f: String => Boolean): Uri =
    query.fold(this)(query => withQuery(query.filterKeys(f)))

  /**
   * Removes any query parameters that return false when their value is applied to the given Function.
   *
   * NOTE: This removes query parameters with a value of `None`.
   */
  @deprecated("Use `queryFilterValues(Option[String] => Boolean)` instead.", "1.0.0")
  def filterQueryValues(f: String => Boolean): Uri =
    query.fold(this)(query => withQuery(query.filterParamsValues(f)))

  /**
   * Removes any query parameters that return false when their value is applied to the given Function.
   */
  def queryFilterValues(f: Option[String] => Boolean): Uri =
    query.fold(this)(query => withQuery(query.filterValues(f)))

  @deprecated("Use `queryReplaceMatching` instead.", "1.0.0")
  def replaceParams(k: String, v: Any): Uri = queryReplaceMatching(k, v)

  /**
   * Removes the existing query parameters with the specified key and appends a new query parameter
   * with the specified value.
   *
   * TODO: Old behaviour has been removed, that was not consistent with parsing and constructing through `QueryString`:
   *   If the value passed in is None, then all Query parameters with the specified key are removed.
   *
   * TODO: Old behaviour has been removed from `Parameters`, that was not expected:
   *   If the `existingKey` did not exist, a new parameter is appended without any being removed.
   *
   * @param existingKey Key for the Query parameter(s) to replace
   * @param value value to replace with
   * @return A new Uri with the result of the replace
   */
  def queryReplaceMatching(existingKey: String, newValue: Any): Uri =
    query.fold(this)(query => withQuery(query.replaceMatching(existingKey, newValue)))

  @deprecated("Use `withQuery` instead.", "1.0.0")
  def replaceAllParams(params: Param*): Uri = withQuery(params)

  @deprecated("Use `queryRemoveMatching` instead.", "1.0.0")
  def removeParams(k: String): Uri = queryRemoveMatching(k)

  /**
   * Removes query parameters with the specified key.
   *
   * @param key Key for the Query parameter(s) to remove
   */
  def queryRemoveMatching(key: String): Uri =
    query.fold(this)(query => withQuery(query.removeMatching(key)))

  @deprecated("Use `queryRemoveMatching` instead.", "1.0.0")
  def removeParams(a: Seq[String]): Uri = queryRemoveMatching(a)

  /**
   * Removes query parameters with a specified key contained in `keys`.
   *
   * @param keys an Array of Keys for the Query parameter(s) to remove
   */
  def queryRemoveMatching(keys: Seq[String]): Uri =
    query.fold(this)(query => withQuery(query.removeMatching(keys)))

  @deprecated("Use `withQuery()` instead.", "1.0.0")
  def removeAllParams(): Uri = withQuery()

  def queryRemoveAllParameters(): Uri =
    query.fold(this)(query => withQuery(query.withParameters()))

  def withFragment(uri: Uri): Uri = copy(fragment = uri.fragment)

  def withFragment(newFragment: Fragment = null): Uri = copy(fragment = Option(newFragment))

  /**
   * Copies this Uri but with the fragment set as the given value.
   *
   * @param newFragment the new fragment to set
   * @return a new Uri with the specified fragment
   */
  def withFragment(newFragment: String): Uri = withFragment(Fragment(newFragment))

  @deprecated("Use `copy` instead.", "1.0.0")
  def copyOld(scheme: Option[String] = protocol,
           user: Option[String] = user,
           password: Option[String] = password,
           host: Option[String] = host,
           port: Option[Int] = port,
           pathParts: Seq[PathPart] = pathParts,
           query: QueryString = queryValue,
           fragment: Option[String] = fragmentString): Uri =
    Uri(scheme, user, password, host, port, pathParts, query, fragment)

  def copy(scheme: Option[Scheme] = scheme, authority: Option[Authority] = authority, path: Option[Path] = path, query: Option[Query] = query, fragment: Option[Fragment] = fragment): Uri =
    Uri(scheme, authority, path, query, fragment)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  override def equals(a: Any): Boolean = {
    a match {
      case null => false
      case uri: Uri => scheme == uri.scheme && authority == uri.authority && path == uri.path && query == uri.query && fragment == uri.fragment
      case _ => false
    }
  }

  override def hashCode: Int = ((((37 + scheme.hashCode) * 37 + authority.hashCode) * 37 + path.hashCode) * 37 + query.hashCode) * 37 + fragment.hashCode

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def schemeToString(implicit c: UriConfig = UriConfig.DEFAULT): String =
    scheme.map(_.toString).getOrElse("")

  def schemeToStringRaw(implicit c: UriConfig = UriConfig.DEFAULT): String =
    scheme.map(_.toStringRaw).getOrElse("")

  def authorityToString(implicit c: UriConfig = UriConfig.DEFAULT): String =
    authority.map(_.toString).getOrElse("")

  def authorityToStringRaw(implicit c: UriConfig = UriConfig.DEFAULT): String =
    authority.map(_.toStringRaw).getOrElse("")

  /**
   * Returns the encoded path. By default non ASCII characters in the path are percent encoded.
   *
   * @return String containing the path for this Uri
   */
  def pathToString(implicit c: UriConfig = UriConfig.DEFAULT): String =
    path.map(_.toString).getOrElse("")

  @deprecated("Use `pathToStringRaw` instead.", "1.0.0")
  def pathRaw(implicit c: UriConfig = UriConfig.DEFAULT): String = pathToStringRaw

  /**
   * Returns the path with no encoders taking place (e.g. non ASCII characters will not be percent encoded)
   *
   * @return String containing the raw path for this Uri
   */
  def pathToStringRaw(implicit c: UriConfig = UriConfig.DEFAULT): String =
    path.map(_.toStringRaw).getOrElse("")

  @deprecated("Use `queryToString` instead.", "1.0.0")
  def queryString(implicit c: UriConfig = UriConfig.DEFAULT): String = queryToString

  def queryToString(implicit c: UriConfig = UriConfig.DEFAULT): String =
    query.map(_.toString).getOrElse("")

  @deprecated("Use `pathToStringRaw` instead.", "1.0.0")
  def queryStringRaw(implicit c: UriConfig = UriConfig.DEFAULT): String = queryToStringRaw

  def queryToStringRaw(implicit c: UriConfig = UriConfig.DEFAULT): String =
    query.map(_.toStringRaw).getOrElse("")

  def fragmentToString(implicit c: UriConfig = UriConfig.DEFAULT): String =
    fragment.map(_.toString).getOrElse("")

  def fragmentToStringRaw(implicit c: UriConfig = UriConfig.DEFAULT): String =
    fragment.map(_.toStringRaw).getOrElse("")

  override def toString: String = toString(UriConfig.DEFAULT)

  def toString(implicit c: UriConfig = UriConfig.DEFAULT): String =
    schemeToString + authorityToString + pathToString + queryToString + fragmentToString

  /**
   * Returns the string representation of this Uri with no encoders taking place
   * (e.g. non ASCII characters will not be percent encoded)
   *
   * @return String containing this Uri in it's raw form
   */
  def toStringRaw(implicit c: UriConfig = UriConfig.DEFAULT): String =
    schemeToStringRaw + authorityToStringRaw + pathToStringRaw + queryToStringRaw + fragmentToStringRaw

  /**
   * Converts to a Java URI.
   * This involves a toString + URI.parse because the specific URI constructors do not deal properly with encoded elements
   * TODO: All of the `java.net.URI` constructors build a `String` from the arguments and then parse it anyway.
   *
   * @return a URI matching this Uri
   */
  def toURI(implicit c: UriConfig = UriConfig.CONSERVATIVE): java.net.URI = new java.net.URI(toString)
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sealed trait BaseUri {

  // TODO: Implement RFC section 5 Reference Resolution???
//  def resolve(relativeReference: RelativeReference): Uri = ???
}

// TODO: RFC section 4.3: An absolute URI cannot have a `Fragment`:
case class AbsoluteUri(private val _scheme: Scheme, private val _authority: Authority, override val path: Option[AbsolutePath], override val query: Option[Query])
  extends Uri(Some(_scheme), Some(_authority), path, query, None) with BaseUri

case class SchemeWithAuthorityAndFragmentUri(private val _scheme: Scheme, private val _authority: Authority, override val path: Option[AbsolutePath], override val query: Option[Query], private val _fragment: Fragment)
  extends Uri(Some(_scheme), Some(_authority), path, query, Some(_fragment)) with BaseUri

case class SchemeWithAbsolutePathUri(private val _scheme: Scheme, private val _path: AbsolutePath, override val query: Option[Query], override val fragment: Option[Fragment])
  extends Uri(Some(_scheme), None, Some(_path), query, fragment) with BaseUri {
  if (_path.segments.length != 1 && _path.segments(0).segment.isEmpty) throw new IllegalArgumentException("First segment cannot be empty otherwise it would be confused for an authority.")
}

case class SchemeWithRootlessPathUri(private val _scheme: Scheme, private val _path: RootlessPath, override val query: Option[Query], override val fragment: Option[Fragment])
  extends Uri(Some(_scheme), None, Some(_path), query, fragment) with BaseUri

case class SchemeWithQueryUri(private val _scheme: Scheme, private val _query: Query, override val fragment: Option[Fragment])
  extends Uri(Some(_scheme), None, None, Some(_query), fragment) with BaseUri

case class SchemeWithFragmentUri(private val _scheme: Scheme, private val _fragment: Fragment)
  extends Uri(Some(_scheme), None, None, None, Some(_fragment)) with BaseUri

// TODO: A URI can be scheme only: "dav:"; "about:";
case class SchemeUri(private val _scheme: Scheme)
  extends Uri(Some(_scheme), None, None, None, None) with BaseUri

/** In obsolete RFCs this was called a `RelativeUri`. */
sealed abstract class RelativeReference(override val authority: Option[Authority], override val path: Option[Path], override val query: Option[Query], override val fragment: Option[Fragment])
  extends Uri(None, authority, path, query, fragment)

/** Protocol Relative URI */
case class AuthorityRelativeReference(private val _authority: Authority, override val path: Option[AbsolutePath], override val query: Option[Query], override val fragment: Option[Fragment])
  extends RelativeReference(Some(_authority), path, query, fragment)

case class AbsolutePathRelativeReference(private val _path: AbsolutePath, override val query: Option[Query], override val fragment: Option[Fragment])
  extends RelativeReference(None, Some(_path), query, fragment) {
  if (_path.segments.length != 1 && _path.segments(0).segment.isEmpty) throw new IllegalArgumentException("First segment cannot be empty otherwise it would be an authority.")
}

/** Relative URI */
case class RootlessPathRelativeReference(private val _path: RootlessPath, override val query: Option[Query], override val fragment: Option[Fragment])
  extends RelativeReference(None, Some(_path), query, fragment) {
  // TODO: This is interesting, as a ':' could have been encoded. The default `pathDecoder` would decode it, but the default `pathEncoder` would not re-encode it as it is not in `PATH_CHARS_TO_ENCODE`.
  //       This could be handled by overriding `pathToString`, `pathToStringRaw`, `toString`, `toString` and `toStringRaw` to include ':' in the `pathEncoder`, but should only be for the first segment.
  if (_path.segments(0).segment.contains(":")) throw new IllegalArgumentException("First segment cannot contain any ':' otherwise it would be a scheme.")
}

case class QueryRelativeReference(private val _query: Query, override val fragment: Option[Fragment])
  extends RelativeReference(None, None, Some(_query), fragment)

case class FragmentRelativeReference(private val _fragment: Fragment)
  extends RelativeReference(None, None, None, Some(_fragment))

case object EmptyRelativeReference extends RelativeReference(None, None, None, None)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

object Uri {

  def apply(scheme: Option[Scheme], authority: Option[Authority], path: Option[Path], query: Option[Query], fragment: Option[Fragment]): Uri = {
    (scheme, authority, path, query, fragment) match {
      //case (Some(scheme), Some(authority), path: Option[AbsolutePath], _, None) => AbsoluteUri(scheme, authority, path, query) // NOTE: AbsolutePath is unchecked since it is eliminated by erasure.
      case (Some(scheme), Some(authority), None, _, None) => AbsoluteUri(scheme, authority, None, query)
      case (Some(scheme), Some(authority), Some(path: AbsolutePath), _, None) => AbsoluteUri(scheme, authority, Some(path), query)
      //case (Some(scheme), Some(authority), path: Option[AbsolutePath], _, Some(fragment)) => SchemeWithAuthorityAndFragmentUri(scheme, authority, path, query, fragment) // NOTE: AbsolutePath is unchecked since it is eliminated by erasure.
      case (Some(scheme), Some(authority), None, _, Some(fragment)) => SchemeWithAuthorityAndFragmentUri(scheme, authority, None, query, fragment)
      case (Some(scheme), Some(authority), Some(path: AbsolutePath), _, Some(fragment)) => SchemeWithAuthorityAndFragmentUri(scheme, authority, Some(path), query, fragment)
      case (Some(scheme), None, Some(path: AbsolutePath), _, _) => SchemeWithAbsolutePathUri(scheme, path, query, fragment)
      case (Some(scheme), None, Some(path: RootlessPath), _, _) => SchemeWithRootlessPathUri(scheme, path, query, fragment)
      case (Some(scheme), None, None, Some(query), _) => SchemeWithQueryUri(scheme, query, fragment)
      case (Some(scheme), None, None, None, Some(fragment)) => SchemeWithFragmentUri(scheme, fragment)
      case (Some(scheme), None, None, None, None) => SchemeUri(scheme)
      //case (None, Some(authority), path: Option[AbsolutePath], _, _) => AuthorityRelativeReference(authority, path, query, fragment) // NOTE: AbsolutePath is unchecked since it is eliminated by erasure.
      case (None, Some(authority), None, _, _) => AuthorityRelativeReference(authority, None, query, fragment)
      case (None, Some(authority), Some(path: AbsolutePath), _, _) => AuthorityRelativeReference(authority, Some(path), query, fragment)
      case (None, None, Some(path: AbsolutePath), _, _) => AbsolutePathRelativeReference(path, query, fragment)
      case (None, None, Some(path: RootlessPath), _, _) => RootlessPathRelativeReference(path, query, fragment)
      case (None, None, None, Some(query), _) => QueryRelativeReference(query, fragment)
      case (None, None, None, None, Some(fragment)) => FragmentRelativeReference(fragment: Fragment)
      case (None, None, None, None, None) => EmptyRelativeReference
      case _ =>
        // NOTE: The only combinations that should get here have an `Authority` and a `RootlessPath` which is missing the '/' delimiter between them.
        throw new IllegalArgumentException("ERROR: Unsupported URI type:" + scheme + ":" + authority + ":" + path + ":" + query + ":" + fragment + ":")
    }
  }

  // TODO: I would like to deprecate this, and replace it with a few methods that relate better to the new structures. (e.g. Option[Host]; Option[Path]; AbsoluePath; RootlessPath)
  /**
   * A simpler way to create a Uri, optionally with an AbsolutePath.
   *
   * NOTE: This method does NOT allow creating rootless/relative paths.
   */
  def apply(scheme: String = null,
            user: String = null,
            password: String = null,
            host: String = null,
            port: Int = 0,
            pathParts: Seq[Segment] = Seq.empty,
            query: Query = null,
            fragment: String = null): Uri =
    apply(Scheme.option(scheme),
          (if (user == null && password == null && host == null && port == 0) None else Authority.option(UserInfo.option(user, password), Host.option(host), if (port == 0) None else Option(port))), // TODO: Previously, EmptyAuthority did not exist, and IP addresses were not explicitly supported.
          AbsolutePath.option(pathParts), // TODO: Previously, all paths were absolute. Moving forward, this does not make sense. See above.
          Option(query),
          Fragment.option(fragment))

  @deprecated("Use other `apply` instead.", "1.0.0")
  def apply(scheme: Option[String],
            user: Option[String],
            password: Option[String],
            host: Option[String],
            port: Option[Int],
            pathParts: Seq[Segment],
            query: Query,
            fragment: Option[String]): Uri =
    apply(scheme.getOrElse(null), user.getOrElse(null), password.getOrElse(null), host.getOrElse(null), port.getOrElse(0), pathParts, query, fragment.getOrElse((null)))

  def apply(javaUri: java.net.URI): Uri = parse(javaUri.toASCIIString)

  def unapply(uri: Uri): Option[(Option[Scheme], Option[Authority], Option[Path], Option[Query], Option[Fragment])] =
    if (uri == null) None else Some((uri.scheme, uri.authority, uri.path, uri.query, uri.fragment))

  def parse(charSequence: CharSequence)(implicit c: UriConfig = UriConfig.DEFAULT): Uri =
    UriParser.parseUri(charSequence.toString, c)

  // TODO: Add `parseRaw` method:
  //def parseRaw(s: CharSequence)(implicit c: UriConfig = UriConfig.DEFAULT): Uri =
  //  UriParser.parse(s.toString, c.withNoDecoding)

  @deprecated("Use `parse` instead, ensuring the starting '?' and noting it returns a `Uri`.", "1.0.0")
  def parseQuery(s: CharSequence)(implicit c: UriConfig = UriConfig.DEFAULT): QueryString =
    UriParser.parseQuery(s.toString, c)

  @deprecated("Use `EmptyRelativeReference` instead.", "1.0.0")
  def empty: Uri = EmptyRelativeReference
}
