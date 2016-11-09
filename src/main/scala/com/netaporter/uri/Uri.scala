package com.netaporter.uri

import scala.collection.GenTraversableOnce
import com.netaporter.uri.Parameters._

/** URI/Reference, based on RFC 3986 section 4.1. */
sealed abstract class Uri(val scheme: Option[Scheme], val authority: Option[Authority], val path: Option[Path], val query: Option[Query], val fragment: Option[Fragment]) {

  @deprecated("Use `scheme: Option[Scheme]` instead.", "1.0.0")
  def protocol: Option[String] = scheme.map(_.scheme)

  def userInfo: Option[UserInfo] = authority.flatMap(_.userInfo)

  def userInfoString: Option[String] = authority.flatMap(_.userInfoString)

  def user: Option[String] = authority.flatMap(_.user)

  def password: Option[String] = authority.flatMap(_.password)

  @deprecated("Use `hostString` instead.", "1.0.0")
  def host: Option[String] = hostString

  def hostString: Option[String] = authority.flatMap(_.hostString)

  private def registeredName: Option[String] = authority.flatMap(_.host.flatMap(_.registeredName))

  @deprecated("Use `registeredNameParts` instead.", "1.0.0")
  def hostParts: Seq[String] = registeredNameParts

  def registeredNameParts: Seq[String] =
    registeredName.map(h => h.split('.').toSeq).getOrElse(Seq.empty)

  @deprecated("Use `registeredNameSubdomain` instead.", "1.0.0")
  def subdomain: Option[String] = registeredNameSubdomain

  def registeredNameSubdomain: Option[String] = registeredNameParts.headOption

  @deprecated("Use `registeredNamePublicSuffix` instead.", "1.0.0")
  def publicSuffix: Option[String] = registeredNamePublicSuffix

  def registeredNamePublicSuffix: Option[String] =
    registeredName.flatMap(registeredName => inet.PublicSuffixes.trie.longestMatch(registeredName.reverse).map(_.reverse))

  @deprecated("Use `registeredNamePublicSuffixes` instead.", "1.0.0")
  def publicSuffixes: Seq[String] = registeredNamePublicSuffixes

  def registeredNamePublicSuffixes: Seq[String] =
    registeredName.fold(Seq.empty[String])(registeredName => inet.PublicSuffixes.trie.matches(registeredName.reverse).map(_.reverse))

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
    pathSegmentOption(existingSegment).get

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
  def queryValue: QueryString = query.getOrElse(EmptyQuery)

  /**
   * Retrieves the query string if it exists.
   */
  def queryString: Option[String] = query match {
    case Some(query: StringQuery) => Option(query.queryString)
    case _ => None
  }

  /**
   * Retrieves the query parameters if any exist.
   */
  def queryParameters: Option[Seq[Parameter]] = query match {
    case Some(EmptyQuery) => None
    case Some(query: ParameterQuery) => Option(query.parameters)
    case _ => None
  }

  def queryValues(existingKey: String): Seq[Option[String]] = query match {
    case Some(query: ParameterQuery) => query.values(existingKey)
    case _ => Seq.empty[Option[String]]
  }

  def queryValueFirst(existingKey: String): Option[String] = query match {
    case Some(query: ParameterQuery) => query.valueFirst(existingKey)
    case _ => None
  }

  def queryMap: Map[String, Seq[String]] = query match {
    case Some(query: ParameterQuery) => query .toMap
    case _ => Map.empty[String, Seq[String]]
  }

  def fragmentString: Option[String] = fragment.map(_.fragment)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def withScheme(uri: Uri)(implicit config: UriConfig): Uri = withScheme(uri.scheme)

  def withScheme(newScheme: Option[Scheme] = None)(implicit config: UriConfig): Uri = copy(scheme = newScheme)

  def withScheme(newScheme: Scheme)(implicit config: UriConfig): Uri = withScheme(Option(newScheme))

  /**
   * Copies this `Uri` but with the `scheme` set based on the provided value.
   *
   * @param newScheme the new scheme to set
   * @return a new Uri with the provided scheme
   */
  def withScheme(newScheme: String)(implicit config: UriConfig): Uri = withScheme(Scheme.option(newScheme))

  def withAuthority(uri: Uri)(implicit config: UriConfig): Uri = withAuthority(uri.authority)

  def withAuthority(newAuthority: Option[Authority] = None)(implicit config: UriConfig): Uri = copy(authority = newAuthority)

  def withAuthority(newAuthority: Authority)(implicit config: UriConfig): Uri = withAuthority(Option(newAuthority))

  def withUserInfo(uri: Uri)(implicit config: UriConfig): Uri = withUserInfo(uri.userInfo)

  def withUserInfo(authority: Authority)(implicit config: UriConfig): Uri = withUserInfo(authority.userInfo)

  def withUserInfo(newUserInfo: Option[UserInfo] = None)(implicit config: UriConfig): Uri = {
    authority match {
      case None if newUserInfo.nonEmpty => throw new IllegalArgumentException("Cannot have a `userInfo` without a `host`.")
      case None => this
      case Some(authority) => withAuthority(authority.copy(userInfo = newUserInfo))
    }
  }

  def withUserInfo(newUserInfo: UserInfo)(implicit config: UriConfig): Uri = withUserInfo(Option(newUserInfo))

  def withUserInfo(newUserInfo: String)(implicit config: UriConfig): Uri = withUserInfo(StringUserInfo.option(newUserInfo))

  def withUser(uri: Uri)(implicit config: UriConfig): Uri = withUser(uri.user)

  def withUser(authority: Authority)(implicit config: UriConfig): Uri = withUser(authority.user)

  def withUser(userInfo: UserPasswordUserInfo)(implicit config: UriConfig): Uri = withUser(userInfo.user)

  def withUser(newUser: Option[String] = None)(implicit config: UriConfig): Uri = {
    authority match {
      case None if newUser.nonEmpty => throw new IllegalArgumentException("Cannot have a `user` without a `host`.")
      case None => this
      case Some(authority) => withAuthority(authority.copy(userInfo = newUser.map(newUser => UserPasswordUserInfo(newUser, authority.password))))
    }
  }

  /**
   * Copies this `Uri` but with the `user` set to the provided value.
   *
   * @param newUser the new user to set
   * @return a new Uri with the provided user
   */
  def withUser(newUser: String)(implicit config: UriConfig): Uri = withUser(Option(newUser))

  def withPassword(uri: Uri)(implicit config: UriConfig): Uri = withPassword(uri.password)

  def withPassword(authority: Authority)(implicit config: UriConfig): Uri = withPassword(authority.password)

  def withPassword(userInfo: UserPasswordUserInfo)(implicit config: UriConfig): Uri = withPassword(userInfo.password)

  def withPassword(newPassword: Option[String] = None)(implicit config: UriConfig): Uri = {
    authority match {
      case None if newPassword.nonEmpty => throw new IllegalArgumentException("Cannot have a `password` without a `host`.")
      case None => this
      case Some(authority) => authority.userInfo match {
        case None if newPassword.isEmpty => this
        case _ => copy(authority = Some(authority.copy(userInfo = UserInfo.option(authority.user.getOrElse(""), newPassword))))
      }
    }
  }

  /**
   * Copies this `Uri` but with the `password` set to the provided value.
   *
   * @param newPassword the new password to set
   * @return a new Uri with the provided password
   */
  def withPassword(newPassword: String)(implicit config: UriConfig): Uri = withPassword(Option(newPassword))

  def withHost(uri: Uri)(implicit config: UriConfig): Uri = withHost(uri.authority.flatMap(_.host))

  def withHost(authority: Authority)(implicit config: UriConfig): Uri = withHost(authority.host)

  def withHost(newHost: Option[Host])(implicit config: UriConfig): Uri =
    copy(authority = if (authority.isEmpty) Authority.option(None, newHost, None) else authority.map(_.copy(host = newHost)))

  /**
   * Copies this `Uri` but with the `host` set to the provided value.
   *
   * @param newHost the new host to set
   * @return a new Uri with the provided host
   */
  def withHost(newHost: Host)(implicit config: UriConfig): Uri = withHost(Option(newHost))

  def withHost(registeredName: String = null, ipv4Address: String = null, ipLiteral: String = null)(implicit config: UriConfig): Uri =
    withHost(Host.option(registeredName, ipv4Address, ipLiteral))

  def withPort(uri: Uri)(implicit config: UriConfig): Uri = withPort(uri.port)

  def withPort(authority: Authority)(implicit config: UriConfig): Uri = withPort(authority.port)

  def withPort(newPort: Option[Int] = None)(implicit config: UriConfig): Uri = {
    if (authority.nonEmpty) copy(authority = authority.map(_.copy(port = newPort)))
    else {
      if (newPort.nonEmpty) throw new IllegalArgumentException("Cannot have a `port` without a `host`.")
      this
    }
  }

  /**
   * Copies this `Uri` but with the `port` set to the provided value.
   *
   * @param newPort the new port to set
   * @return a new Uri with the provided port
   */
  def withPort(newPort: Int)(implicit config: UriConfig): Uri = withPort(if (newPort == 0) None else Option(newPort))

  def withPath(uri: Uri)(implicit config: UriConfig): Uri = withPath(uri.path)

  def withPath(newPath: Option[Path] = None)(implicit config: UriConfig): Uri = copy(path = newPath)

  def withPath(newPath: Path)(implicit config: UriConfig): Uri = withPath(Option(newPath))

  @deprecated("Use `appendMatrixParameter` instead.", "1.0.0")
  def addMatrixParam(pp: String, k: String, v: String): Uri = appendMatrixParameter(pp, k, v)(UriConfig.default)

  /**
   * Append the string segment to the path.
   */
  def appendSegment(stringSegment: String)(implicit config: UriConfig): Uri =
    path.fold(this)(path => withPath(path.appendSegment(StringSegment(stringSegment))))

  /**
   * Append the matrix parameter segment to the path.
   */
  def appendSegment(stringSegment: String, key: String, value: Any = None)(implicit config: UriConfig): Uri =
    path.fold(this)(path => withPath(path.appendSegment(MatrixParametersSegment(stringSegment, Parameter(key, value)))))

  /**
   * Append the matrix parameter key and value to any existing segment that matches `existingSegment`.
   */
  def appendMatrixParameter(existingSegment: String, key: String, value: Any = None)(implicit config: UriConfig): Uri =
    path.fold(this)(path => withPath(path.appendMatrixParameter(existingSegment, key, value)))

  /**
   * Append the matrix parameter to any existing segment that matches `existingSegment`.
   */
  def appendMatrixParameter(existingSegment: String, parameter: Parameter)(implicit config: UriConfig): Uri =
    path.fold(this)(path => withPath(path.appendMatrixParameter(existingSegment, parameter)))

  @deprecated("Use `appendMatrixParameterToLastSegment` instead.", "1.0.0")
  def addMatrixParam(k: String, v: String): Uri = appendMatrixParameterToLastSegment(k, v)(UriConfig.default)

  /**
   * If the `path` exists, append the matrix parameter key and value to the last segment.
   */
  def appendMatrixParameterToLastSegment(key: String, value: Any = None)(implicit config: UriConfig): Uri =
    path.fold(this)(path => withPath(path.appendMatrixParameterToLastSegment(key, value)))

  /**
   * If the `path` exists, append the matrix parameter to the last segment.
   */
  def appendMatrixParameterToLastSegment(parameter: Parameter)(implicit config: UriConfig): Uri =
    path.fold(this)(path => withPath(path.appendMatrixParameterToLastSegment(parameter)))

  def withQuery(uri: Uri)(implicit config: UriConfig): Uri = withQuery(uri.query)

  def withQuery(newQuery: Option[Query] = None)(implicit config: UriConfig): Uri = copy(query = newQuery)

  def withQuery(newQuery: Query)(implicit config: UriConfig): Uri = withQuery(Option(newQuery))

  /**
   * Copies this `Uri` but with the `query` parameters set to the provided value.
   */
  def withQuery(parameters: Seq[Parameter])(implicit config: UriConfig): Uri = withQuery(Query.option(parameters))

  def withQuery(firstParameter: Parameter, otherParameters: Parameter*)(implicit config: UriConfig): Uri = withQuery(Query.option(firstParameter +: otherParameters))

  @deprecated("Use `queryAppend` instead.", "1.0.0")
  def addParam(name: String, value: Any): Uri = queryAppend(name, value)(UriConfig.default)

  /**
   * Append a query parameter key and value.
   *
   * THEON: Old behaviour has been removed, that was not consistent with parsing and constructing through `QueryString`:
   *   If the value for the Query parameter is None, then this Query parameter will not be rendered in calls to toString or toStringRaw.
   *
   * @param key key of the new parameter
   * @param value value for the new parameter
   * @return a new Uri with the provided query parameter
   */
  def queryAppend(key: String, value: Any = None)(implicit config: UriConfig): Uri =
    withQuery(query.fold(Query(Parameter(key, value)))(query => query.append(Parameter(key, value))))

  @deprecated("Use `queryAppend` instead.", "1.0.0")
  def addParams(kvs: Seq[(String, Any)]): Uri = queryAppend(kvs)(UriConfig.default)

  /**
   * Append query parameters.
   *
   * THEON: Old behaviour has been removed, that was not consistent with parsing and constructing through `QueryString`:
   *   If the value for the Query parameter is None, then this Query parameter will not be rendered in calls to toString or toStringRaw.
   */
  def queryAppend(parameters: Seq[(String, Any)])(implicit config: UriConfig): Uri = {
    val newParameters = parameters.map {
      case (key, null) => Parameter(key, None)
      case (key, None) => Parameter(key, None)
      case (key, Some(value)) => Parameter(key, Option(value.toString))
      case (key, value) => Parameter(key, Option(value.toString))
    }
    withQuery(query.fold(Query(newParameters))(query => query.append(newParameters)))
  }

  /**
   * Append query parameters.
   */
  def queryAppend(parameters: Parameter*)(implicit config: UriConfig, di: DummyImplicit): Uri =
    withQuery(query.fold(Query(parameters))(query => query.append(parameters)))

  /**
   * Append query parameters from another query.
   */
  def queryAppend(otherQuery: ParameterQuery)(implicit config: UriConfig): Uri =
    withQuery(query.fold(otherQuery: Query)(query => query.append(otherQuery)))

  @deprecated("Use `queryMapParameters` instead.", "1.0.0")
  def mapQuery(f: Param => Param): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.mapParams(f))(UriConfig.default)
    case _ => this
  }

  /**
   * Transforms the `query` by applying the specified function to each query parameter.
   *
   * @param f A function that returns the new parameter when applied to each parameter
   */
  def queryMapParameters(f: Parameter => Parameter)(implicit config: UriConfig): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.mapParameters(f))
    case _ => this
  }

  @deprecated("Use `queryFlatMapParameters` instead.", "1.0.0")
  def flatMapQuery(f: Param => GenTraversableOnce[Param]): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.flatMapParams(f))(UriConfig.default)
    case _ => this
  }

  /**
   * Transforms the `query` by applying the specified function to each query parameter.
   *
   * @param f A function that returns the new parameter(s) when applied to each parameter
   */
  def queryFlatMapParameters(f: Parameter => scala.collection.GenTraversableOnce[Parameter])(implicit config: UriConfig): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.flatMapParameters(f))
    case _ => this
  }

  @deprecated("Use `queryMapKeys` instead.", "1.0.0")
  def mapQueryNames(f: String => String): Uri = queryMapKeys(f)(UriConfig.default)

  /**
   * Transforms the `query` by applying the specified function to each query parameter key.
   *
   * @param f A function that returns the new parameter key when applied to each parameter key
   */
  def queryMapKeys(f: String => String)(implicit config: UriConfig): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.mapKeys(f))
    case _ => this
  }

  /**
   * Transforms the `query` by applying the specified function to each query parameter value.
   * NOTE: This ignores `None` values, so you CANNOT transform them.
   *
   * @param f A function that returns the new parameter value when applied to each parameter value
   */
  @deprecated("Use `queryMapValues(Option[String] => Option[String])` instead.", "1.0.0")
  def mapQueryValues(f: String => String): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.mapParamValues(f))(UriConfig.default)
    case _ => this
  }

  /**
   * Transforms the `query` by applying the specified function to each query parameter value.
   *
   * @param f A function that returns the new parameter value when applied to each parameter value
   */
  def queryMapValues(f: Option[String] => Option[String])(implicit config: UriConfig): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.mapValues(f))
    case _ => this
  }

  @deprecated("Use `queryFilterParameters` instead.", "1.0.0")
  def filterQuery(f: Param => Boolean): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.filterParams(f))(UriConfig.default)
    case _ => this
  }

  /**
   * Removes any query parameters that return false when applied to the given function.
   */
  def queryFilterParameters(f: Parameter => Boolean)(implicit config: UriConfig): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.filterParameters(f))
    case _ => this
  }

  @deprecated("Use `queryFilterKeys` instead.", "1.0.0")
  def filterQueryNames(f: String => Boolean): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.filterParamsNames(f))(UriConfig.default)
    case _ => this
  }

  /**
   * Removes any query parameters that return false when their key is applied to the given function.
   */
  def queryFilterKeys(f: String => Boolean)(implicit config: UriConfig): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.filterKeys(f))
    case _ => this
  }

  /**
   * Removes any query parameters that return false when their value is applied to the given function.
   * NOTE: This removes query parameters with a value of `None`.
   */
  @deprecated("Use `queryFilterValues(Option[String] => Boolean)` instead.", "1.0.0")
  def filterQueryValues(f: String => Boolean): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.filterParamsValues(f))(UriConfig.default)
    case _ => this
  }

  /**
   * Removes any query parameters that return false when their value is applied to the given function.
   */
  def queryFilterValues(f: Option[String] => Boolean)(implicit config: UriConfig): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.filterValues(f))
    case _ => this
  }

  @deprecated("Use `queryReplaceMatching` instead.", "1.0.0")
  def replaceParams(k: String, v: Any): Uri = queryReplaceMatching(k, v)(UriConfig.default)

  /**
   * Removes the existing query parameters with the specified key and appends a new query parameter with the specified value.
   *
   * THEON: Old behaviour has been removed, that was not consistent with parsing and constructing through `QueryString`:
   *   If the value passed in is None, then all Query parameters with the specified key are removed.
   *
   * THEON: Old behaviour has been removed from `Parameters`, that was not expected:
   *   If the `existingKey` did not exist, a new parameter is appended without any being removed.
   *
   * @param existingKey Key for the Query parameter(s) to replace
   * @param newValue Value to replace with
   * @return A new Uri with the result of the replace
   */
  def queryReplaceMatching(existingKey: String, newValue: Any)(implicit config: UriConfig): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.replaceMatching(existingKey, newValue))
    case _ => this
  }

  @deprecated("Use `withQuery` instead.", "1.0.0")
  def replaceAllParams(params: Param*): Uri = withQuery(params)(UriConfig.default)

  @deprecated("Use `queryRemoveMatching` instead.", "1.0.0")
  def removeParams(k: String): Uri = queryRemoveMatching(k)(UriConfig.default)

  /**
   * Removes query parameters with the specified key.
   *
   * @param existingKey Key for the Query parameter(s) to remove
   */
  def queryRemoveMatching(existingKey: String)(implicit config: UriConfig): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.removeMatching(existingKey))
    case _ => this
  }

  @deprecated("Use `queryRemoveMatching` instead.", "1.0.0")
  def removeParams(a: Seq[String]): Uri = queryRemoveMatching(a)(UriConfig.default)

  /**
   * Removes query parameters with a specified key contained in `existingKeys`.
   *
   * @param existingKeys Keys for the Query parameter(s) to remove
   */
  def queryRemoveMatching(existingKeys: Seq[String])(implicit config: UriConfig): Uri = query match {
    case Some(query: ParameterQuery) => withQuery(query.removeMatching(existingKeys))
    case _ => this
  }

  @deprecated("Use `withQuery()` instead.", "1.0.0")
  def removeAllParams(): Uri = withQuery()(UriConfig.default)

  def queryRemoveAllParameters(implicit config: UriConfig): Uri =
    query.fold(this)(query => withQuery(query.withParameters()))

  def withFragment(uri: Uri)(implicit config: UriConfig): Uri = withFragment(uri.fragment)

  def withFragment(newFragment: Option[Fragment] = None)(implicit config: UriConfig): Uri = copy(fragment = newFragment)

  def withFragment(newFragment: Fragment)(implicit config: UriConfig): Uri = withFragment(Option(newFragment))

  /**
   * Copies this `Uri` but with the `fragment` set to the provided value.
   *
   * @param newFragment the new fragment to set
   * @return a new Uri with the provided fragment
   */
  def withFragment(newFragment: String)(implicit config: UriConfig): Uri = withFragment(Fragment.option(newFragment))

  @deprecated("Use `copy` instead.", "1.0.0")
  def copyOld(scheme: Option[String] = protocol,
              user: Option[String] = user,
              password: Option[String] = password,
              host: Option[String] = hostString,
              port: Option[Int] = port,
              pathParts: Seq[PathPart] = pathParts,
              query: QueryString = queryValue,
              fragment: Option[String] = fragmentString): Uri =
    Uri(scheme, user, password, host, port, pathParts, query, fragment)

  def copy(scheme: Option[Scheme] = scheme, authority: Option[Authority] = authority, path: Option[Path] = path, query: Option[Query] = query, fragment: Option[Fragment] = fragment)(implicit config: UriConfig): Uri =
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

  def schemeToString(implicit config: UriConfig): String =
    scheme.map(_.toString).getOrElse("")

  def authorityToString(implicit config: UriConfig): String =
    authority.map(_.toString).getOrElse("")

  def pathToString(implicit config: UriConfig): String =
    path.map(_.toString).getOrElse("")

  @deprecated("Use `pathToString(config.withNoEncoding)` instead.", "1.0.0")
  def pathRaw(implicit c: UriConfig = UriConfig.default): String = pathToString(c.withNoEncoding)

  def queryToString(implicit config: UriConfig): String =
    query.map(_.toString).getOrElse("")

  @deprecated("Use `queryToString(config.withNoEncoding)` instead.", "1.0.0")
  def queryStringRaw(implicit c: UriConfig = UriConfig.default): String = queryToString(c.withNoEncoding)

  def fragmentToString(implicit config: UriConfig): String =
    fragment.map(_.toString).getOrElse("")

  def toString()(implicit config: UriConfig, di: DummyImplicit): String = toString

  /**
   * Based on RFC 3986 section 5.3: Component recomposition.
   */
  def toString(implicit config: UriConfig): String =
    schemeToString + authorityToString + pathToString + queryToString + fragmentToString

  /**
   * Returns the string representation of this Uri with no encoders taking place.
   * (e.g. non ASCII characters will not be percent encoded)
   */
  @deprecated("Use `toString(config.withNoEncoding)` instead.", "1.0.0")
  def toStringRaw(implicit config: UriConfig = UriConfig.default): String = toString(config.withNoEncoding)

  /**
   * Converts to a Java URI.
   * This involves a toString + URI.parse.
   *
   * @return a URI matching this Uri
   */
  def toURI(implicit config: UriConfig = UriConfig.CONSERVATIVE): java.net.URI = new java.net.URI(toString)
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

object Uri {

  def apply(scheme: Option[Scheme], authority: Option[Authority], path: Option[Path], query: Option[Query], fragment: Option[Fragment])(implicit config: UriConfig): Uri =
    if (scheme.isEmpty) RelativeReference(authority, path, query, fragment) else UriWithScheme(scheme.get, authority, path, query, fragment)

  /**
   * A simpler way to create a Uri, optionally with an AbsolutePath.
   * NOTE: This method does NOT allow creating rootless/relative paths.
   */
  @deprecated("Use other `apply`, or `parse`, or the DSL instead.", "1.0.0")
  def apply(scheme: String = null,
            user: String = null,
            password: String = null,
            host: String = null,
            port: Int = 0,
            pathParts: Seq[PathPart] = Seq.empty,
            query: QueryString  = null,
            fragment: String = null): Uri =
    apply(Scheme.option(scheme),
          (if (user == null && password == null && host == null && port == 0) None else Authority.option(UserInfo.option(user, password), Host.parse(host)(UriConfig.default), if (port == 0) None else Option(port))), // NOTE: Previously, EmptyAuthority did not exist.
          AbsolutePath.option(pathParts), // NOTE: Previously, all paths were absolute.
          Option(query),
          Fragment.option(fragment))(UriConfig.default)

  @deprecated("Use other `apply` instead.", "1.0.0")
  def apply(scheme: Option[String],
            user: Option[String],
            password: Option[String],
            host: Option[String],
            port: Option[Int],
            pathParts: Seq[PathPart],
            query: QueryString,
            fragment: Option[String]): Uri =
    apply(scheme.getOrElse(null), user.getOrElse(null), password.getOrElse(null), host.getOrElse(null), port.getOrElse(0), pathParts, query, fragment.getOrElse((null)))

  def apply(uriString: String)(implicit config: UriConfig): Uri = parsing.ParseUri(uriString)

  def apply(javaUri: java.net.URI)(implicit config: UriConfig): Uri = apply(javaUri.toASCIIString)

  def unapply(uri: Uri): Option[(Option[Scheme], Option[Authority], Option[Path], Option[Query], Option[Fragment])] =
    if (uri == null) None else Some((uri.scheme, uri.authority, uri.path, uri.query, uri.fragment))

  def option(uriString: String)(implicit config: UriConfig): Option[Uri] = parsing.ParseUri.option(uriString)

  @deprecated("Use `apply` instead, noting that it takes a `String`.", "1.0.0")
  def parse(charSequence: CharSequence)(implicit config: UriConfig): Uri =
    parsing.UriParser.parse(charSequence.toString, config)

  @deprecated("Use `apply` instead, ensuring the starting '?' and noting it returns a `Uri`.", "1.0.0")
  def parseQuery(s: CharSequence)(implicit c: UriConfig = UriConfig.default): QueryString =
    parsing.UriParser.parseQuery(s.toString, c)

  @deprecated("Use `EmptyReference` instead.", "1.0.0")
  def empty: Uri = EmptyReference
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/**
 * URI, based on RFC 3986 section 3.
 *
 * NOTE: According to the RFC: "The scheme and path components are required, though the path may be empty (no characters)."
 *       A `Uri` that contains an authority would have to allow the path to be an `AbsolutePath` or an empty rootless path.
 *       This would require checking to ensure the `RootlessPath` must be empty, or a more complicated `Path` type setup.
 *       Instead, we use `None` to represent an empty rootless path.
 */
sealed abstract class UriWithScheme(protected val _scheme: Scheme, override val authority: Option[Authority], override val path: Option[Path], override val query: Option[Query], override val fragment: Option[Fragment])
  extends Uri(Some(_scheme), authority, path, query, fragment)

object UriWithScheme {

  def apply(scheme: Scheme, authority: Option[Authority], path: Option[Path], query: Option[Query], fragment: Option[Fragment])(implicit config: UriConfig): UriWithScheme = {
    (authority, path, query, fragment) match {
      case (None, None, None, None) => SchemeUri(scheme)
      case (None, None, None, Some(fragment)) => SchemeWithFragmentUri(scheme, fragment)
      case (None, None, Some(query), _) => SchemeWithQueryUri(scheme, query, fragment)
      case (None, Some(path: RootlessPath), _, _) => SchemeWithRootlessPathUri(scheme, path, query, fragment)
      case (None, Some(path: AbsolutePath), _, _) => SchemeWithAbsolutePathUri(scheme, path, query, fragment)
      case (Some(authority), None, _, _) => SchemeWithAuthorityUri(scheme, authority, None, query, fragment)
      case (Some(authority), Some(path: AbsolutePath), _, _) => SchemeWithAuthorityUri(scheme, authority, Some(path), query, fragment)
      case (Some(authority), Some(_: RootlessPath), _, _) =>
        throw new IllegalArgumentException("Invalid URI:" + scheme + ":" + authority + ":" + path + ":" + query + ":" + fragment + ":")
    }
  }
}

/** Absolute URI, based on RFC 3986 section 4.3. */
sealed trait AbsoluteUri extends UriWithScheme {
  // The following constraint is not required with the current implementation, as it can never fail.
  // if (fragment.nonEmpty) throw new IllegalArgumentException("`fragment` must be `None`.")

  /** Relative resolution, based on RFC 3986 section 5.2. */
  def resolve(referenceUri: Uri, ignoreIdenticalScheme: Boolean = false)(implicit config: UriConfig): UriWithScheme = {
    // // Original implementation, base on the algorithm in the RFC:
    // (if (referenceUri.scheme.nonEmpty) {
    //   if (referenceUri.path.isEmpty) referenceUri
    //   else referenceUri.withPath(referenceUri.path.map(_.removeDotSegments)) // `removeDotSegments` has already been run.
    // } else if (referenceUri.authority.nonEmpty) {
    //   Uri(scheme, referenceUri.authority, referenceUri.path.map(_.removeDotSegments), referenceUri.query, referenceUri.fragment) // `removeDotSegments` has already been run.
    // } else referenceUri.path match {
    //   case None => Uri(scheme, authority, path, if (referenceUri.query.nonEmpty) referenceUri.query else query, referenceUri.fragment)
    //   case Some(_: AbsolutePath) => Uri(scheme, authority, referenceUri.path.map(_.removeDotSegments), referenceUri.query, referenceUri.fragment) // `removeDotSegments` has already been run.
    //   case Some(referenceUriPath: RootlessPath) =>
    //     path match {
    //       case None =>
    //         authority match {
    //           case None => Uri(scheme, authority, Option(referenceUriPath.removeDotSegments), referenceUri.query, referenceUri.fragment)
    //           case Some(_) => Uri(scheme, authority, Option(EmptyAbsolutePath.merge(referenceUriPath).removeDotSegments), referenceUri.query, referenceUri.fragment)
    //         }
    //       case Some(path) => Uri(scheme, authority, Option(path.merge(referenceUriPath).removeDotSegments), referenceUri.query, referenceUri.fragment)
    //     }
    // }).asInstanceOf[UriWithScheme]
    referenceUri match {
      case referenceUri: UriWithScheme if !ignoreIdenticalScheme || scheme != referenceUri.scheme => referenceUri
      case Uri(_, None, None, None, _) => UriWithScheme(_scheme, authority, path, query, referenceUri.fragment)
      case Uri(_, None, None, Some(_), _) => UriWithScheme(_scheme, authority, path, referenceUri.query, referenceUri.fragment)
      case Uri(_, None, Some(_: AbsolutePath), _, _) => UriWithScheme(_scheme, authority, referenceUri.path, referenceUri.query, referenceUri.fragment)
      case Uri(_, None, Some(referenceUriPath: RootlessPath), _, _) => (path, authority) match {
        case (None, None) => UriWithScheme(_scheme, authority, referenceUri.path.map(_.removeDotSegments), referenceUri.query, referenceUri.fragment)
        case (None, Some(_)) => UriWithScheme(_scheme, authority, Option(EmptyAbsolutePath.merge(referenceUriPath).removeDotSegments), referenceUri.query, referenceUri.fragment)
        case (Some(_), _) => UriWithScheme(_scheme, authority, path.map(_.merge(referenceUriPath).removeDotSegments), referenceUri.query, referenceUri.fragment)
      }
      case Uri(_, Some(_), _, _, _) => UriWithScheme(_scheme, referenceUri.authority, referenceUri.path, referenceUri.query, referenceUri.fragment)
    }
  }
}

object AbsoluteUri {

  def apply(scheme: Scheme, authority: Option[Authority], path: Option[Path], query: Option[Query])(implicit config: UriConfig): AbsoluteUri =
    UriWithScheme(scheme, authority, path, query, None).asInstanceOf[AbsoluteUri]
}

sealed abstract case class SchemeWithAuthorityUri(protected override val _scheme: Scheme, private val _authority: Authority, override val path: Option[AbsolutePath], override val query: Option[Query], override val fragment: Option[Fragment])
  extends UriWithScheme(_scheme, Some(_authority), path, query, fragment)

object SchemeWithAuthorityUri {

  def apply(scheme: Scheme, authority: Authority, path: Option[AbsolutePath], query: Option[Query], fragment: Option[Fragment])(implicit config: UriConfig): SchemeWithAuthorityUri = {
    val _path = if (config.pathSegmentNormalization) path.map(path => path.copy(Path.removeDotSegments(path.segments))) else path
    fragment match {
      case None => new SchemeWithAuthorityUri(scheme, authority, _path, query, fragment) with AbsoluteUri {}
      case _ => new SchemeWithAuthorityUri(scheme, authority, _path, query, fragment) {}
    }
  }
}

sealed abstract case class SchemeWithAbsolutePathUri(protected override val _scheme: Scheme, private val _path: AbsolutePath, override val query: Option[Query], override val fragment: Option[Fragment])
  extends UriWithScheme(_scheme, None, Some(_path), query, fragment)

object SchemeWithAbsolutePathUri {

  def apply(scheme: Scheme, path: AbsolutePath, query: Option[Query], fragment: Option[Fragment])(implicit config: UriConfig): SchemeWithAbsolutePathUri = {
    if (path.segments.length != 1 && path.segments(0).segment.isEmpty) throw new IllegalArgumentException("First segment cannot be empty otherwise it would be an authority.")
    val _path = if (config.pathSegmentNormalization) path.copy(Path.removeDotSegments(path.segments)) else path
    fragment match {
      case None => new SchemeWithAbsolutePathUri(scheme, _path, query, fragment) with AbsoluteUri {}
      case _ => new SchemeWithAbsolutePathUri(scheme, _path, query, fragment) {}
    }
  }
}

sealed abstract case class SchemeWithRootlessPathUri(protected override val _scheme: Scheme, private val _path: RootlessPath, override val query: Option[Query], override val fragment: Option[Fragment])
  extends UriWithScheme(_scheme, None, Some(_path), query, fragment)

object SchemeWithRootlessPathUri {

  def apply(scheme: Scheme, path: RootlessPath, query: Option[Query], fragment: Option[Fragment])(implicit config: UriConfig): SchemeWithRootlessPathUri = {
    val _path = if (config.pathSegmentNormalization) path.copy(Path.removeDotSegments(path.segments)) else path
    fragment match {
      case None => new SchemeWithRootlessPathUri(scheme, _path, query, fragment) with AbsoluteUri {}
      case _ => new SchemeWithRootlessPathUri(scheme, _path, query, fragment) {}
    }
  }
}

sealed abstract case class SchemeWithQueryUri(protected override val _scheme: Scheme, private val _query: Query, override val fragment: Option[Fragment])
  extends UriWithScheme(_scheme, None, None, Some(_query), fragment)

object SchemeWithQueryUri {

  def apply(scheme: Scheme, query: Query, fragment: Option[Fragment]): SchemeWithQueryUri = {
    fragment match {
      case None => new SchemeWithQueryUri(scheme, query, fragment) with AbsoluteUri {}
      case _ => new SchemeWithQueryUri(scheme, query, fragment) {}
    }
  }
}

sealed abstract case class SchemeWithFragmentUri(protected override val _scheme: Scheme, private val _fragment: Fragment)
  extends UriWithScheme(_scheme, None, None, None, Some(_fragment))

object SchemeWithFragmentUri {

  def apply(scheme: Scheme, fragment: Fragment): SchemeWithFragmentUri = {
    new SchemeWithFragmentUri(scheme, fragment) {}
  }
}

/** Scheme only URI. (e.g. "dav:", "about:") */
sealed abstract case class SchemeUri(protected override val _scheme: Scheme)
  extends UriWithScheme(_scheme, None, None, None, None) with AbsoluteUri

object SchemeUri {

  def apply(scheme: Scheme): SchemeUri = {
    new SchemeUri(scheme) {}
  }
}

/**
 * Relative Reference, based on RFC 3986 section 4.2.
 * In obsolete RFCs this was called a Partial URI or a Relative URI.
 */
sealed abstract class RelativeReference(override val authority: Option[Authority], override val path: Option[Path], override val query: Option[Query], override val fragment: Option[Fragment])
  extends Uri(None, authority, path, query, fragment)

object RelativeReference {

  def apply(authority: Option[Authority], path: Option[Path], query: Option[Query], fragment: Option[Fragment])(implicit config: UriConfig): Uri = {
    (authority, path, query, fragment) match {
      case (None, None, None, None) => EmptyReference
      case (None, None, None, Some(fragment)) => FragmentReference(fragment)
      case (None, None, Some(query), _) => QueryReference(query, fragment)
      case (None, Some(path: AbsolutePath), _, _) => AbsolutePathReference(path, query, fragment)
      case (None, Some(path: RootlessPath), _, _) => RelativePathReference(path, query, fragment)
      case (Some(authority), None, _, _) => NetworkPathReference(authority, None, query, fragment)
      case (Some(authority), Some(path: AbsolutePath), _, _) => NetworkPathReference(authority, Some(path), query, fragment)
      case (Some(authority), Some(_: RootlessPath), _, _) =>
        throw new IllegalArgumentException("Invalid URI:" + authority + ":" + path + ":" + query + ":" + fragment + ":")
    }
  }
}

sealed abstract case class NetworkPathReference(private val _authority: Authority, override val path: Option[AbsolutePath], override val query: Option[Query], override val fragment: Option[Fragment])
  extends RelativeReference(Some(_authority), path, query, fragment)

object NetworkPathReference {

  def apply(authority: Authority, path: Option[AbsolutePath], query: Option[Query], fragment: Option[Fragment])(implicit config: UriConfig): NetworkPathReference = {
    val _path = if (config.pathSegmentNormalization) path.map(path => path.copy(Path.removeDotSegments(path.segments))) else path
    new NetworkPathReference(authority, _path, query, fragment) {}
  }
}

sealed abstract case class AbsolutePathReference(private val _path: AbsolutePath, override val query: Option[Query], override val fragment: Option[Fragment])
  extends RelativeReference(None, Some(_path), query, fragment)

object AbsolutePathReference {

  def apply(path: AbsolutePath, query: Option[Query], fragment: Option[Fragment])(implicit config: UriConfig): AbsolutePathReference = {
    if (path.segments.length != 1 && path.segments(0).segment.isEmpty) throw new IllegalArgumentException("First segment cannot be empty otherwise it would be an authority.")
    val _path = if (config.pathSegmentNormalization) path.copy(Path.removeDotSegments(path.segments)) else path
    new AbsolutePathReference(_path, query, fragment) {}
  }
}

sealed abstract case class RelativePathReference(private val _path: RootlessPath, override val query: Option[Query], override val fragment: Option[Fragment])
  extends RelativeReference(None, Some(_path), query, fragment)

object RelativePathReference {

  /**
   * The first segment cannot contain any ':'s, as it would be mistaken for a scheme, based on RFC 3986 section 4.2 last paragraph.
   * Therefore, if the first segment contains any ':'s, a "." segment is prepended.
   */
  def apply(path: RootlessPath, query: Option[Query], fragment: Option[Fragment]): RelativePathReference = {
    val _path = if (path.segments.head.segment.contains(":")) path.copy(StringSegment(".") +: path.segments) else path
    new RelativePathReference(_path, query, fragment) {}
  }
}

sealed abstract case class QueryReference(private val _query: Query, override val fragment: Option[Fragment])
  extends RelativeReference(None, None, Some(_query), fragment)

object QueryReference {

  def apply(query: Query, fragment: Option[Fragment]): QueryReference = {
    new QueryReference(query, fragment) {}
  }
}

sealed abstract case class FragmentReference(private val _fragment: Fragment)
  extends RelativeReference(None, None, None, Some(_fragment))

object FragmentReference {

  def apply(fragment: Fragment): FragmentReference = {
    new FragmentReference(fragment) {}
  }
}

/** The existence of an empty relative reference is noted in RFC 3986 section 4.4 first paragraph. */
object EmptyReference extends RelativeReference(None, None, None, None)
