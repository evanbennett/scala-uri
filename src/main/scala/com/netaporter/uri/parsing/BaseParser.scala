package com.netaporter.uri.parsing

import com.netaporter.uri._
import fastparse.all._

class BaseParser(val input: String)(implicit protected val config: UriConfig) {

  protected val extractScheme = (scheme: String) =>
    Scheme(scheme)

  protected val extractUserInfo = (userInfo: String) =>
    UserInfo(config.userInfoDecoder.decode(userInfo, input))

  protected val extractRegisteredNameHost = (registeredName: String) =>
    Host.fromParser(registeredName = config.registeredNameDecoder.decode(registeredName, input))

  protected val extractIpv4AddressHost = (ipv4Address: String) =>
    Host.fromParser(ipv4Address = ipv4Address)

  protected val extractIpLiteralHost = (ipLiteral: String) =>
    Host.fromParser(ipLiteral = ipLiteral)

  protected val extractPort = (num: String) =>
    if (num.isEmpty) -1 else num.toInt

  protected val extractAuthority: ((Option[UserInfo], Option[Host], Option[Int])) => Authority = { case (userInfo, host, port) =>
    Authority(userInfo, host, port)
  }

  protected val extractSegment = (segment: String) =>
    StringSegment(config.pathDecoder.decode(segment, input))

  protected val extractAbsolutePath: ((Segment, Seq[Segment])) => AbsolutePath = { case (firstSegment, segments) =>
    AbsolutePath(firstSegment +: segments)
  }

  protected val extractEmptyAbsolutePath = (emptyString: String) =>
    EmptyAbsolutePath

  protected val extractRootlessPath: ((Segment, Seq[Segment])) => RootlessPath = { case (firstSegment, segments) =>
    RootlessPath(firstSegment +: segments)
  }

  protected val extractParameter: ((String, Option[String])) => Parameter = { case (key, value) =>
    Parameter(key, value)
  }

  protected val extractQuery = (queryString: String) =>
    Query(config.queryDecoder.decode(queryString, input))

  protected val extractFragment = (fragment: String) =>
    Fragment(config.fragmentDecoder.decode(fragment, input))

  protected val extractSchemeWithAuthorityUri: ((Scheme, Authority, Option[AbsolutePath], Option[Query], Option[Fragment])) => SchemeWithAuthorityUri = { case (scheme, authority, path, query, fragment) =>
    SchemeWithAuthorityUri(scheme, authority, path, query, fragment)
  }

  protected val extractSchemeWithAbsolutePathUri: ((Scheme, AbsolutePath, Option[Query], Option[Fragment])) => SchemeWithAbsolutePathUri = { case (scheme, path, query, fragment) =>
    SchemeWithAbsolutePathUri(scheme, path, query, fragment)
  }

  protected val extractSchemeWithRootlessPathUri: ((Scheme, RootlessPath, Option[Query], Option[Fragment])) => SchemeWithRootlessPathUri = { case (scheme, path, query, fragment) =>
    SchemeWithRootlessPathUri(scheme, path, query, fragment)
  }

  protected val extractSchemeWithQueryUri: ((Scheme, Query, Option[Fragment])) => SchemeWithQueryUri = { case (scheme, query, fragment) =>
    SchemeWithQueryUri(scheme, query, fragment)
  }

  protected val extractSchemeWithFragmentUri: ((Scheme, Fragment)) => SchemeWithFragmentUri = { case (scheme, fragment) =>
    SchemeWithFragmentUri(scheme, fragment)
  }

  protected val extractSchemeUri = (scheme: Scheme) =>
    SchemeUri(scheme)

  protected val extractNetworkPathReference: ((Authority, Option[AbsolutePath], Option[Query], Option[Fragment])) => NetworkPathReference = { case (authority, path, query, fragment) =>
    NetworkPathReference(authority, path, query, fragment)
  }

  protected val extractAbsolutePathReference: ((AbsolutePath, Option[Query], Option[Fragment])) => AbsolutePathReference = { case (path, query, fragment) =>
    AbsolutePathReference(path, query, fragment)
  }

  protected val extractRelativePathReference: ((RootlessPath, Option[Query], Option[Fragment])) => RelativePathReference = { case (path, query, fragment) =>
    RelativePathReference(path, query, fragment)
  }

  protected val extractQueryReference: ((Query, Option[Fragment])) => QueryReference = { case (query, fragment) =>
    QueryReference(query, fragment)
  }

  protected val extractFragmentReference = (fragment: Fragment) =>
    FragmentReference(fragment)

  protected val extractEmptyReference = (emptyString: String) =>
    EmptyReference

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  protected val scheme: P[Scheme] =
    P(BaseParser.SCHEME.! ~ ":" map extractScheme)

  protected val userInfo: P[UserInfo] =
    P(USER_INFO.rep.! ~ "@" map extractUserInfo)

  protected val registeredName: P[String] =
    P(REGISTERED_NAME.rep(1).!)

  protected val host: P[Host] =
    P((("[" ~ (BaseParser.IPVFUTURE | BaseParser.IPV6_ADDRESS) ~ "]").! map extractIpLiteralHost) | (BaseParser.IPV4_ADDRESS.! map extractIpv4AddressHost) | (registeredName map extractRegisteredNameHost))

  protected val port: P[Int] =
    P(":" ~ BaseParser.DIGIT.rep.! map extractPort)

  protected val authority: P[Authority] =
    P("//" ~ userInfo.? ~ host.? ~ port.? map extractAuthority)

  protected val segment: P[Segment] =
    P(SEGMENT.rep.! map extractSegment)

  /** Segment non-zero length. */
  protected val segmentNz: P[Segment] =
    P(SEGMENT.rep(1).! map extractSegment)

  /** Segment non-zero length, no ':'s. */
  protected val segmentNzNc: P[Segment] =
    P(SEGMENT_NO_COLONS.rep(1).! map extractSegment)

  /** A sequence of segments that MUST start with a '/'. (Must follow an authority.) */
  protected val pathAbEmpty: P[AbsolutePath] =
    P("/" ~ segment ~ ("/" ~ segment).rep map extractAbsolutePath)

  /** A sequence of segments that MUST start with a '/' but not "//" otherwise it would be confused for an authority. */
  protected val pathAbsolute: P[AbsolutePath] =
    P(("/" ~ segmentNz ~ ("/" ~ segment).rep map extractAbsolutePath) | ("/" ~ "".! map extractEmptyAbsolutePath))

  /**  A sequence of segments that MUST NOT start with a '/', and the first segment MUST NOT contain any ':' otherwise to would be confused for a scheme. */
  protected val pathNoScheme: P[RootlessPath] =
    P(segmentNzNc ~ ("/" ~ segment).rep map extractRootlessPath)

  /**  A sequence of segments that MUST NOT start with a '/'. */
  protected val pathRootless: P[RootlessPath] =
    P(segmentNz ~ ("/" ~ segment).rep map extractRootlessPath)

  // `_pathEmpty` is supported by making a path optional.

  protected val query: P[Query] =
    P("?" ~ QUERY.rep.! map extractQuery)

  protected val fragment: P[Fragment] =
    P("#" ~ FRAGMENT.rep.! map extractFragment)

  // - - - - - - - - - -

  protected val schemeWithAuthorityUri: P[Uri] =
    P(scheme ~ authority ~ pathAbEmpty.? ~ query.? ~ fragment.? map extractSchemeWithAuthorityUri)

  protected val schemeWithAbsolutePathUri: P[Uri] =
    P(scheme ~ pathAbsolute ~ query.? ~ fragment.? map extractSchemeWithAbsolutePathUri)

  protected val schemeWithRootlessPathUri: P[Uri] =
    P(scheme ~ pathRootless ~ query.? ~ fragment.? map extractSchemeWithRootlessPathUri)

  protected val schemeWithQueryUri: P[Uri] =
    P(scheme ~ query ~ fragment.? map extractSchemeWithQueryUri)

  protected val schemeWithFragmentUri: P[Uri] =
    P(scheme ~ fragment map extractSchemeWithFragmentUri)

  protected val schemeUri: P[Uri] =
    P(scheme map extractSchemeUri)

  protected val networkPathReference: P[Uri] =
    P(authority ~ pathAbEmpty.? ~ query.? ~ fragment.? map extractNetworkPathReference)

  protected val absolutePathReference: P[Uri] =
    P(pathAbsolute ~ query.? ~ fragment.? map extractAbsolutePathReference)

  protected val relativePathReference: P[Uri] =
    P(pathNoScheme ~ query.? ~ fragment.? map extractRelativePathReference)

  protected val queryReference: P[Uri] =
    P(query ~ fragment.? map extractQueryReference)

  protected val fragmentReference: P[Uri] =
    P(fragment map extractFragmentReference)

  protected val emptyReference: P[Uri] =
    P("".! map extractEmptyReference)

  protected val uri: P[Uri] =
    (schemeWithAuthorityUri | schemeWithAbsolutePathUri | schemeWithRootlessPathUri | schemeWithQueryUri | schemeWithFragmentUri | schemeUri |
      networkPathReference | absolutePathReference | relativePathReference | queryReference | fragmentReference | emptyReference) ~ End

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  lazy val parseUri = uri.parse(input)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  protected val USER_INFO: P[Unit] = BaseParser.USER_INFO
  protected val REGISTERED_NAME: P[Unit] = BaseParser.REGISTERED_NAME
  protected val SEGMENT: P[Unit] = BaseParser.SEGMENT
  protected val SEGMENT_NO_COLONS: P[Unit] = BaseParser.SEGMENT_NO_COLONS
  protected val QUERY: P[Unit] = BaseParser.QUERY
  protected val FRAGMENT: P[Unit] = BaseParser.FRAGMENT
}

object BaseParser {

  /** User info characters from RFC 3986 section 3.2.1. */
  private val USER_INFO = CharIn(encoding.PercentEncoder.RfcCharsets.USER_INFO.toSeq)
  /** Registered name characters from RFC 3986 section 3.2.2. */
  private val REGISTERED_NAME = CharIn(encoding.PercentEncoder.RfcCharsets.REG_NAME.toSeq)
  /** Segment characters from RFC 3986 section 3.3. */
  private val SEGMENT = CharIn(encoding.PercentEncoder.RfcCharsets.SEGMENT.toSeq)
  private val SEGMENT_NO_COLONS = CharIn((encoding.PercentEncoder.RfcCharsets.SEGMENT - (':')).toSeq)
  /** Query characters from RFC 3986 section 3.4. */
  private val QUERY = CharIn(encoding.PercentEncoder.RfcCharsets.QUERY.toSeq)
  /** Fragment characters from RFC 3986 section 3.5. */
  private val FRAGMENT = CharIn(encoding.PercentEncoder.RfcCharsets.FRAGMENT.toSeq)

  private val ALPHA = CharIn('a' to 'z', 'A' to 'Z')
  private val SCHEME_AFTER_FIRST = CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "+-.")
  private val SCHEME: P[Unit] = P(ALPHA ~ SCHEME_AFTER_FIRST.rep)
  private val DIGIT = CharIn('0' to '9')
  private val DECIMAL_OCTET: P[Unit] = P(("25" ~ CharIn('0' to '5')) | ("2" ~ CharIn('0' to '4') ~ DIGIT) | ("1" ~ DIGIT ~ DIGIT) | (CharIn('1' to '9') ~ DIGIT) | DIGIT)
  private val IPV4_ADDRESS: P[Unit] = P(DECIMAL_OCTET.rep(exactly = 4, sep = "."))
  private val HEX_DIGIT = CharIn('a' to 'f', 'A' to 'F', '0' to '9')
  private val HEXTET: P[Unit] = P(HEX_DIGIT.rep(min = 1, max = 4))
  private val LS32: P[Unit] = P(HEXTET.rep(exactly = 2, sep = ":") | IPV4_ADDRESS)
  private val IPV6_ADDRESS: P[Unit] = P(               ((HEXTET ~ ":").rep(exactly = 6) ~ LS32) |
                                                ("::" ~ (HEXTET ~ ":").rep(exactly = 5) ~ LS32) |
                                     (HEXTET.? ~ "::" ~ (HEXTET ~ ":").rep(exactly = 4) ~ LS32) |
    (HEXTET.rep(min = 1, max = 2, sep = ":").? ~ "::" ~ (HEXTET ~ ":").rep(exactly = 3) ~ LS32) |
    (HEXTET.rep(min = 1, max = 3, sep = ":").? ~ "::" ~ (HEXTET ~ ":").rep(exactly = 2) ~ LS32) |
    (HEXTET.rep(min = 1, max = 4, sep = ":").? ~ "::" ~ HEXTET ~ ":" ~ LS32) |
    (HEXTET.rep(min = 1, max = 5, sep = ":").? ~ "::" ~ LS32) |
    (HEXTET.rep(min = 1, max = 6, sep = ":").? ~ "::" ~ HEXTET) |
    (HEXTET.rep(min = 1, max = 7, sep = ":").? ~ "::")
  )
  private val IPVFUTURE_CONTENT = CharIn((encoding.PercentEncoder.RfcCharsets.UNRESERVED ++ encoding.PercentEncoder.RfcCharsets.SUB_DELIMS + ':').toSeq)
  private val IPVFUTURE: P[Unit] = P(CharIn("vV") ~ HEX_DIGIT.rep(1) ~ "." ~ IPVFUTURE_CONTENT.rep(1))

  // The following are provided ONLY for `Scheme` and `Host` validation:
  private[uri] val SCHEME_SCHEME = P(SCHEME ~ End)
  private[uri] val IPV4_ADDRESS_HOST = P(IPV4_ADDRESS ~ End)
  private[uri] val IPV6_ADDRESS_HOST = P("[" ~ IPV6_ADDRESS ~ "]" ~ End)
  private[uri] val IPVFUTURE_HOST = P("[" ~ IPVFUTURE ~ "]" ~ End)
}
