package com.netaporter.uri

import com.netaporter.uri.decoding._
import com.netaporter.uri.encoding._
import PercentEncoder.{ CharsetsToEncode, RfcCharsets }

/**
 * Configuration for parsing and `toString` methods.
 *
 * The `*Decoder`s and `*Encoder`s configure the percent-encoding normalization.
 *
 * @param emptyComponentNormalization whether to remove empty authority components, based on RFC 3986 section 3.2
 * @param caseNormalization whether to perform case normalization, based on RFC 3986 section 6.2.2.1 (ensure scheme and host are lower case)
 * @param percentEncodingNormalization whether to perform percent (decoding and) encoding normalization, based on RFC 3986 section 6.2.2.2.
 * @param pathSegmentNormalization whether to perform path segment normalization, based on RFC 3986 section 6.2.2.3 (remove dot segments in non-relative paths)
 * @param delimiterParsing whether to parse with RFC compliance or using delimiters
 * @param userPasswordParsing whether to parse the user info into user and password
 * @param registeredNameMustBeDomainName whether a registered name must match DNS specification (in `Host` and parsing)
 * @param matrixParameterParsing whether to parse matrix parameters
 * @param queryParameterParsing whether to parse query parameters
 * @param fragmentAllowHashParsing whether a fragment can contain a '#' during parsing
 * @param userInfoDecoder the decoder to use on the user info
 * @param userDecoder the decoder to use on the user
 * @param passwordDecoder the decoder to use on the password
 * @param registeredNameDecoder the decoder to use on the registered name
 * @param pathDecoder the decoder to use on the path
 * @param queryDecoder the decoder to use on the query
 * @param fragmentDecoder the decoder to use on the fragment
 * @param userInfoEncoder the encoder to use on the user info
 * @param userEncoder the encoder to use on the user
 * @param passwordEncoder the encoder to use on the password
 * @param registeredNameEncoder the encoder to use on the registered name
 * @param pathEncoder the encoder to use on the path
 * @param queryEncoder the encoder to use on the query
 * @param fragmentEncoder the encoder to use on the fragment
 * @param charset the character set to use when decoding, encoding, etc.
 */
// TODO: When Scala 2.10 support is dropped, this can be changed to a `sealed abstract case class`.
sealed class UriConfig(
    val emptyComponentNormalization: Boolean = true,
    val caseNormalization: Boolean = true,
    val percentEncodingNormalization: Boolean = true,
    val pathSegmentNormalization: Boolean = true,
    val delimiterParsing: Boolean = false,
    val userPasswordParsing: Boolean = false,
    val registeredNameMustBeDomainName: Boolean = true,
    val matrixParameterParsing: Boolean = false,
    val queryParameterParsing: Boolean = true,
    val fragmentAllowHashParsing: Boolean = true,
    val userInfoDecoder: UriDecoder = PercentDecoder,
    val userDecoder: UriDecoder = PercentDecoder,
    val passwordDecoder: UriDecoder = PercentDecoder,
    val registeredNameDecoder: UriDecoder = PercentDecoder,
    val pathDecoder: UriDecoder = PercentDecoder,
    val queryDecoder: UriDecoder = PercentDecoder,
    val fragmentDecoder: UriDecoder = PercentDecoder,
    val userInfoEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.USER_INFO),
    val userEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.USER),
    val passwordEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.PASSWORD),
    val registeredNameEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.REGISTERED_NAME),
    val pathEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.SEGMENT),
    val queryEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.QUERY),
    val fragmentEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.FRAGMENT - '#'),
    val charset: java.nio.charset.Charset = java.nio.charset.StandardCharsets.UTF_8) {

  def withEncoding(encoder: UriEncoder): UriConfig = copy(userInfoEncoder = encoder, userEncoder = encoder, passwordEncoder = encoder, registeredNameEncoder = encoder, pathEncoder = encoder, queryEncoder = encoder, fragmentEncoder = encoder)

  def withNoEncoding: UriConfig = withEncoding(NoopEncoder)

  def withDecoding(decoder: UriDecoder): UriConfig = copy(userInfoDecoder = decoder, userDecoder = decoder, passwordDecoder = decoder, registeredNameDecoder = decoder, pathDecoder = decoder, queryDecoder = decoder, fragmentDecoder = decoder)

  def withNoDecoding: UriConfig = withDecoding(NoopDecoder)

  def copy(
    emptyComponentNormalization: Boolean = this.emptyComponentNormalization,
    caseNormalization: Boolean = this.caseNormalization,
    percentEncodingNormalization: Boolean = this.percentEncodingNormalization,
    pathSegmentNormalization: Boolean = this.pathSegmentNormalization,
    delimiterParsing: Boolean = this.delimiterParsing,
    userPasswordParsing: Boolean = this.userPasswordParsing,
    registeredNameMustBeDomainName: Boolean = this.registeredNameMustBeDomainName,
    matrixParameterParsing: Boolean = this.matrixParameterParsing,
    queryParameterParsing: Boolean = this.queryParameterParsing,
    fragmentAllowHashParsing: Boolean = this.fragmentAllowHashParsing,
    userInfoDecoder: UriDecoder = this.userInfoDecoder,
    userDecoder: UriDecoder = this.userDecoder,
    passwordDecoder: UriDecoder = this.passwordDecoder,
    registeredNameDecoder: UriDecoder = this.registeredNameDecoder,
    pathDecoder: UriDecoder = this.pathDecoder,
    queryDecoder: UriDecoder = this.queryDecoder,
    fragmentDecoder: UriDecoder = this.fragmentDecoder,
    userInfoEncoder: UriEncoder = this.userInfoEncoder,
    userEncoder: UriEncoder = this.userEncoder,
    passwordEncoder: UriEncoder = this.passwordEncoder,
    registeredNameEncoder: UriEncoder = this.registeredNameEncoder,
    pathEncoder: UriEncoder = this.pathEncoder,
    queryEncoder: UriEncoder = this.queryEncoder,
    fragmentEncoder: UriEncoder = this.fragmentEncoder,
    charset: java.nio.charset.Charset = this.charset): UriConfig =
    UriConfig(
      emptyComponentNormalization, caseNormalization, percentEncodingNormalization, pathSegmentNormalization,
      delimiterParsing, userPasswordParsing, registeredNameMustBeDomainName, matrixParameterParsing, queryParameterParsing, fragmentAllowHashParsing,
      userInfoDecoder, userDecoder, passwordDecoder, registeredNameDecoder, pathDecoder, queryDecoder, fragmentDecoder,
      userInfoEncoder, userEncoder, passwordEncoder, registeredNameEncoder, pathEncoder, queryEncoder, fragmentEncoder, charset)

  override def equals(a: Any): Boolean = {
    a match {
      case null => false
      case c: UriConfig =>
        emptyComponentNormalization == c.emptyComponentNormalization &&
        caseNormalization == c.caseNormalization &&
        percentEncodingNormalization == c.percentEncodingNormalization &&
        pathSegmentNormalization == c.pathSegmentNormalization &&
        delimiterParsing == c.delimiterParsing &&
        userPasswordParsing == c.userPasswordParsing &&
        registeredNameMustBeDomainName == c.registeredNameMustBeDomainName &&
        matrixParameterParsing == c.matrixParameterParsing &&
        queryParameterParsing == c.queryParameterParsing &&
        fragmentAllowHashParsing == c.fragmentAllowHashParsing &&
        userInfoDecoder == c.userInfoDecoder &&
        userDecoder == c.userDecoder &&
        passwordDecoder == c.passwordDecoder &&
        registeredNameDecoder == c.registeredNameDecoder &&
        pathDecoder == c.pathDecoder &&
        queryDecoder == c.queryDecoder &&
        fragmentDecoder == c.fragmentDecoder &&
        userInfoEncoder == c.userInfoEncoder &&
        userEncoder == c.userEncoder &&
        passwordEncoder == c.passwordEncoder &&
        registeredNameEncoder == c.registeredNameEncoder &&
        pathEncoder == c.pathEncoder &&
        queryEncoder == c.queryEncoder &&
        fragmentEncoder == c.fragmentEncoder &&
        charset == c.charset
      case _ => false
    }
  }

  override def hashCode: Int = ((((((((((((((((((((((((
    41 + emptyComponentNormalization.hashCode) *
    41 + caseNormalization.hashCode) *
    41 + percentEncodingNormalization.hashCode) *
    41 + pathSegmentNormalization.hashCode) *
    41 + delimiterParsing.hashCode) *
    41 + userPasswordParsing.hashCode) *
    41 + registeredNameMustBeDomainName.hashCode) *
    41 + matrixParameterParsing.hashCode) *
    41 + queryParameterParsing.hashCode) *
    41 + fragmentAllowHashParsing.hashCode) *
    41 + userInfoDecoder.hashCode) *
    41 + userDecoder.hashCode) *
    41 + passwordDecoder.hashCode) *
    41 + registeredNameDecoder.hashCode) *
    41 + pathDecoder.hashCode) *
    41 + queryDecoder.hashCode) *
    41 + fragmentDecoder.hashCode) *
    41 + userInfoEncoder.hashCode) *
    41 + userEncoder.hashCode) *
    41 + passwordEncoder.hashCode) *
    41 + registeredNameEncoder.hashCode) *
    41 + pathEncoder.hashCode) *
    41 + queryEncoder.hashCode) *
    41 + fragmentEncoder.hashCode) *
    41 + charset.hashCode

  override def toString = "UriConfig(" +
    emptyComponentNormalization + ";" + caseNormalization + ";" + percentEncodingNormalization + ";" + pathSegmentNormalization + ";" +
    delimiterParsing + ";" + userPasswordParsing + ";" + registeredNameMustBeDomainName + ";" + matrixParameterParsing + ";" + queryParameterParsing + ";" + fragmentAllowHashParsing + ";" +
    userInfoDecoder + ";" + userDecoder + ";" + passwordDecoder + ";" + registeredNameDecoder + ";" + pathDecoder + ";" + queryDecoder + ";" + fragmentDecoder +
    userInfoEncoder + ";" + userEncoder + ";" + passwordEncoder + ";" + registeredNameEncoder + ";" + pathEncoder + ";" + queryEncoder + ";" + fragmentEncoder + ";" + charset + ")"
}

object UriConfig {

  def apply(
    emptyComponentNormalization: Boolean = true,
    caseNormalization: Boolean = true,
    percentEncodingNormalization: Boolean = true,
    pathSegmentNormalization: Boolean = true,
    delimiterParsing: Boolean = false,
    userPasswordParsing: Boolean = false,
    registeredNameMustBeDomainName: Boolean = true,
    matrixParameterParsing: Boolean = false,
    queryParameterParsing: Boolean = true,
    fragmentAllowHashParsing: Boolean = true,
    userInfoDecoder: UriDecoder = PercentDecoder,
    userDecoder: UriDecoder = PercentDecoder,
    passwordDecoder: UriDecoder = PercentDecoder,
    registeredNameDecoder: UriDecoder = PercentDecoder,
    pathDecoder: UriDecoder = PercentDecoder,
    queryDecoder: UriDecoder = PercentDecoder,
    fragmentDecoder: UriDecoder = PercentDecoder,
    userInfoEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.USER_INFO),
    userEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.USER),
    passwordEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.PASSWORD),
    registeredNameEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.REGISTERED_NAME),
    pathEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.SEGMENT),
    queryEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.QUERY),
    fragmentEncoder: UriEncoder = PercentEncoder(CharsetsToEncode.FRAGMENT - '#'),
    charset: java.nio.charset.Charset = java.nio.charset.StandardCharsets.UTF_8): UriConfig = {
    if (userInfoDecoder == null) throw new IllegalArgumentException("`userInfoDecoder` cannot be `null`.")
    if (userDecoder == null) throw new IllegalArgumentException("`userDecoder` cannot be `null`.")
    if (passwordDecoder == null) throw new IllegalArgumentException("`passwordDecoder` cannot be `null`.")
    if (registeredNameDecoder == null) throw new IllegalArgumentException("`registeredNameDecoder` cannot be `null`.")
    if (pathDecoder == null) throw new IllegalArgumentException("`pathDecoder` cannot be `null`.")
    if (queryDecoder == null) throw new IllegalArgumentException("`queryDecoder` cannot be `null`.")
    if (fragmentDecoder == null) throw new IllegalArgumentException("`fragmentDecoder` cannot be `null`.")
    if (userInfoEncoder == null) throw new IllegalArgumentException("`userInfoEncoder` cannot be `null`.")
    if (userEncoder == null) throw new IllegalArgumentException("`userEncoder` cannot be `null`.")
    if (passwordEncoder == null) throw new IllegalArgumentException("`passwordEncoder` cannot be `null`.")
    if (registeredNameEncoder == null) throw new IllegalArgumentException("`registeredNameEncoder` cannot be `null`.")
    if (pathEncoder == null) throw new IllegalArgumentException("`pathEncoder` cannot be `null`.")
    if (queryEncoder == null) throw new IllegalArgumentException("`queryEncoder` cannot be `null`.")
    if (fragmentEncoder == null) throw new IllegalArgumentException("`fragmentEncoder` cannot be `null`.")
    if (charset == null) throw new IllegalArgumentException("`charset` cannot be `null`.")
    new UriConfig(
      emptyComponentNormalization, caseNormalization, percentEncodingNormalization, pathSegmentNormalization,
      delimiterParsing, userPasswordParsing, registeredNameMustBeDomainName, matrixParameterParsing, queryParameterParsing, fragmentAllowHashParsing,
      userInfoDecoder, userDecoder, passwordDecoder, registeredNameDecoder, pathDecoder, queryDecoder, fragmentDecoder,
      userInfoEncoder, userEncoder, passwordEncoder, registeredNameEncoder, pathEncoder, queryEncoder, fragmentEncoder, charset)
  }

  /**
   * The default configuration, which follows RFC 3986 with the following exceptions:
   *  * a registered name must be a domain name
   *  * the query string is parsed into parameters
   *  * the fragment may contain '#'s
   */
  implicit val DEFAULT = apply()

  /** A strict RFC 3986 configuration. */
  val RFC3986 = apply(fragmentEncoder = PercentEncoder(CharsetsToEncode.FRAGMENT), registeredNameMustBeDomainName = false, queryParameterParsing = false, fragmentAllowHashParsing = false)

  /** A conservation configuration, which follows RFC 3986 excpet with much more conservative encoding. */
  val CONSERVATIVE = RFC3986.withEncoding(PercentEncoder(RfcCharsets.RESERVED ++ RfcCharsets.EXCLUDED ++ CharsetsToEncode.USER ++ CharsetsToEncode.PASSWORD ++ CharsetsToEncode.REGISTERED_NAME ++ CharsetsToEncode.SEGMENT ++ CharsetsToEncode.QUERY ++ CharsetsToEncode.FRAGMENT))

  @deprecated("Use `DEFAULT` or create the required `UriConfig` instead.", "1.0.0")
  val default = apply(
    emptyComponentNormalization = false,
    caseNormalization = false,
    pathSegmentNormalization = false,
    userPasswordParsing = true,
    registeredNameDecoder = NoopDecoder,
    userEncoder = PercentEncoder(PercentEncoder.USER_INFO_CHARS_TO_ENCODE),
    passwordEncoder = PercentEncoder(PercentEncoder.USER_INFO_CHARS_TO_ENCODE),
    registeredNameEncoder = NoopEncoder,
    pathEncoder = PercentEncoder(PercentEncoder.PATH_CHARS_TO_ENCODE),
    queryEncoder = PercentEncoder(PercentEncoder.QUERY_CHARS_TO_ENCODE),
    fragmentEncoder = PercentEncoder(PercentEncoder.FRAGMENT_CHARS_TO_ENCODE)
  )

  /**
   * Probably more than you need to percent encode. Wherever possible try to use a tighter Set of characters
   * to encode depending on your use case.
   */
  @deprecated("Use `CONSERVATIVE` instead.", "1.0.0")
  val conservative = UriConfig(PercentEncoder(), PercentDecoder)

  @deprecated("Use other `apply` instead.", "1.0.0")
  def apply(
    userInfoEncoder: UriEncoder,
    pathEncoder: UriEncoder,
    queryEncoder: UriEncoder,
    fragmentEncoder: UriEncoder,
    userInfoDecoder: UriDecoder,
    pathDecoder: UriDecoder,
    queryDecoder: UriDecoder,
    fragmentDecoder: UriDecoder,
    matrixParams: Boolean,
    charset: String): UriConfig =
    apply(false, false, true, false, false, true, false, matrixParams, true, true,
      userInfoDecoder, userInfoDecoder, userInfoDecoder, NoopDecoder, pathDecoder, queryDecoder, fragmentDecoder,
      userInfoEncoder, userInfoEncoder, userInfoEncoder, NoopEncoder, pathEncoder, queryEncoder, fragmentEncoder,
      java.nio.charset.Charset.forName(charset))

  @deprecated("Use other `apply` instead.", "1.0.0")
  def apply(encoder: UriEncoder, decoder: UriDecoder, matrixParams: Boolean, charset: String): UriConfig =
    apply(encoder, encoder, encoder, encoder, decoder, decoder, decoder, decoder, matrixParams, charset)

  @deprecated("Use other `apply` instead.", "1.0.0")
  def apply(encoder: UriEncoder, decoder: UriDecoder, matrixParams: Boolean): UriConfig =
    apply(encoder, encoder, encoder, encoder, decoder, decoder, decoder, decoder, matrixParams, "UTF-8")

  @deprecated("Use other `apply` instead.", "1.0.0")
  def apply(encoder: UriEncoder, decoder: UriDecoder): UriConfig =
    apply(encoder, encoder, encoder, encoder, decoder, decoder, decoder, decoder, false, "UTF-8")

  @deprecated("Use other `apply` instead.", "1.0.0")
  def apply(encoder: UriEncoder): UriConfig =
    apply(encoder, encoder, encoder, encoder, PercentDecoder, PercentDecoder, PercentDecoder, PercentDecoder, false, "UTF-8")

  @deprecated("Use other `apply` instead.", "1.0.0")
  def apply(decoder: UriDecoder, matrixParams: Boolean, charset: String): UriConfig =
    apply(PercentEncoder(), PercentEncoder(), PercentEncoder(), PercentEncoder(), decoder, decoder, decoder, decoder, matrixParams, charset)

  @deprecated("Use other `apply` instead.", "1.0.0")
  def apply(decoder: UriDecoder, matrixParams: Boolean): UriConfig =
    apply(PercentEncoder(), PercentEncoder(), PercentEncoder(), PercentEncoder(), decoder, decoder, decoder, decoder, matrixParams, "UTF-8")

  @deprecated("Use other `apply` instead.", "1.0.0")
  def apply(decoder: UriDecoder): UriConfig =
    apply(PercentEncoder(), PercentEncoder(), PercentEncoder(), PercentEncoder(), decoder, decoder, decoder, decoder, false, "UTF-8")

  @deprecated("Use other `apply` instead.", "1.0.0")
  def apply(matrixParams: Boolean, charset: String): UriConfig =
    apply(PercentEncoder(), PercentEncoder(), PercentEncoder(), PercentEncoder(), PercentDecoder, PercentDecoder, PercentDecoder, PercentDecoder, matrixParams, charset)

  @deprecated("Use other `apply` instead.", "1.0.0")
  def apply(matrixParams: Boolean): UriConfig =
    apply(PercentEncoder(), PercentEncoder(), PercentEncoder(), PercentEncoder(), PercentDecoder, PercentDecoder, PercentDecoder, PercentDecoder, matrixParams, "UTF-8")

  @deprecated("Use other `apply` instead.", "1.0.0")
  def apply(charset: String): UriConfig =
    apply(PercentEncoder(), PercentEncoder(), PercentEncoder(), PercentEncoder(), PercentDecoder, PercentDecoder, PercentDecoder, PercentDecoder, false, charset)

  @deprecated("`unapply` no longer available.", "1.0.0")
  def unapply(uriConfig: UriConfig): Option[(UriEncoder, UriEncoder, UriEncoder, UriEncoder, UriDecoder, UriDecoder, UriDecoder, UriDecoder, Boolean, String)] = {
    if (uriConfig == null) None else Some((
      uriConfig.userInfoEncoder, uriConfig.pathEncoder, uriConfig.queryEncoder, uriConfig.fragmentEncoder,
      uriConfig.userInfoDecoder, uriConfig.pathDecoder, uriConfig.queryDecoder, uriConfig.fragmentDecoder,
      uriConfig.matrixParameterParsing, uriConfig.charset.name
    ))
  }
}
