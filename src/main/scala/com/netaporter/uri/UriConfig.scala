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
case class UriConfig(
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
    charset: java.nio.charset.Charset = java.nio.charset.StandardCharsets.UTF_8) {

  @deprecated("Use the new constructor or a factory method.", "1.0.0")
  def this(
    userInfoEncoder: UriEncoder, pathEncoder: UriEncoder, queryEncoder: UriEncoder, fragmentEncoder: UriEncoder,
    userInfoDecoder: UriDecoder, pathDecoder: UriDecoder, queryDecoder: UriDecoder, fragmentDecoder: UriDecoder,
    matrixParams: Boolean, charset: String) {
    this(false, false, true, false, false, true, false, matrixParams, true, true,
      userInfoDecoder, userInfoDecoder, userInfoDecoder, NoopDecoder, pathDecoder, queryDecoder, fragmentDecoder,
      userInfoEncoder, userInfoEncoder, userInfoEncoder, NoopEncoder, pathEncoder, queryEncoder, fragmentEncoder,
      java.nio.charset.Charset.forName(charset))
  }

  def withEncoding(encoder: UriEncoder): UriConfig = copy(userInfoEncoder = encoder, userEncoder = encoder, passwordEncoder = encoder, registeredNameEncoder = encoder, pathEncoder = encoder, queryEncoder = encoder, fragmentEncoder = encoder)

  def withNoEncoding: UriConfig = withEncoding(NoopEncoder)

  def withDecoding(decoder: UriDecoder): UriConfig = copy(userInfoDecoder = decoder, userDecoder = decoder, passwordDecoder = decoder, registeredNameDecoder = decoder, pathDecoder = decoder, queryDecoder = decoder, fragmentDecoder = decoder)

  def withNoDecoding: UriConfig = withDecoding(NoopDecoder)
}

object UriConfig {

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
    new UriConfig(
      userInfoEncoder, pathEncoder, queryEncoder, fragmentEncoder,
      userInfoDecoder, pathDecoder, queryDecoder, fragmentDecoder,
      matrixParams, charset
    )

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
}
