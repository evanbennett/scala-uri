package com.netaporter.uri.config

import com.netaporter.uri.encoding._
import com.netaporter.uri.decoding._
import PercentEncoder._

case class UriConfig(userInfoEncoder: UriEncoder,
                     hostEncoder: UriEncoder,
                     pathEncoder: UriEncoder,
                     queryEncoder: UriEncoder,
                     fragmentEncoder: UriEncoder,
                     userInfoDecoder: UriDecoder,
                     hostDecoder: UriDecoder,
                     pathDecoder: UriDecoder,
                     queryDecoder: UriDecoder,
                     fragmentDecoder: UriDecoder,
                     delimiterParsing: Boolean,
                     matrixParams: Boolean,
                     charset: String) {

  def withNoEncoding = copy(userInfoEncoder = NoopEncoder, hostEncoder = NoopEncoder, pathEncoder = NoopEncoder, queryEncoder = NoopEncoder, fragmentEncoder = NoopEncoder)

  // TODO: Add `withNoDecoding` method:
  //def withNoDecoding = copy(userInfoDecoder = NoopDecoder, hostDecoder = NoopDecoder, pathDecoder = NoopDecoder, queryDecoder = NoopDecoder, fragmentDecoder = NoopDecoder)
}

object UriConfig {

  val DEFAULT = apply(userInfoEncoder = PercentEncoder(USER_INFO_CHARS_TO_ENCODE),
                      hostEncoder = PercentEncoder(HOST_CHARS_TO_ENCODE),
                      pathEncoder = PercentEncoder(PATH_CHARS_TO_ENCODE),
                      queryEncoder = PercentEncoder(QUERY_CHARS_TO_ENCODE),
                      fragmentEncoder = PercentEncoder(FRAGMENT_CHARS_TO_ENCODE),
                      userInfoDecoder = PercentDecoder,
                      hostDecoder = PercentDecoder,
                      pathDecoder = PercentDecoder,
                      queryDecoder = PercentDecoder,
                      fragmentDecoder = PercentDecoder,
                      delimiterParsing = false,
                      matrixParams = false,
                      charset = "UTF-8")

  /**
   * Probably more than you need to percent encode. Wherever possible try to use a tighter Set of characters
   * to encode depending on your use case
   */
  val CONSERVATIVE = apply(PercentEncoder.DEFAULT, PercentDecoder)

  def apply(encoder: UriEncoder = PercentEncoder.DEFAULT,
            decoder: UriDecoder = PercentDecoder,
            delimiterParsing: Boolean = false,
            matrixParams: Boolean = false,
            charset: String = "UTF-8"): UriConfig =
    apply(encoder, encoder, encoder, encoder, encoder, decoder, decoder, decoder, decoder, decoder, delimiterParsing, matrixParams, charset)

  @deprecated("Use `DEFAULT` instead.", "1.0.0")
  val default = DEFAULT

  /**
   * Probably more than you need to percent encode. Wherever possible try to use a tighter Set of characters
   * to encode depending on your use case
   */
  @deprecated("Use `CONSERVATIVE` instead.", "1.0.0")
  val conservative = CONSERVATIVE
}
