package com.netaporter.uri.encoding

trait UriEncoder {

  @deprecated("Being made `protected`.", "1.0.0")
  def shouldEncode(char: Char): Boolean

  @deprecated("Being made `protected`.", "1.0.0")
  def encodeChar(char: Char): String

  @deprecated("Use other `encode` instead.", "1.0.0")
  def encode(s: String, charset: String): String = encode(s)(com.netaporter.uri.UriConfig.default.copy(charset = java.nio.charset.Charset.forName(charset)))

  def encode(s: String)(implicit config: com.netaporter.uri.UriConfig): String = {
    if (!config.percentEncodingNormalization) s
    else {
      val chars = s.getBytes(config.charset).map(_.toChar)
      val encodedChars = chars.flatMap { char =>
        if (shouldEncode(char)) {
          encodeChar(char).getBytes(config.charset)
        } else {
          Array(char.toByte)
        }
      }
      new String(encodedChars, config.charset)
    }
  }

  /**
   * Prepend `encoder` to `this` as a `ChainedUriEncoder`.
   */
  def +(encoder: UriEncoder): ChainedUriEncoder = ChainedUriEncoder(Seq(encoder, this))
}
