package com.netaporter.uri.encoding

trait UriEncoder {

  def shouldEncode(char: Char): Boolean

  def encodeChar(char: Char): String

  def encode(s: String, charset: String): String = {
    val chars = s.getBytes(charset).map(_.toChar)
    val encodedChars = chars.flatMap { char =>
      if (shouldEncode(char)) {
        encodeChar(char).getBytes(charset)
      } else {
        Array(char.toByte)
      }
    }
    new String(encodedChars, charset)
  }

  /**
   * Prepend `encoder` to `this` as a `ChainedUriEncoder`.
   */
  def +(encoder: UriEncoder): ChainedUriEncoder = ChainedUriEncoder(Seq(encoder, this))
}
