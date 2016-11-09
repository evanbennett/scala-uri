package com.netaporter.uri.encoding

object NoopEncoder extends UriEncoder {

  @deprecated("Being made `protected`.", "1.0.0")
  def shouldEncode(char: Char): Boolean = false

  @deprecated("Being made `protected`.", "1.0.0")
  def encodeChar(char: Char): String = throw new UnsupportedOperationException
}
