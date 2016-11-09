package com.netaporter.uri.encoding

case class EncodeCharAs(char: Char, asString: String) extends UriEncoder {

  @deprecated("Being made `protected`.", "1.0.0")
  def shouldEncode(char: Char): Boolean = char == this.char

  @deprecated("Being made `protected`.", "1.0.0")
  def encodeChar(char: Char): String = asString
}

object EncodeCharAs {

  val SPACE_AS_PLUS = EncodeCharAs(' ', "+")
}
