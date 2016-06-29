package com.netaporter.uri.encoding

case class EncodeCharAs(char: Char, asString: String) extends UriEncoder {

  def shouldEncode(char: Char): Boolean = char == this.char

  def encodeChar(char: Char): String = asString
}

object EncodeCharAs {

  val spaceAsPlus = EncodeCharAs(' ', "+")
}
