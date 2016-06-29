package com.netaporter.uri.encoding

object NoopEncoder extends UriEncoder {

  def shouldEncode(char: Char): Boolean = false

  def encodeChar(char: Char): String = char.toString
}
