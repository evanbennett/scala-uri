package com.netaporter.uri.encoding

import PercentEncoder._

// TODO: I think this should force '%' to be encoded, otherwise percent decoding will fail or change the value.
case class PercentEncoder(charsToEncode: Set[Char] = DEFAULT_CHARS_TO_ENCODE) extends UriEncoder {

  def shouldEncode(char: Char): Boolean = !isAscii(char) || charsToEncode.contains(char)

  def encodeChar(char: Char): String = "%" + toHex(char)

  def toHex(char: Char): String = "%04x".format(char.toInt).substring(2).toUpperCase

  @deprecated("Use `isAscii` instead.", "1.0.0")
  def ascii(char: Char) = isAscii(char)

  /**
   * Determines if this character is in the ASCII range (excluding control characters)
   */
  // TODO: Forcing '%' to be encoded could be done here:
  def isAscii(char: Char): Boolean = char > 31 && char < 127

  def --(chars: Char*): PercentEncoder = PercentEncoder(charsToEncode -- chars)

  def ++(chars: Char*): PercentEncoder = PercentEncoder(charsToEncode ++ chars)
}

object PercentEncoder {

  val USER_INFO_CHARS_TO_ENCODE = Set (
    ' ', '%', '<', '>', '[', ']', '#', '{', '}', '^', '`', '|', '?', '@', ':', '/'
  )

  val PATH_CHARS_TO_ENCODE = Set (
    ' ', '%', '<', '>', '[', ']', '#', '{', '}', '^', '`', '|', '?'
  )

  val QUERY_CHARS_TO_ENCODE = Set (
    ' ', '%', '<', '>', '[', ']', '#', '{', '}', '^', '`', '|', '&', '\\', '+', '='
  )

  val FRAGMENT_CHARS_TO_ENCODE = Set('%', '#') // TODO: This needs to encode '%' otherwise percent decoding will fail or change the value.

  val GEN_DELIMS = Set(':', '/', '?',  '#', '[', ']', '@')
  val SUB_DELIMS  = Set('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=')
  val RESERVED = GEN_DELIMS ++ SUB_DELIMS

  val EXCLUDED = Set('"') // RFC 2396 section 2.4.3

  /**
   * Probably more than you need to percent encode. Wherever possible try to use a tighter Set of characters
   * to encode depending on your use case
   */
  val DEFAULT_CHARS_TO_ENCODE = RESERVED ++ PATH_CHARS_TO_ENCODE ++ QUERY_CHARS_TO_ENCODE ++ EXCLUDED

  // NOTE: Must have at least one Char so that the default value `DEFAULT_CHARS_TO_ENCODE` can be used when none are provided.
  def apply(firstChar: Char, chars: Char*): PercentEncoder = apply(chars.toSet + firstChar)

  val default = apply()
}
