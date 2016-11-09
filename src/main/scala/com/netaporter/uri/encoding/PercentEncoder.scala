package com.netaporter.uri.encoding

case class PercentEncoder(charsToEncode: Set[Char]) extends UriEncoder {

  @deprecated("Use a tighter character set relavent to your use case instead.", "1.0.0")
  def this() { this(PercentEncoder.DEFAULT_CHARS_TO_ENCODE) }

  @deprecated("Being made `protected`.", "1.0.0")
  // THEON: This now forces '%' to be encoded, otherwise percent decoding will fail or change the value.
  def shouldEncode(char: Char): Boolean = char == '%' || !isAscii(char) || charsToEncode.contains(char)

  @deprecated("Being made `protected`.", "1.0.0")
  def encodeChar(char: Char): String = "%" + toHex(char)

  @deprecated("Being made `protected`.", "1.0.0")
  def toHex(char: Char): String = "%04x".format(char.toInt).substring(2).toUpperCase

  @deprecated("Use `isAscii` instead.", "1.0.0")
  def ascii(char: Char) = isAscii(char)

  /**
   * Determines if this character is in the allowed ASCII range (excluding control characters).
   */
  def isAscii(char: Char): Boolean = char > 31 && char < 127

  def --(chars: Char*): PercentEncoder = PercentEncoder(charsToEncode -- chars)

  def ++(chars: Char*): PercentEncoder = PercentEncoder(charsToEncode ++ chars)
}

object PercentEncoder {

  /** ASCII characters by default not encoded. */
  val UNENCODED_ASCII_CHARS = (' ' to '~').toSet

  object RfcCharsets {

    /** Percent encoded characters from RFC 3986 section 2.1. */
    val PCT_ENCODED = ('a' to 'f').toSet ++ ('A' to 'F') ++ ('0' to '9') + '%'
    /** Generic delimiter characters from RFC 3986 section 2.2. */
    val GEN_DELIMS = Set(':', '/', '?', '#', '[', ']', '@')
    /** Sub-delimiter characters from RFC 3986 section 2.2. */
    val SUB_DELIMS = Set('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=')
    /** Reserved characters from RFC 3986 section 2.2. */
    val RESERVED = GEN_DELIMS ++ SUB_DELIMS
    /** Unreserved characters from RFC 3986 section 2.3. */
    val UNRESERVED = ('a' to 'z').toSet ++ ('A' to 'Z') ++ ('0' to '9') + ('-', '.', '_', '~')
    /** User info characters from RFC 3986 section 3.2.1. */
    val USER_INFO = UNRESERVED ++ PCT_ENCODED ++ SUB_DELIMS + ':'
    /** User characters from RFC 3986 section 3.2.1. */
    val USER = USER_INFO - ':'
    /** Registered name characters from RFC 3986 section 3.2.2. */
    val REG_NAME = UNRESERVED ++ PCT_ENCODED ++ SUB_DELIMS
    /** "pchar" characters from RFC 3986 section 3.3. */
    val PCHAR = UNRESERVED ++ PCT_ENCODED ++ SUB_DELIMS + (':', '@')
    /** Segment characters from RFC 3986 section 3.3. */
    val SEGMENT = PCHAR
    /** Query characters from RFC 3986 section 3.4. */
    val QUERY = PCHAR + ('/', '?')
    /** Fragment characters from RFC 3986 section 3.5. */
    val FRAGMENT = PCHAR + ('/', '?')

    /**
     * Exclude characters from RFC 2396 section 2.4.3.
     * NOTE: "control" characters are handled by the `isAscii` method, and '%' is handled by the `shouldEncode` method.
     */
    val EXCLUDED = Set(' ', '<', '>', '#', '"', '{', '}', '|', '\\', '^', '[', ']', '`')
  }

  object CharsetsToEncode {

    val USER_INFO = UNENCODED_ASCII_CHARS -- RfcCharsets.USER_INFO
    val USER = UNENCODED_ASCII_CHARS -- RfcCharsets.USER
    val PASSWORD = UNENCODED_ASCII_CHARS -- RfcCharsets.USER_INFO
    val REGISTERED_NAME = UNENCODED_ASCII_CHARS -- RfcCharsets.REG_NAME
    val SEGMENT = UNENCODED_ASCII_CHARS -- RfcCharsets.SEGMENT
    val QUERY = UNENCODED_ASCII_CHARS -- (RfcCharsets.QUERY - ('&', '='))
    val FRAGMENT = UNENCODED_ASCII_CHARS -- RfcCharsets.FRAGMENT
  }

  @deprecated("Use `CharsetsToEncode.USER` or `.PASSWORD` instead.", "1.0.0")
  val USER_INFO_CHARS_TO_ENCODE = Set(
    ' ', '%', '<', '>', '[', ']', '#', '%', '{', '}', '^', '`', '|', '?', '@', ':', '/'
  )
  @deprecated("Use `CharsetsToEncode.SEGMENT` instead.", "1.0.0")
  val PATH_CHARS_TO_ENCODE = Set(
    ' ', '%', '<', '>', '[', ']', '#', '%', '{', '}', '^', '`', '|', '?', '/' // THEON: This needs to encode '/' otherwise converting a `Uri` to string and re-parsing will change the `Uri`.
  )
  @deprecated("Use `CharsetsToEncode.QUERY` instead.", "1.0.0")
  val QUERY_CHARS_TO_ENCODE = Set(
    ' ', '%', '<', '>', '[', ']', '#', '%', '{', '}', '^', '`', '|', '&', '\\', '+', '='
  )
  @deprecated("Use `CharsetsToEncode.FRAGMENT` instead.", "1.0.0")
  val FRAGMENT_CHARS_TO_ENCODE = Set('#')

  @deprecated("Use `RfcCharsets.GEN_DELIMS` instead.", "1.0.0")
  val GEN_DELIMS = RfcCharsets.GEN_DELIMS
  @deprecated("Use `RfcCharsets.SUB_DELIMS` instead.", "1.0.0")
  val SUB_DELIMS = RfcCharsets.SUB_DELIMS
  @deprecated("Use `RfcCharsets.RESERVED` instead.", "1.0.0")
  val RESERVED = RfcCharsets.RESERVED

  @deprecated("Use `RfcCharsets.EXCLUDED` (expanded to the full character set) instead.", "1.0.0")
  val EXCLUDED = Set('"') // RFC 2396 section 2.4.3

  /**
   * Probably more than you need to percent encode.
   * Wherever possible try to use a tighter Set of characters to encode depending on your use case.
   */
  @deprecated("Use a tighter character set relavent to your use case instead.", "1.0.0")
  val DEFAULT_CHARS_TO_ENCODE = RfcCharsets.RESERVED ++ RfcCharsets.EXCLUDED ++ PATH_CHARS_TO_ENCODE ++ QUERY_CHARS_TO_ENCODE

  @deprecated("Use other factory methods instead.", "1.0.0")
  def apply(): PercentEncoder = new PercentEncoder()

  // NOTE: Must have at least one `Char`, otherwise should use `NoopEncoder`.
  def apply(firstChar: Char, charsToEncode: Char*): PercentEncoder = apply(charsToEncode.toSet + firstChar)
}
