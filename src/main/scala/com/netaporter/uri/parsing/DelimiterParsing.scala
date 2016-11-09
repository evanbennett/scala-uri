package com.netaporter.uri.parsing

import com.netaporter.uri._
import fastparse.all._

trait DelimiterParsing extends BaseParser {

  protected override val USER_INFO = DelimiterParsing.USER_INFO
  protected override val REGISTERED_NAME = DelimiterParsing.REGISTERED_NAME
  protected override val SEGMENT = DelimiterParsing.SEGMENT
  protected override val SEGMENT_NO_COLONS = DelimiterParsing.SEGMENT_NO_COLONS
  protected override val QUERY = DelimiterParsing.QUERY
  protected override val FRAGMENT = DelimiterParsing.FRAGMENT
}

object DelimiterParsing {

  // TODO: For some reason, I cannot get `!"???" ~ AnyChar` to work, so I have used `CharsWhile(!"`...:
  private val USER_INFO = CharsWhile(!"[@/?#".contains(_: Char)) // !"[@/?#" ~ AnyChar // THEON: I had to add '[' or this tries to parse an `ipLiteral` host:
  private val REGISTERED_NAME = CharsWhile(!":/?#".contains(_: Char)) // !":/?#" ~ AnyChar
  private val SEGMENT = CharsWhile(!"/?#".contains(_: Char)) // !"/?#" ~ AnyChar
  private val SEGMENT_NO_COLONS = CharsWhile(!":/?#".contains(_: Char)) //  !":/?#" ~ AnyChar
  private val QUERY = CharsWhile(!"#".contains(_: Char)) // !"#" ~ AnyChar
  private val FRAGMENT = AnyChar
}
