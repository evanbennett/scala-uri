package com.netaporter.uri.parsing

import com.netaporter.uri._
import fastparse.all._

trait UserPasswordWithDelimiterParsing extends BaseParser {
  this: UserPasswordParsing with DelimiterParsing =>

  protected override val USER_INFO = UserPasswordWithDelimiterParsing.USER_INFO
  protected override val USER = UserPasswordWithDelimiterParsing.USER
}

object UserPasswordWithDelimiterParsing {

  // TODO: For some reason, I cannot get `!"???" ~ AnyChar` to work, so I have used `CharsWhile(!"`...:
  private val USER_INFO = CharsWhile(!"@/?#".contains(_: Char)) // !"@/?#" ~ AnyChar
  /** User characters from RFC 3986 section 3.2.1. */
  private val USER = CharsWhile(!"[:@/?#".contains(_: Char)) // !"[:@/?#" ~ AnyChar // THEON: I had to add '[' or this tries to parse an `ipLiteral` host:
}
