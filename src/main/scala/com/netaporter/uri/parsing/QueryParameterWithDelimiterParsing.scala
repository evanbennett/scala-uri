package com.netaporter.uri.parsing

import com.netaporter.uri._
import fastparse.all._

trait QueryParameterWithDelimiterParsing extends BaseParser {
  this: QueryParameterParsing with DelimiterParsing =>

  protected override val QUERY_PARAMETER_KEY = QueryParameterWithDelimiterParsing.QUERY_PARAMETER_KEY
  protected override val QUERY_PARAMETER_VALUE = QueryParameterWithDelimiterParsing.QUERY_PARAMETER_VALUE
}

object QueryParameterWithDelimiterParsing {

  // TODO: For some reason, I cannot get `!"???" ~ AnyChar` to work, so I have used `CharsWhile(!"`...:
  private val QUERY_PARAMETER_KEY = CharsWhile(!"=&#".contains(_: Char)) // !"=&#" ~ AnyChar
  private val QUERY_PARAMETER_VALUE = CharsWhile(!"&#".contains(_: Char)) // !"&#" ~ AnyChar
}
