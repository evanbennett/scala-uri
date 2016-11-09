package com.netaporter.uri.parsing

import com.netaporter.uri._
import fastparse.all._

trait MatrixParameterWithDelimiterParsing extends BaseParser {
  this: MatrixParameterParsing with DelimiterParsing =>

  protected override val SEGMENT_WITH_MATRIX_PARAMETER = MatrixParameterWithDelimiterParsing.SEGMENT_WITH_MATRIX_PARAMETER
  protected override val SEGMENT_WITH_MATRIX_PARAMETER_NO_COLONS = MatrixParameterWithDelimiterParsing.SEGMENT_WITH_MATRIX_PARAMETER_NO_COLONS
  protected override val MATRIX_PARAMETER_KEY = MatrixParameterWithDelimiterParsing.MATRIX_PARAMETER_KEY
  protected override val MATRIX_PARAMETER_VALUE = MatrixParameterWithDelimiterParsing.MATRIX_PARAMETER_VALUE
}

object MatrixParameterWithDelimiterParsing {

  // TODO: For some reason, I cannot get `!"???" ~ AnyChar` to work, so I have used `CharsWhile(!"`...:
  private val SEGMENT_WITH_MATRIX_PARAMETER = CharsWhile(!";/?#".contains(_: Char)) // !";/?#" ~ AnyChar
  private val SEGMENT_WITH_MATRIX_PARAMETER_NO_COLONS = CharsWhile(!":;/?#".contains(_: Char)) // !":;/?#" ~ AnyChar
  private val MATRIX_PARAMETER_KEY = CharsWhile(!"=;/?#".contains(_: Char)) // !"=;/?#" ~ AnyChar
  private val MATRIX_PARAMETER_VALUE = CharsWhile(!";/?#".contains(_: Char)) // !";/?#" ~ AnyChar
}
