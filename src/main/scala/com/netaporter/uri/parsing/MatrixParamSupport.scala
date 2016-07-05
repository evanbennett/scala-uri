package com.netaporter.uri.parsing

import com.netaporter.uri._
import MatrixParamSupport._
import org.parboiled2._

trait MatrixParamSupport {
  self: UriParser =>

  protected def _matrixParam: Rule1[Parameter] = rule {
    capture(oneOrMore(MATRIX_PARAMETER_KEY)) ~ optional("=" ~ capture(zeroOrMore(MATRIX_PARAMETER_VALUE))) ~> extractParam
  }

  protected override def _segment: Rule1[Segment] = rule {
    capture(zeroOrMore(SEGMENT_WITH_MATRIX_PARAMETER)) ~ zeroOrMore(";") ~ zeroOrMore(_matrixParam).separatedBy(oneOrMore(";")) ~ zeroOrMore(";") ~> extractSegmentWithMatrixParams
  }

  protected override def _segmentNz: Rule1[Segment] = rule {
    capture(oneOrMore(SEGMENT_WITH_MATRIX_PARAMETER)) ~ zeroOrMore(";") ~ zeroOrMore(_matrixParam).separatedBy(oneOrMore(";")) ~ zeroOrMore(";") ~> extractSegmentWithMatrixParams
  }

  protected override def _segmentNzNc: Rule1[Segment] = rule {
    capture(oneOrMore(SEGMENT_WITH_MATRIX_PARAMETER -- ':')) ~ zeroOrMore(";") ~ zeroOrMore(_matrixParam).separatedBy(oneOrMore(";")) ~ zeroOrMore(";") ~> extractSegmentWithMatrixParams
  }

  protected val extractSegmentWithMatrixParams: (String, Seq[Parameter]) => Segment = (segment: String, matrixParams: Seq[Parameter]) => {
    val decodedSegment = c.pathDecoder.decode(segment, originalInput)
    val decodedMatrixParams = matrixParams.map(c.pathDecoder.decodeParameter(_, originalInput))
    Segment(decodedSegment, decodedMatrixParams.toVector)
  }
}

object MatrixParamSupport {

  val SEGMENT_WITH_MATRIX_PARAMETER = UriParser.SEGMENT -- ";"
  val MATRIX_PARAMETER_KEY = SEGMENT_WITH_MATRIX_PARAMETER -- "="
  val MATRIX_PARAMETER_VALUE = SEGMENT_WITH_MATRIX_PARAMETER
}
