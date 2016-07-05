package com.netaporter.uri.parsing

import com.netaporter.uri._
import org.parboiled2._

trait MatrixParamSupportWithDelimiterParsing extends MatrixParamSupport {
  self: UriParser =>

  protected override def _matrixParam: Rule1[Parameter] = rule {
    capture(oneOrMore(noneOf("=;/?#"))) ~ optional("=" ~ capture(zeroOrMore(noneOf(";/?#")))) ~> extractParam
  }

  protected override def _segment: Rule1[Segment] = rule {
    capture(zeroOrMore(noneOf(";/?#"))) ~ zeroOrMore(";") ~ zeroOrMore(_matrixParam).separatedBy(oneOrMore(";")) ~ zeroOrMore(";") ~> extractSegmentWithMatrixParams
  }

  protected override def _segmentNz: Rule1[Segment] = rule {
    capture(oneOrMore(noneOf(";/?#"))) ~ zeroOrMore(";") ~ zeroOrMore(_matrixParam).separatedBy(oneOrMore(";")) ~ zeroOrMore(";") ~> extractSegmentWithMatrixParams
  }

  /** The matrix parameters allow ':' which they probably should not. */
  protected override def _segmentNzNc: Rule1[Segment] = rule {
    capture(oneOrMore(noneOf(":;/?#"))) ~ zeroOrMore(";") ~ zeroOrMore(_matrixParam).separatedBy(oneOrMore(";")) ~ zeroOrMore(";") ~> extractSegmentWithMatrixParams
  }
}
