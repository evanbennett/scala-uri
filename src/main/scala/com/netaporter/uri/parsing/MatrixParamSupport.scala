package com.netaporter.uri.parsing

import com.netaporter.uri._
import org.parboiled2._

trait MatrixParamSupport {
  this: UriParser =>

  def _matrixParam: Rule1[Parameter] = rule {
    capture(oneOrMore(!anyOf("=;/?#") ~ ANY)) ~ optional("=" ~ capture(zeroOrMore(!anyOf(";/?#") ~ ANY))) ~> extractParam
  }

  override def _segment: Rule1[Segment] = rule {
    capture(zeroOrMore(!anyOf(";/?#") ~ ANY)) ~ zeroOrMore(";") ~ zeroOrMore(_matrixParam).separatedBy(oneOrMore(";")) ~ zeroOrMore(";") ~> extractSegmentWithMatrixParams
  }

  override def _segmentNz: Rule1[Segment] = rule {
    capture(oneOrMore(!anyOf(";/?#") ~ ANY)) ~ zeroOrMore(";") ~ zeroOrMore(_matrixParam).separatedBy(oneOrMore(";")) ~ zeroOrMore(";") ~> extractSegmentWithMatrixParams
  }

  /** The matrix parameters allow ':' which they probably should not. */
  override def _segmentNzNc: Rule1[Segment] = rule {
    capture(oneOrMore(!anyOf(":;/?#") ~ ANY)) ~ zeroOrMore(";") ~ zeroOrMore(_matrixParam).separatedBy(oneOrMore(";")) ~ zeroOrMore(";") ~> extractSegmentWithMatrixParams
  }

  val extractSegmentWithMatrixParams: (String, Seq[Parameter]) => Segment = (segment: String, matrixParams: Seq[Parameter]) => {
    val decodedSegment = c.pathDecoder.decode(segment, originalInput)
    val decodedMatrixParams = matrixParams.map(c.pathDecoder.decodeParameter(_, originalInput))
    Segment(decodedSegment, decodedMatrixParams.toVector)
  }
}
