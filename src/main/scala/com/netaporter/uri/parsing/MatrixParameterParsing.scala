package com.netaporter.uri.parsing

import com.netaporter.uri._
import fastparse.all._

trait MatrixParameterParsing extends BaseParser {

  protected val extractSegmentWithMatrixParameters: ((String, Seq[Parameter])) => Segment = { case (segment, matrixParameters) =>
    val decodedSegment = config.pathDecoder.decode(segment, input)
    val decodedMatrixParameters = matrixParameters.map(config.pathDecoder.decodeParameter(_, input))
    Segment(decodedSegment, decodedMatrixParameters)
  }

  protected val matrixParameter: P[Parameter] =
    P(MATRIX_PARAMETER_KEY.rep(1).! ~ ("=" ~ MATRIX_PARAMETER_VALUE.rep.!).? map extractParameter)

  protected override val segment: P[Segment] =
    P(SEGMENT_WITH_MATRIX_PARAMETER.rep.! ~ ";".rep ~ matrixParameter.rep(sep = ";".rep(1)) ~ ";".rep map extractSegmentWithMatrixParameters)

  protected override val segmentNz: P[Segment] =
    P(SEGMENT_WITH_MATRIX_PARAMETER.rep(1).! ~ ";".rep ~ matrixParameter.rep(sep = ";".rep(1)) ~ ";".rep map extractSegmentWithMatrixParameters)

  protected override val segmentNzNc: P[Segment] =
    P(SEGMENT_WITH_MATRIX_PARAMETER_NO_COLONS.rep(1).! ~ ";".rep ~ matrixParameter.rep(sep = ";".rep(1)) ~ ";".rep map extractSegmentWithMatrixParameters)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  protected val SEGMENT_WITH_MATRIX_PARAMETER: P[Unit] = MatrixParameterParsing.SEGMENT_WITH_MATRIX_PARAMETER
  protected val SEGMENT_WITH_MATRIX_PARAMETER_NO_COLONS: P[Unit] = MatrixParameterParsing.SEGMENT_WITH_MATRIX_PARAMETER_NO_COLONS
  protected val MATRIX_PARAMETER_KEY: P[Unit] = MatrixParameterParsing.MATRIX_PARAMETER_KEY
  protected val MATRIX_PARAMETER_VALUE: P[Unit] = MatrixParameterParsing.MATRIX_PARAMETER_VALUE
}

object MatrixParameterParsing {

  private val SEGMENT_WITH_MATRIX_PARAMETER_CHARSET = encoding.PercentEncoder.RfcCharsets.SEGMENT - (';')
  private val SEGMENT_WITH_MATRIX_PARAMETER = CharIn(SEGMENT_WITH_MATRIX_PARAMETER_CHARSET.toSeq)
  private val SEGMENT_WITH_MATRIX_PARAMETER_NO_COLONS = CharIn((SEGMENT_WITH_MATRIX_PARAMETER_CHARSET - (':')).toSeq)
  private val MATRIX_PARAMETER_KEY = CharIn((SEGMENT_WITH_MATRIX_PARAMETER_CHARSET - ('=')).toSeq)
  private val MATRIX_PARAMETER_VALUE = CharIn(SEGMENT_WITH_MATRIX_PARAMETER_CHARSET.toSeq)
}
