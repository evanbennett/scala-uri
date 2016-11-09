package com.netaporter.uri.parsing

import org.parboiled2._
import com.netaporter.uri.PathPart
import com.netaporter.uri.Parameters._

@deprecated("Convert to the new DSL.", "1.0.0")
trait MatrixParamSupport {
  this: Parser with UriParser =>

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _plainPathPart: Rule1[String] = rule {
    capture(zeroOrMore(!anyOf(";/?#") ~ ANY))
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  def _matrixParam: Rule1[Param] = rule {
    capture(zeroOrMore(!anyOf(";/=?#") ~ ANY)) ~ "=" ~ capture(zeroOrMore(!anyOf(";/=?#") ~ ANY)) ~> extractTuple
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  override def _pathSegment: Rule1[PathPart] = rule {
    _plainPathPart ~ zeroOrMore(";") ~
    zeroOrMore(_matrixParam).separatedBy(oneOrMore(";")) ~
    zeroOrMore(";") ~> extractPathPartWithMatrixParams
  }

  @deprecated("Convert to the new DSL.", "1.0.0")
  val extractPathPartWithMatrixParams = (pathPart: String, matrixParams: ParamSeq) => {
    val decodedPathPart = pathDecoder.decode(pathPart, originalInput)
    val decodedMatrixParams = matrixParams.map(pathDecoder.decodeTuple(_, originalInput))
    PathPart(decodedPathPart, decodedMatrixParams.toVector)
  }
}
