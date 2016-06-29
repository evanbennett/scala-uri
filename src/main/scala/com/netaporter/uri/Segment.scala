package com.netaporter.uri

import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.Parameters._

sealed abstract class Segment {

  type Self <: Segment

  @deprecated("Use `segment` instead.", "1.0.0")
  def part: String = segment

  /**
   * The non-parameter segment of this segment
   *
   * @return
   */
  def segment: String

  @deprecated("Use `parameters` instead.", "1.0.0")
  def params: ParamSeq

  def parameters: Seq[Parameter]

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  @deprecated("Use `mapSegments` instead.", "1.0.0")
  def map(f: String => String): Self = mapSegments(f)

  /**
   * Transforms the `segment` by applying the specified Function
   */
  def mapSegments(f: String => String): Self

  @deprecated("Use `append(Parameter)` instead.", "1.0.0")
  def addParam(kv: Param): PathPart = append(kv._1, kv._2)

  /**
   * Adds a matrix parameter to the end of this segment
   */
  def append(key: String, value: String): MatrixParametersSegment

  /**
   * Adds a matrix parameter to the end of this segment
   */
  def append(key: String, value: Option[String]): MatrixParametersSegment

  /**
   * Adds a matrix parameter to the end of this segment
   */
  def append(parameter: Parameter): MatrixParametersSegment

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  @deprecated("Use `toString` instead.", "1.0.0")
  def partToString(c: UriConfig): String = toString(c)

  def toString(implicit c: UriConfig): String = c.pathEncoder.encode(segment, c.charset)

  def toStringRaw(implicit c: UriConfig): String = toString(c.withNoEncoding)
}

sealed abstract case class StringSegment(segment: String) extends Segment {

  type Self = StringSegment

  @deprecated("Use `parameters` instead.", "1.0.0")
  def params = Vector.empty

  val parameters = Vector.empty

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def mapSegments(f: String => String) = StringSegment(f(segment))

  def append(key: String, value: String) = append(Parameter(key, value))

  def append(key: String, value: Option[String]) = append(Parameter(key, value))

  def append(parameter: Parameter) = MatrixParametersSegment(segment, Vector(parameter))

  def copy(segment: String = segment): StringSegment = StringSegment(segment)
}

sealed abstract case class MatrixParametersSegment(segment: String, parameters: Seq[Parameter]) extends Segment with Parameters {

  type Self = MatrixParametersSegment

  val separator = ";"

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def mapSegments(f: String => String) = MatrixParametersSegment(f(segment), parameters)

  def withParameters(newParameters: Seq[Parameter]) = MatrixParametersSegment(segment, newParameters)

  def copy(segment: String = segment, parameters: Seq[Parameter] = parameters): MatrixParametersSegment = MatrixParametersSegment(segment, parameters)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  override def toString(implicit c: UriConfig): String =
    super.toString + ";" + toString(c.pathEncoder, c.charset)
}

object Segment {

  def apply(segment: String, matrixParameters: Seq[Parameter]): Segment = {
    if (matrixParameters.isEmpty) StringSegment(segment) else MatrixParametersSegment(segment, matrixParameters)
  }

  def apply(segment: String, matrixParameters: Parameter*)(implicit di: DummyImplicit): Segment =
    apply(segment, matrixParameters)
}

object StringSegment {

  def apply(segment: String): StringSegment = {
    if (segment == null) throw new IllegalArgumentException("`segment` cannot be `null`.")
    if (segment.isEmpty) EmptySegment else new StringSegment(segment) {}
  }
}

object MatrixParametersSegment {

  def apply(segment: String, parameters: Seq[Parameter]): MatrixParametersSegment = {
    if (segment == null) throw new IllegalArgumentException("`segment` cannot be `null`.")
    if (parameters == null) throw new IllegalArgumentException("`parameters` cannot be `null`.")
    new MatrixParametersSegment(segment, parameters) {}
  }

  def apply(segment: String, parameters: Parameter*)(implicit di: DummyImplicit): MatrixParametersSegment = apply(segment, parameters)
}

object EmptySegment extends StringSegment("")

object PathPart {

  @deprecated("Use `Segment.apply` instead.", "1.0.0")
  def apply(path: String, matrixParams: ParamSeq = Seq.empty): PathPart =
    if(matrixParams.isEmpty) StringSegment(path) else MatrixParametersSegment(path, matrixParams)
}

object StringPathPart {

  @deprecated("Use `StringSegment.apply` instead.", "1.0.0")
  def apply(path: String) = StringSegment(path)

  @deprecated("Use `StringSegment.unapply` instead.", "1.0.0")
  def unapply(pathpart: StringPathPart): Option[(String)] =
    if (pathpart == null) None else Some((pathpart.segment))
}

object MatrixParams {

  @deprecated("Use `MatrixParametersSegment.apply` instead.", "1.0.0")
  def apply(part: String, matrixParams: ParamSeq = Seq.empty) = MatrixParametersSegment(part, matrixParams)

  @deprecated("Use `MatrixParametersSegment.unapply` instead.", "1.0.0")
  def unapply(pathpart: MatrixParams): Option[(String, ParamSeq)] =
    if (pathpart == null) None else Some((pathpart.segment, pathpart.params))
}
