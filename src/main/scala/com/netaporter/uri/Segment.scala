package com.netaporter.uri

import com.netaporter.uri.Parameters._

/**
 * URI Segment, based on RFC 3986 section 3.3.
 */
sealed abstract class Segment(val segment: String) {

  type Self <: Segment

  @deprecated("Use `segment` instead.", "1.0.0")
  def part: String = segment

  @deprecated("Ensure you have a `MatrixParametersSegment` and then use `parameters` instead.", "1.0.0")
  def params: ParamSeq

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  @deprecated("Use `mapSegment` instead.", "1.0.0")
  def map(f: String => String): Self = mapSegment(f)

  /**
   * Transforms the `segment` by applying the specified function.
   */
  def mapSegment(f: String => String): Self

  @deprecated("Use `append(Parameter)` instead.", "1.0.0")
  def addParam(kv: Param): PathPart = append(kv._1, kv._2)

  def append(key: String, value: Any): MatrixParametersSegment

  def append(parameter: Parameter): MatrixParametersSegment

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  @deprecated("Use `toString` instead.", "1.0.0")
  def partToString(c: UriConfig): String = toString(c)

  def toString(implicit config: UriConfig): String = config.pathEncoder.encode(segment)
}

object Segment {

  def apply(segment: String, matrixParameters: Seq[Parameter]): Segment = {
    if (matrixParameters == null || matrixParameters.isEmpty) StringSegment(segment) else MatrixParametersSegment(segment, matrixParameters)
  }

  def apply(segment: String, matrixParameters: Parameter*)(implicit di: DummyImplicit): Segment =
    apply(segment, matrixParameters)
}

sealed abstract case class StringSegment(override val segment: String) extends Segment(segment) {

  type Self = StringSegment

  @deprecated("Is only relavent to `MatrixParametersSegment` and is being removed.", "1.0.0")
  def params = Vector.empty

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def append(key: String, value: Any) = append(Parameter(key, value))

  def append(parameter: Parameter) = MatrixParametersSegment(segment, Seq(parameter))

  def mapSegment(f: String => String) = StringSegment(f(segment))

  def copy(segment: String = segment): StringSegment = StringSegment(segment)
}

object StringSegment {

  def apply(segment: String): StringSegment = {
    if (segment == null) throw new IllegalArgumentException("`segment` cannot be `null`.")
    if (segment.isEmpty) EmptySegment else new StringSegment(segment) {}
  }
}

sealed abstract case class MatrixParametersSegment(override val segment: String, parameters: Seq[Parameter]) extends Segment(segment) with Parameters {

  type Self = MatrixParametersSegment

  val separator = ";"

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def mapSegment(f: String => String) = MatrixParametersSegment(f(segment), parameters)

  def withParameters(newParameters: Seq[Parameter]) = MatrixParametersSegment(segment, newParameters)

  def copy(segment: String = segment, parameters: Seq[Parameter] = parameters): MatrixParametersSegment = MatrixParametersSegment(segment, parameters)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  override def toString(implicit config: UriConfig): String =
    super.toString + ";" + paramString(config.pathEncoder)
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

@deprecated("Use `Segment` instead.", "1.0.0")
object PathPart {

  @deprecated("Use `Segment.apply` instead.", "1.0.0")
  def apply(path: String, matrixParams: ParamSeq = Seq.empty): PathPart =
    if (matrixParams.isEmpty) StringSegment(path) else MatrixParametersSegment(path, matrixParams)
}

@deprecated("Use `StringSegment` instead.", "1.0.0")
object StringPathPart {

  @deprecated("Use `StringSegment.apply` instead.", "1.0.0")
  def apply(part: String) = StringSegment(part)

  @deprecated("Use `StringSegment.unapply` instead.", "1.0.0")
  def unapply(stringPathPart: StringPathPart): Option[String] =
    if (stringPathPart == null) None else Option(stringPathPart.segment)
}

@deprecated("Use `MatrixParametersSegment` instead.", "1.0.0")
object MatrixParams {

  @deprecated("Use `MatrixParametersSegment.apply` instead.", "1.0.0")
  def apply(part: String, params: ParamSeq) = MatrixParametersSegment(part, params)

  @deprecated("Use `MatrixParametersSegment.unapply` instead.", "1.0.0")
  def unapply(matrixParams: MatrixParams): Option[(String, ParamSeq)] =
    if (matrixParams == null) None else Option((matrixParams.segment, matrixParams.params))
}
