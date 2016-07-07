package com.netaporter.uri

import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.Parameters._

sealed abstract class Path(val hasRootSlash: Boolean) {

  type Self <: Path

  def segments: Seq[Segment]

  def matrixParameters(existingSegment: String): Seq[Parameter] =
    segments.filter(_.segment == existingSegment).flatMap(_.parameters)

  def matrixParametersOfLastSegment: Seq[Parameter] =
    segments.last.parameters

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def appendSegment(segment: Segment): Self = copy(segments :+ segment)

  def appendSegments(segments: Seq[Segment]): Self = copy(this.segments ++ segments)

  def appendSegments(segments: Segment*)(implicit di: DummyImplicit): Self = copy(this.segments ++ segments)

  def appendSegments(path: Path): Self = copy(segments ++ path.segments)

  def appendMatrixParameter(existingSegment: String, key: String, value: Any): Self =
    appendMatrixParameter(existingSegment, Parameter(key, value))

  def appendMatrixParameter(existingSegment: String, parameter: Parameter): Self = {
    copy(segments = segments.map {
      case segment if segment.segment == existingSegment => segment.append(parameter)
      case segment => segment
    })
  }

  def appendMatrixParameterToLastSegment(key: String, value: Any): Self =
    copy(segments = segments.dropRight(1) :+ segments.last.append(key, value))

  def appendMatrixParameterToLastSegment(parameter: Parameter): Self =
    copy(segments = segments.dropRight(1) :+ segments.last.append(parameter))

  def copy(segments: Seq[Segment]): Self

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  /**
   * Returns the encoded path. By default non ASCII characters in the path are percent encoded.
   * @return String containing the path for this Uri
   */
  def toString(implicit c: UriConfig): String =
    (if (hasRootSlash) "/" else "") + segments.map(_.toString).mkString("/")

  /**
   * Returns the path with no encoders taking place (e.g. non ASCII characters will not be percent encoded)
   * @return String containing the raw path for this Uri
   */
  def toStringRaw(implicit c: UriConfig): String = toString(c.withNoEncoding)
}

sealed abstract case class AbsolutePath(segments: Seq[Segment]) extends Path(true) {

  type Self = AbsolutePath

  def copy(segments: Seq[Segment] = segments): AbsolutePath = AbsolutePath(segments)
}

// TODO: I stuck with `RootlessPath` as `RelativePath` does not really make sense when dealing with schemes other than "http". (e.g. "mailto"; "urn" with "isbn" or "uuid")
sealed abstract case class RootlessPath(segments: Seq[Segment]) extends Path(false) {

  type Self = RootlessPath

  def copy(segments: Seq[Segment] = segments): RootlessPath = RootlessPath(segments)
}

object AbsolutePath {

  def apply(segments: Seq[Segment]): AbsolutePath = {
    if (segments == null) throw new IllegalArgumentException("`segments` cannot be `null`.")
    if (segments.isEmpty) throw new IllegalArgumentException("This should be an empty Path (None).")
    if (segments.length == 1 && segments(0) == EmptySegment) EmptyAbsolutePath else new AbsolutePath(segments) {}
  }

  def apply(segments: Segment*)(implicit di: DummyImplicit): AbsolutePath = apply(segments)

  def option(segments: Seq[Segment]): Option[AbsolutePath] =
    if (segments == null || segments.isEmpty) None else Option(apply(segments))

  def option(segments: Segment*)(implicit di: DummyImplicit): Option[AbsolutePath] = option(segments)
}

object EmptyAbsolutePath extends AbsolutePath(Seq(EmptySegment))

// TODO: This object could be duplicated as `RelativePath` (and the class aliased) if you really want a `RelativePath`?
object RootlessPath {

  def apply(segments: Seq[Segment]): RootlessPath = {
    if (segments == null) throw new IllegalArgumentException("`segments` cannot be `null`.")
    if (segments.isEmpty) throw new IllegalArgumentException("This should be an empty Path (None).")
    if (segments.head.segment.isEmpty) {
      if (segments.length == 1) throw new IllegalArgumentException("This should be an empty Path (None).")
      throw new IllegalArgumentException("This would be an AbsolutePath.")
    }
    new RootlessPath(segments) {}
  }

  def apply(segments: Segment*)(implicit di: DummyImplicit): RootlessPath = apply(segments)

  def option(segments: Seq[Segment]): Option[RootlessPath] =
    if (segments == null || segments.isEmpty) None else Option(apply(segments))

  def option(segments: Segment*)(implicit di: DummyImplicit): Option[RootlessPath] = option(segments)
}
