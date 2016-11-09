package com.netaporter.uri

import com.netaporter.uri.Parameters._

/**
 * URI Path, based on RFC 3986 section 3.3.
 */
sealed abstract class Path(val segments: Seq[Segment]) {

  type Self <: Path

  def matrixParameters(existingSegment: String): Seq[Parameter] = {
    segments.filter(_.segment == existingSegment).flatMap {
      case segment: MatrixParametersSegment => segment.parameters
      case _ => Seq.empty
    }
  }

  def matrixParametersOfLastSegment: Seq[Parameter] = {
    segments.last match {
      case segment: MatrixParametersSegment => segment.parameters
      case _ => Seq.empty
    }
  }

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
    copy(segments.init :+ segments.last.append(key, value))

  def appendMatrixParameterToLastSegment(parameter: Parameter): Self =
    copy(segments.init :+ segments.last.append(parameter))

  /** Remove the last segment, and append `otherPath`s segments, based on RFC 3986 section 5.2.3. */
  def merge(otherPath: RootlessPath): Self =
    copy(segments.init ++ otherPath.segments)

  /** Remove any "." and ".." segments, based on RFC 3986 section 5.2.4. */
  def removeDotSegments(): Self = copy(Path.removeDotSegments(segments))

  def copy(segments: Seq[Segment]): Self

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit config: UriConfig): String
}

object Path {

  /** Remove any "." and ".." segments, based on RFC 3986 section 5.2.4. */
  def removeDotSegments(segments: Seq[Segment]): Seq[Segment] = {
    val segmentsWithoutDots = segments.foldLeft(Seq.empty[Segment]) {
      case (segmentsSoFar, nextSegment) =>
        nextSegment match {
          case nextSegment: StringSegment =>
            if (nextSegment.segment == ".") segmentsSoFar
            else if (nextSegment.segment == "..") segmentsSoFar.dropRight(1)
            else segmentsSoFar :+ nextSegment
          case _ => segmentsSoFar :+ nextSegment
        }
    }
    val lastSegment = segments.last.segment
    if (lastSegment == "." || lastSegment == "..") segmentsWithoutDots :+ EmptySegment else segmentsWithoutDots
  }
}

sealed abstract case class RootlessPath(override val segments: Seq[Segment]) extends Path(segments) {

  type Self = RootlessPath

  def copy(segments: Seq[Segment] = segments): RootlessPath = RootlessPath(segments)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit config: UriConfig): String = segments.map(_.toString).mkString("/")
}

object RootlessPath {

  def apply(segments: Seq[Segment]): RootlessPath = {
    if (segments == null) throw new IllegalArgumentException("`segments` cannot be `null`.")
    if (segments.isEmpty) throw new IllegalArgumentException("This should be an empty Path (None).")
    if (segments.head == EmptySegment) {
      if (segments.length == 1) throw new IllegalArgumentException("This should be an empty Path (None).")
      throw new IllegalArgumentException("This should be an AbsolutePath.")
    }
    new RootlessPath(segments) {}
  }

  def apply(segments: Segment*)(implicit di: DummyImplicit): RootlessPath = apply(segments)

  def option(segments: Seq[Segment]): Option[RootlessPath] =
    if (segments == null || segments.isEmpty || (segments.length == 1 && segments.head == EmptySegment)) None else Option(apply(segments))

  def option(segments: Segment*)(implicit di: DummyImplicit): Option[RootlessPath] = option(segments)
}

sealed abstract case class AbsolutePath(override val segments: Seq[Segment]) extends Path(segments) {

  type Self = AbsolutePath

  def copy(segments: Seq[Segment] = segments): AbsolutePath = AbsolutePath(segments)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit config: UriConfig): String = "/" + segments.map(_.toString).mkString("/")
}

object AbsolutePath {

  def apply(segments: Seq[Segment]): AbsolutePath = {
    if (segments == null) throw new IllegalArgumentException("`segments` cannot be `null`.")
    if (segments.isEmpty || (segments.length == 1 && segments.head == EmptySegment)) EmptyAbsolutePath else new AbsolutePath(segments) {}
  }

  def apply(segments: Segment*)(implicit di: DummyImplicit): AbsolutePath = apply(segments)

  def option(segments: Seq[Segment]): Option[AbsolutePath] =
    if (segments == null || segments.isEmpty) None else Option(apply(segments))

  def option(segments: Segment*)(implicit di: DummyImplicit): Option[AbsolutePath] = option(segments)
}

object EmptyAbsolutePath extends AbsolutePath(Seq(EmptySegment))
