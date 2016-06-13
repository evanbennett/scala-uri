package com.netaporter.uri

import com.netaporter.uri.config.UriConfig

sealed abstract class Path(val hasRootSlash: Boolean) {
  this : Path.PathWithCopyParts =>

  def segments: Seq[PathPart]
  if (segments.isEmpty) throw new IllegalArgumentException

  def appendSegment(segment: PathPart): Path = copy(segments :+ segment)

  def appendSegments(segments: Seq[PathPart]): Path = copy(segments ++ segments)

  def appendPath(path: Path): Path = copy(segments ++ path.segments)

  def addMatrixParam(existingSegment: String, key: String, value: String) = {
    copy(segments = segments.map {
      case segment if segment.part == existingSegment => segment.addParam(key -> Option(value))
      case segment => segment
    })
  }

  def addMatrixParam(key: String, value: String) = {
    copy(segments = segments.init :+ segments.last.addParam(key -> Some(value)))
  }

  override def toString = toString(UriConfig.default)

  /**
   * Returns the encoded path. By default non ASCII characters in the path are percent encoded.
   * @return String containing the path for this Uri
   */
  def toString(implicit c: UriConfig = UriConfig.default): String = {
    (if (hasRootSlash) "/" else "") + segments.map(_.partToString(c)).mkString("/")
  }

  /**
   * Returns the path with no encoders taking place (e.g. non ASCII characters will not be percent encoded)
   * @return String containing the raw path for this Uri
   */
  def toStringRaw(implicit c: UriConfig = UriConfig.default): String = toString(c.withNoEncoding)
}

case class AbsolutePath(segments: Seq[PathPart]) extends Path(true)

case class RootlessPath(segments: Seq[PathPart]) extends Path(false)

object Path {

  type PathWithCopyParts = {

    def copy(segments: Seq[PathPart]): Path
  }
}
