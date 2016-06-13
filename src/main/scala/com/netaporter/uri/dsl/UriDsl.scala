package com.netaporter.uri.dsl

import com.netaporter.uri._

/**
 * Value class to add DSL functionality to Uris
 *
 * @param uri
 */
class UriDsl(val uri: Uri) extends AnyVal {

  /**
   * Adds a new Query String parameter key-value pair. If the value for the Query String parameter is None, then this
   * Query String parameter will not be rendered in calls to toString or toStringRaw
   * @param kv Tuple2 representing the query string parameter
   * @return A new Uri with the new Query String parameter
   */
  def ?(kv: (String, Any)) = uri.addParam(kv._1, kv._2)

  /**
   * Adds a trailing forward slash to the path and a new Query String parameter key-value pair.
   * If the value for the Query String parameter is None, then this Query String parameter will
   * not be rendered in calls to toString or toStringRaw
   * @param kv Tuple2 representing the query string parameter
   * @return A new Uri with the new Query String parameter
   */
  def /?(kv: (String, Any)) = /("").addParam(kv._1, kv._2)

  /**
   * Adds a new Query String parameter key-value pair. If the value for the Query String parameter is None, then this
   * Query String parameter will not be rendered in calls to toString or toStringRaw
   * @param kv Tuple2 representing the query string parameter
   * @return A new Uri with the new Query String parameter
   */
  def &(kv: (String, Any)) = uri.addParam(kv._1, kv._2)

  /**
   * Adds a fragment to the end of the uri
   * @param fragment String representing the fragment
   * @return A new Uri with this fragment
   */
  def `#`(fragment: String) = uri.withFragment(fragment)

  /**
   * Appends a path part to the path of this URI
   * @param pp The path part
   * @return A new Uri with this path part appended
   */
  def /(pp: String) = uri.copy(path = uri.path.map(_.appendSegment(StringPathPart(pp))).orElse(Some(AbsolutePath(Seq(StringPathPart(pp))))))

  /**
   * Operator precedence in Scala will mean that our DSL will not always be executed left to right.
   *
   * For the operators this DSL cares about, the order will be
   *
   * (all letters)
   * &
   * :
   * /
   * `#` ?
   *
   * (see Scala Reference - 6.12.3 Infix Operations: http://www.scala-lang.org/docu/files/ScalaReference.pdf)
   *
   * To handle cases where the right hard part of the DSL is executed first, we turn that into a Uri, and merge
   * it with the left had side. It is assumed the right hand Uri is generated from this DSL only to add path
   * parts, query parameters or to overwrite the fragment
   *
   * @param other A Uri generated by more DSL to the right of us
   * @return A Uri with the right hand DSL merged into us
   */
  private def merge(other: Uri) =
    uri.copy(
      path = uri.path.map(path => other.path.fold(path)(path.appendPath)).orElse(other.path),
      query = uri.query.addParams(other.query),
      fragment = other.fragment.orElse(uri.fragment)
    )

  // NOTE: When using uri / "" ? ("", ""), `?` has precedence and converts the path to a RootlessPath which must be converted to an AbsolutePath.
  def /(other: Uri) =
    uri.copy(
      path = uri.path.map(path => other.path.fold(path)(path.appendPath)).orElse {
        other.path match {
          case Some(path: RootlessPath) => Some(AbsolutePath(path.segments))
          case pathOption => pathOption
        }
      },
      query = uri.query.addParams(other.query),
      fragment = other.fragment.orElse(uri.fragment)
    )
  def ?(other: Uri) = merge(other)
  def `#`(other: Uri) = merge(other)
  def &(other: Uri) = merge(other)
}
