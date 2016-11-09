package com.netaporter.uri.dsl

import com.netaporter.uri._

sealed case class FragmentDsl(scheme: Option[Scheme], authority: Option[Authority], path: Option[Path], query: Option[Query], fragment: Option[Fragment]) {

  def toUri(implicit config: UriConfig): Uri = Uri(scheme, authority, path, query, fragment)

  def toString(implicit config: UriConfig): String = toUri.toString
}
