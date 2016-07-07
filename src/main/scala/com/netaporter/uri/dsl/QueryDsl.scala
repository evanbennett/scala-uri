package com.netaporter.uri.dsl

import com.netaporter.uri._

sealed case class QueryDsl(scheme: Option[Scheme], authority: Option[Authority], path: Option[Path], query: Query) {

  def &(nextQueryParameter: Parameter): QueryDsl = QueryDsl(scheme, authority, path, query.append(nextQueryParameter))

  def &(nextQueryKey: String): QueryDsl = QueryDsl(scheme, authority, path, query.append(nextQueryKey))

  def &(nextQueryParameterAndFragment: ParameterAndFragment): FragmentDsl = FragmentDsl(scheme, authority, path, Option(query.append(nextQueryParameterAndFragment.parameter)), Option(nextQueryParameterAndFragment.fragment))

  def &(nextQueryParameterAndFragment: SegmentAndFragment): FragmentDsl = FragmentDsl(scheme, authority, path, Option(query.append(nextQueryParameterAndFragment.segment.segment)), Option(nextQueryParameterAndFragment.fragment))

  def `#`: FragmentDsl = FragmentDsl(scheme, authority, path, Option(query), Some(EmptyFragment))

  def `#`(fragment: String): FragmentDsl = FragmentDsl(scheme, authority, path, Option(query), Fragment.option(fragment))

  def toUri: Uri = Uri(scheme, authority, path, Option(query), None)
}
