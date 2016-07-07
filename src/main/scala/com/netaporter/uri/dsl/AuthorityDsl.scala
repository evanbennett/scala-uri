package com.netaporter.uri.dsl

import com.netaporter.uri._

sealed case class AuthorityDsl(scheme: Option[Scheme], authority: Authority) {

  def / : PathDsl = PathDsl(scheme, Option(authority), EmptyAbsolutePath)

  def /(firstSegment: Segment): PathDsl = PathDsl(scheme, Option(authority), AbsolutePath(firstSegment))

  def /(firstSegmentAndQuery: SegmentAndQuery): QueryDsl = QueryDsl(scheme, Option(authority), AbsolutePath.option(firstSegmentAndQuery.segment), firstSegmentAndQuery.query)

  def /(firstSegmentAndQueryAndFragment: SegmentAndQueryAndFragment): FragmentDsl = FragmentDsl(scheme, Option(authority), AbsolutePath.option(firstSegmentAndQueryAndFragment.segment), Option(firstSegmentAndQueryAndFragment.query), Option(firstSegmentAndQueryAndFragment.fragment))

  def /(firstSegmentAndFragment: SegmentAndFragment): FragmentDsl = FragmentDsl(scheme, Option(authority), AbsolutePath.option(firstSegmentAndFragment.segment), None, Option(firstSegmentAndFragment.fragment))

  def /? : QueryDsl = QueryDsl(scheme, Option(authority), Some(EmptyAbsolutePath), EmptyQuery)

  def /?(firstQueryParameter: Parameter): QueryDsl = QueryDsl(scheme, Option(authority), Some(EmptyAbsolutePath), Query(firstQueryParameter))

  def /?(firstQueryKey: String): QueryDsl = QueryDsl(scheme, Option(authority), Some(EmptyAbsolutePath), EmptyQuery.append(firstQueryKey))

  def /?# : FragmentDsl = FragmentDsl(scheme, Option(authority), Some(EmptyAbsolutePath), Some(EmptyQuery), Some(EmptyFragment))

  def /?#(fragment: String): FragmentDsl = FragmentDsl(scheme, Option(authority), Some(EmptyAbsolutePath), Some(EmptyQuery), Fragment.option(fragment))

  def /# : FragmentDsl = FragmentDsl(scheme, Option(authority), Some(EmptyAbsolutePath), None, Some(EmptyFragment))

  def /#(fragment: String): FragmentDsl = FragmentDsl(scheme, Option(authority), Some(EmptyAbsolutePath), None, Fragment.option(fragment))

  def ? : QueryDsl = QueryDsl(scheme, Option(authority), None, EmptyQuery)

  def ?(firstQueryParameter: Parameter): QueryDsl = QueryDsl(scheme, Option(authority), None, Query(firstQueryParameter))

  def ?(firstQueryKey: String): QueryDsl = QueryDsl(scheme, Option(authority), None, EmptyQuery.append(firstQueryKey))

  def ?# : FragmentDsl = FragmentDsl(scheme, Option(authority), None, Some(EmptyQuery), Some(EmptyFragment))

  def ?#(fragment: String): FragmentDsl = FragmentDsl(scheme, Option(authority), None, Some(EmptyQuery), Fragment.option(fragment))

  def `#`: FragmentDsl = FragmentDsl(scheme, Option(authority), None, None, Some(EmptyFragment))

  def `#`(fragment: String): FragmentDsl = FragmentDsl(scheme, Option(authority), None, None, Fragment.option(fragment))

  def toUri: Uri = Uri(scheme, Some(authority), None, None, None)
}
