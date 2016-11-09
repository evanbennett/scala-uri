package com.netaporter.uri.dsl

import com.netaporter.uri._

sealed case class PathDsl(scheme: Option[Scheme], authority: Option[Authority], path: Path) {

  def / : PathDsl = copy(path = path.appendSegment(EmptySegment))

  def /(nextSegment: Segment): PathDsl = copy(path = path.appendSegment(nextSegment))

  def /(nextSegmentAndQuery: SegmentAndQuery): QueryDsl = QueryDsl(scheme, authority, Option(path.appendSegment(nextSegmentAndQuery.segment)), nextSegmentAndQuery.query)

  def /(nextSegmentAndQueryAndFragment: SegmentAndQueryAndFragment): FragmentDsl = FragmentDsl(scheme, authority, Option(path.appendSegment(nextSegmentAndQueryAndFragment.segment)), Option(nextSegmentAndQueryAndFragment.query), Option(nextSegmentAndQueryAndFragment.fragment))

  def /(nextSegmentAndFragment: SegmentAndFragment): FragmentDsl = FragmentDsl(scheme, authority, Option(path.appendSegment(nextSegmentAndFragment.segment)), None, Option(nextSegmentAndFragment.fragment))

  def /? : QueryDsl = QueryDsl(scheme, authority, Option(path.appendSegment(EmptySegment)), EmptyQuery)

  def /?(firstQueryParameter: Parameter): QueryDsl = QueryDsl(scheme, authority, Option(path.appendSegment(EmptySegment)), Query(firstQueryParameter))

  def /?(queryString: String): QueryDsl = QueryDsl(scheme, authority, Option(path.appendSegment(EmptySegment)), Query(queryString))

  def /?# : FragmentDsl = FragmentDsl(scheme, authority, Option(path.appendSegment(EmptySegment)), Some(EmptyQuery), Some(EmptyFragment))

  def /?#(fragment: String): FragmentDsl = FragmentDsl(scheme, authority, Option(path.appendSegment(EmptySegment)), Some(EmptyQuery), Fragment.option(fragment))

  def /# : FragmentDsl = FragmentDsl(scheme, authority, Option(path.appendSegment(EmptySegment)), None, Some(EmptyFragment))

  def /#(fragment: String): FragmentDsl = FragmentDsl(scheme, authority, Option(path.appendSegment(EmptySegment)), None, Fragment.option(fragment))

  def ? : QueryDsl = QueryDsl(scheme, authority, Option(path), EmptyQuery)

  def ?(firstQueryParameter: Parameter): QueryDsl = QueryDsl(scheme, authority, Option(path), Query(firstQueryParameter))

  def ?(queryString: String): QueryDsl = QueryDsl(scheme, authority, Option(path), Query(queryString))

  def ?# : FragmentDsl = FragmentDsl(scheme, authority, Option(path), Some(EmptyQuery), Some(EmptyFragment))

  def ?#(fragment: String): FragmentDsl = FragmentDsl(scheme, authority, Option(path), Some(EmptyQuery), Fragment.option(fragment))

  def `#`: FragmentDsl = FragmentDsl(scheme, authority, Option(path), None, Some(EmptyFragment))

  def `#`(fragment: String): FragmentDsl = FragmentDsl(scheme, authority, Option(path), None, Fragment.option(fragment))

  def toUri(implicit config: UriConfig): Uri = Uri(scheme, authority, Some(path), None, None)

  def toString(implicit config: UriConfig): String = toUri.toString
}
