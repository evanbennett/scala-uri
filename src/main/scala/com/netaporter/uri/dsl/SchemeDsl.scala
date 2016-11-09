package com.netaporter.uri.dsl

import com.netaporter.uri._

sealed case class SchemeDsl(scheme: Option[Scheme]) {

  def `:`: SchemeDsl = this

  def `://`: AuthorityDsl = AuthorityDsl(scheme, EmptyAuthority)

  def `://`(authority: Authority): AuthorityDsl = AuthorityDsl(scheme, authority)

  def `:///`: PathDsl = PathDsl(scheme, Some(EmptyAuthority), EmptyAbsolutePath)

  def `:///`(firstSegment: Segment): PathDsl = PathDsl(scheme, Some(EmptyAuthority), AbsolutePath(firstSegment))

  def `:///?`: QueryDsl = QueryDsl(scheme, Some(EmptyAuthority), Some(EmptyAbsolutePath), EmptyQuery)

  def `:///?`(firstQueryParameter: Parameter): QueryDsl = QueryDsl(scheme, Some(EmptyAuthority), Some(EmptyAbsolutePath), Query(firstQueryParameter))

  def `:///?`(queryString: String): QueryDsl = QueryDsl(scheme, Some(EmptyAuthority), Some(EmptyAbsolutePath), Query(queryString))

  def `:///?#`: FragmentDsl = FragmentDsl(scheme, Some(EmptyAuthority), Some(EmptyAbsolutePath), Some(EmptyQuery), Some(EmptyFragment))

  def `:///?#`(fragment: String): FragmentDsl = FragmentDsl(scheme, Some(EmptyAuthority), Some(EmptyAbsolutePath), Some(EmptyQuery), Fragment.option(fragment))

  def `:///#`: FragmentDsl = FragmentDsl(scheme, Some(EmptyAuthority), Some(EmptyAbsolutePath), None, Some(EmptyFragment))

  def `:///#`(fragment: String): FragmentDsl = FragmentDsl(scheme, Some(EmptyAuthority), Some(EmptyAbsolutePath), None, Fragment.option(fragment))

  def `://?`: QueryDsl = QueryDsl(scheme, Some(EmptyAuthority), None, EmptyQuery)

  def `://?`(firstQueryParameter: Parameter): QueryDsl = QueryDsl(scheme, Some(EmptyAuthority), None, Query(firstQueryParameter))

  def `://?`(queryString: String): QueryDsl = QueryDsl(scheme, Some(EmptyAuthority), None, Query(queryString))

  def `://?#`: FragmentDsl = FragmentDsl(scheme, Some(EmptyAuthority), None, Some(EmptyQuery), Some(EmptyFragment))

  def `://?#`(fragment: String): FragmentDsl = FragmentDsl(scheme, Some(EmptyAuthority), None, Some(EmptyQuery), Fragment.option(fragment))

  def `://#`: FragmentDsl = FragmentDsl(scheme, Some(EmptyAuthority), None, None, Some(EmptyFragment))

  def `://#`(fragment: String): FragmentDsl = FragmentDsl(scheme, Some(EmptyAuthority), None, None, Fragment.option(fragment))

  def :/ : PathDsl = PathDsl(scheme, None, EmptyAbsolutePath)

  def :/(firstSegment: Segment): PathDsl = PathDsl(scheme, None, AbsolutePath(firstSegment))

  def :/? : QueryDsl = QueryDsl(scheme, None, Some(EmptyAbsolutePath), EmptyQuery)

  def :/?(firstQueryParameter: Parameter): QueryDsl = QueryDsl(scheme, None, Some(EmptyAbsolutePath), Query(firstQueryParameter))

  def :/?(queryString: String): QueryDsl = QueryDsl(scheme, None, Some(EmptyAbsolutePath), Query(queryString))

  def :/?# : FragmentDsl = FragmentDsl(scheme, None, Some(EmptyAbsolutePath), Some(EmptyQuery), Some(EmptyFragment))

  def :/?#(fragment: String): FragmentDsl = FragmentDsl(scheme, None, Some(EmptyAbsolutePath), Some(EmptyQuery), Fragment.option(fragment))

  def :/# : FragmentDsl = FragmentDsl(scheme, None, Some(EmptyAbsolutePath), None, Some(EmptyFragment))

  def :/#(fragment: String): FragmentDsl = FragmentDsl(scheme, None, Some(EmptyAbsolutePath), None, Fragment.option(fragment))

  /**
   * THEON: This method CANNOT be used using operator notation, as it reverses the arguments, and when it does not provide a compilation error, it will provide a "runtime" error.
   * I have tried:
   *  * adding a `DummyImplicit` argument
   *  * adding a dummy argument with a default value
   *  * implementing an "implicit class(String) def `:`(String)" which swaps the arguments, but this kills the usage that is working.
   */
  def `:`(firstSegment: Segment): PathDsl = PathDsl(scheme, None, RootlessPath(firstSegment))

  def :? : QueryDsl = QueryDsl(scheme, None, None, EmptyQuery)

  def :?(firstQueryParameter: Parameter): QueryDsl = QueryDsl(scheme, None, None, Query(firstQueryParameter))

  def :?(queryString: String): QueryDsl = QueryDsl(scheme, None, None, Query(queryString))

  def :?# : FragmentDsl = FragmentDsl(scheme, None, None, Some(EmptyQuery), Some(EmptyFragment))

  def :?#(fragment: String): FragmentDsl = FragmentDsl(scheme, None, None, Some(EmptyQuery), Fragment.option(fragment))

  def :# : FragmentDsl = FragmentDsl(scheme, None, None, None, Some(EmptyFragment))

  def :#(fragment: String): FragmentDsl = FragmentDsl(scheme, None, None, None, Fragment.option(fragment))

  def toUri(implicit config: UriConfig): Uri = Uri(scheme, None, None, None, None)

  def toString(implicit config: UriConfig): String = toUri.toString
}
