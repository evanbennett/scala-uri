package com.netaporter

package object uri {

  @deprecated("Use `Segment` instead.", "1.0.0")
  type PathPart = Segment

  @deprecated("Use `StringSegment` instead.", "1.0.0")
  type StringPathPart = StringSegment

  @deprecated("Use `MatrixParametersSegment` instead.", "1.0.0")
  type MatrixParams = MatrixParametersSegment

  @deprecated("Use `ParameterQuery` or more generically `Query` instead.", "1.0.0")
  type QueryString = Query

  @deprecated("Use `EmptyQuery` instead.", "1.0.0")
  val EmptyQueryString = EmptyQuery
}
