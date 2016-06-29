package com.netaporter

package object uri {

  @deprecated("Use `Segment` instead.", "1.0.0")
  type PathPart = Segment

  @deprecated("Use `StringSegment` instead.", "1.0.0")
  type StringPathPart = StringSegment

  @deprecated("Use `MatrixParametersSegment` instead.", "1.0.0")
  type MatrixParams = MatrixParametersSegment

  @deprecated("Use `Query` instead.", "1.0.0")
  type QueryString = Query
}
