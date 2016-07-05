package com.netaporter.uri.parsing

import com.netaporter.uri._
import org.parboiled2._

trait DelimiterParsing {
  self: UriParser =>

  @deprecated("Being made `protected`.", "1.0.0")
  override def _userInfo: Rule1[UserInfo] = rule { // TODO: I had to add '[' or this tries to parse an `ipLiteralAddress` host:
    capture(oneOrMore(noneOf("[:@/?#"))) ~ optional(":" ~ capture(zeroOrMore(noneOf("@/?#")))) ~ "@" ~> extractUserInfo
  }

  protected override def _registeredName: Rule0 = rule {
    noneOf(".[:/?#") ~ optional((1 to 254).times(noneOf(":/?#"))) // TODO: I do not know how to implement this here: Cannot contain ".."
  }

  protected override def _segment: Rule1[Segment] = rule {
    capture(zeroOrMore(noneOf("/?#"))) ~> extractSegment
  }

  protected override def _segmentNz: Rule1[Segment] = rule {
    capture(oneOrMore(noneOf("/?#"))) ~> extractSegment
  }

  protected override def _segmentNzNc: Rule1[Segment] = rule {
    capture(oneOrMore(noneOf(":/?#"))) ~> extractSegment
  }

  protected override def _queryParam: Rule1[Parameter] = rule {
    capture(oneOrMore(noneOf("=&#"))) ~ optional("=" ~ capture(zeroOrMore(noneOf("&#")))) ~> extractParam
  }

  protected override def _fragment: Rule1[Fragment] = rule {
    "#" ~ capture(zeroOrMore(ANY)) ~> extractFragment
  }
}
