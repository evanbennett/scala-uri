package com.netaporter.uri.parsing

import com.netaporter.uri._
import FragmentAllowHashParsing._
import fastparse.all._

/**
 * Modify `BaseParser` to only allow the fragment to contain '#'s.
 *
 * RFC 3986 Errata 3330 requesting allowing '#'s in the fragment, has been rejected.
 * Even so, this appears to occur with some regularity, and would likely be included in an updated specification and so has been implemented.
 */
trait FragmentAllowHashParsing extends BaseParser {

  protected override val FRAGMENT = FragmentAllowHashParsing.FRAGMENT
}

object FragmentAllowHashParsing {

  private val FRAGMENT = CharIn((encoding.PercentEncoder.RfcCharsets.FRAGMENT + '#').toSeq)
}
