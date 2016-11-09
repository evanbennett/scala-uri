package com.netaporter.uri

package object encoding {

  @deprecated("Use a tighter character set relavent to your use case instead.", "1.0.0")
  val percentEncode = PercentEncoder()

  @deprecated("Use `PercentEncoder.apply` instead.", "1.0.0")
  def percentEncode(chars: Char*) = PercentEncoder(chars.toSet)

  @deprecated("Use `EncodeCharAs.apply` instead.", "1.0.0")
  def encodeCharAs(c: Char, as: String) = EncodeCharAs(c, as)

  @deprecated("Use `EncodeCharAs.SPACE_AS_PLUS` instead.", "1.0.0")
  val spaceAsPlus = EncodeCharAs(' ', "+")
}
