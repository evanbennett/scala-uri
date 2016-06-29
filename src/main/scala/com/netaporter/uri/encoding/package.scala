package com.netaporter.uri

package object encoding {

  @deprecated("Use `PercentEncoder.default` instead.", "1.0.0")
  val percentEncode = PercentEncoder()

  @deprecated("Use `PercentEncoder.apply` instead.", "1.0.0")
  def percentEncode(chars: Char*) = PercentEncoder(chars.toSet)

  @deprecated("Use `EncodeCharAs.apply` instead.", "1.0.0")
  def encodeCharAs(c: Char, as: String) = EncodeCharAs(c, as)

  @deprecated("Use `EncodeCharAs.spaceAsPlus` instead.", "1.0.0")
  val spaceAsPlus = EncodeCharAs(' ', "+")
}
