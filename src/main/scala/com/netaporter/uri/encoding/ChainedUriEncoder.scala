package com.netaporter.uri.encoding

case class ChainedUriEncoder(encoders: Seq[UriEncoder]) extends UriEncoder {

  @deprecated("Being made `protected`.", "1.0.0")
  def shouldEncode(char: Char): Boolean = findFirstEncoder(char).nonEmpty

  @deprecated("Being made `protected`.", "1.0.0")
  def encodeChar(char: Char): String = findFirstEncoder(char).getOrElse(NoopEncoder).encodeChar(char)

  @deprecated("Being made `protected`.", "1.0.0")
  def findFirstEncoder(char: Char): Option[UriEncoder] = encoders.find(_.shouldEncode(char))

  override def +(encoder: UriEncoder): ChainedUriEncoder = copy(encoder +: encoders)
}
