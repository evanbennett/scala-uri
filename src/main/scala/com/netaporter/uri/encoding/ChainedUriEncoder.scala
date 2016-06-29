package com.netaporter.uri.encoding

case class ChainedUriEncoder(encoders: Seq[UriEncoder]) extends UriEncoder {

  def shouldEncode(char: Char): Boolean = findFirstEncoder(char).isDefined

  def encodeChar(char: Char): String = findFirstEncoder(char).getOrElse(NoopEncoder).encodeChar(char)

  // TODO: I think this method should be `protected`, but I would like to check if anyone is using it:
  protected def findFirstEncoder(char: Char): Option[UriEncoder] = encoders.find(_.shouldEncode(char))

  override def +(encoder: UriEncoder): ChainedUriEncoder = copy(encoders = encoder +: encoders)
}
