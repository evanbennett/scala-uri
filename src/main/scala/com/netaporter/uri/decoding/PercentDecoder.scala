package com.netaporter.uri.decoding

object PercentDecoder extends UriDecoder {

  protected def _decode(s: String, originalInput: String)(implicit config: com.netaporter.uri.UriConfig): String = {
    // THEON: Previously, decoding did NOT fail when there was a '%' at the end of `s`:
    if (s.endsWith("%")) throw new java.net.URISyntaxException(originalInput, "Encountered '%' at the end of the string to decode. It looks like this URI isn't Percent Encoded. If so, look at using the NoopDecoder")
    try {
      val segments = s.split('%')
      val decodedSegments = segments.tail.flatMap(seg => {
        val percentByte = Integer.parseInt(seg.substring(0, 2), 16).toByte
        percentByte +: seg.substring(2).getBytes(config.charset)
      })
      segments.head + new String(decodedSegments, config.charset)
    } catch {
      case e: NumberFormatException => throw new java.net.URISyntaxException(originalInput, "Encountered '%' followed by a non hex number. It looks like this URI isn't Percent Encoded. If so, look at using the NoopDecoder")
    }
  }

  override def hashCode: Int = 105899

  override def toString = "PercentDecoder()"
}
