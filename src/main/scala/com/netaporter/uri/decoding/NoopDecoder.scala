package com.netaporter.uri.decoding

object NoopDecoder extends UriDecoder {

  protected def _decode(s: String, originalInput: String)(implicit config: com.netaporter.uri.UriConfig): String = s
}
