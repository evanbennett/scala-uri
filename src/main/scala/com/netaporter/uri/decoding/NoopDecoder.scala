package com.netaporter.uri.decoding

/**
 * Date: 28/08/2013
 * Time: 20:58
 */
object NoopDecoder extends UriDecoder {
  def decode(s: String) = s
}
