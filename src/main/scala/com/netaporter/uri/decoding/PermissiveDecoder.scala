package com.netaporter.uri.decoding

class PermissiveDecoder(child: UriDecoder) extends UriDecoder {
  def decode(s: String) = {
    try {
      child.decode(s)
    } catch {
      case _: Throwable => s
    }
  }
}

object PermissivePercentDecoder extends PermissiveDecoder(PercentDecoder)
