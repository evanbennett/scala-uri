package com.netaporter.uri.decoding

class PermissiveDecoder(child: UriDecoder) extends UriDecoder {

  def decode(s: String, originalInput: String): String = {
    try {
      child.decode(s, originalInput)
    } catch {
      case _: Throwable => s
    }
  }
}

object PermissivePercentDecoder extends PermissiveDecoder(PercentDecoder)
