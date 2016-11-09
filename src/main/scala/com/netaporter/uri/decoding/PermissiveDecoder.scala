package com.netaporter.uri.decoding

/**
 * Attempts to decode, but when an error occurs, the original input is returned rather than failing.
 */
class PermissiveDecoder(child: UriDecoder) extends UriDecoder {

  protected def _decode(s: String, originalInput: String)(implicit config: com.netaporter.uri.UriConfig): String = {
    try {
      child.decode(s, originalInput)
    } catch {
      case _: Throwable => s
    }
  }
}

object PermissivePercentDecoder extends PermissiveDecoder(PercentDecoder)
