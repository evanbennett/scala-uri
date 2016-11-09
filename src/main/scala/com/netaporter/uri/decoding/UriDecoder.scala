package com.netaporter.uri.decoding

import com.netaporter.uri._

abstract class UriDecoder {

  protected def _decode(s: String, originalInput: String)(implicit config: UriConfig): String

  def decode(s: String, originalInput: String)(implicit config: UriConfig): String = {
    if (!config.percentEncodingNormalization) s
    else _decode(s, originalInput)
  }

  def decodeParameter(parameter: Parameter, originalInput: String)(implicit config: UriConfig): Parameter =
    Parameter(decode(parameter.key, originalInput), parameter.value.map(decode(_, originalInput)))

  @deprecated("Use `decodeParameter` instead.", "1.0.0")
  def decodeTuple(kv: Parameters.Param, originalInput: String): Parameters.Param =
    decode(kv._1, originalInput)(UriConfig.default) -> kv._2.map(decode(_, originalInput)(UriConfig.default))
}
