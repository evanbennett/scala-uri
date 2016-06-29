package com.netaporter.uri.decoding

import com.netaporter.uri._

abstract class UriDecoder {

  def decode(s: String, originalInput: String): String

  def decodeParameter(param: Parameter, originalInput: String): Parameter =
    Parameter(decode(param.key, originalInput), param.value.map(decode(_, originalInput)))

  @deprecated("Use `decodeParameter` instead.", "1.0.0")
  def decodeTuple(param: Parameters.Param, originalInput: String): Parameters.Param =
    decode(param._1, originalInput) -> param._2.map(decode(_, originalInput))
}
