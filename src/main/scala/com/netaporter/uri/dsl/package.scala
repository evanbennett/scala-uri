package com.netaporter.uri

import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.encoding.{ChainedUriEncoder, UriEncoder}

package object dsl {

  import scala.language.implicitConversions

  implicit def uriToUriDsl(uri: Uri) = new UriDsl(uri)

  @deprecated("Not needed anymore. `+` method added to `UriEncoder`", "1.0.0")
  implicit def encoderToChainedEncoder(enc: UriEncoder) = ChainedUriEncoder(enc :: Nil)

  implicit def stringToUri(s: String)(implicit c: UriConfig = UriConfig.default) = Uri.parse(s)
  implicit def stringToUriDsl(s: String)(implicit c: UriConfig = UriConfig.default) = new UriDsl(stringToUri(s))

  implicit def queryParamToUriDsl(keyValue: (String, Any))(implicit c: UriConfig = UriConfig.default) = new UriDsl(EmptyRelativeReference.queryAppend(keyValue._1, keyValue._2))

  implicit def uriToString(uri: Uri)(implicit c: UriConfig = UriConfig.default): String = uri.toString
}
