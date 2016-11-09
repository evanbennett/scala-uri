package com.netaporter.uri

import com.netaporter.uri.parsing.BaseParser.SCHEME_SCHEME

/** URI Scheme, based on RFC 3986 section 3.1. */
sealed abstract case class Scheme(scheme: String) {

  def toString(implicit config: UriConfig): String =
    (if (config.caseNormalization) scheme.toLowerCase else scheme) + ":"
}

object Scheme {

  def apply(scheme: String): Scheme = {
    if (scheme == null || scheme.isEmpty) throw new IllegalArgumentException("`scheme` cannot be `null` and cannot be empty.")
    // THEON: This stops invalid content from being input via calls other than from the parser.
    scheme match {
      case SCHEME_SCHEME(_) => new Scheme(scheme) {}
      case _ => throw new IllegalArgumentException("`scheme` is not valid.")
    }
  }

  def option(scheme: String): Option[Scheme] =
    if (scheme == null || scheme.isEmpty) None else Option(apply(scheme))
}
