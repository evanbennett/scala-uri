package com.netaporter.uri

import com.netaporter.uri.config.UriConfig

sealed abstract case class Scheme(scheme: String) {

  def copy(scheme: String = scheme): Scheme = Scheme(scheme)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit c: UriConfig): String =
    scheme.toLowerCase + ":" // TODO: According to the RFC this should be presented in lower-case.

  def toStringRaw(implicit c: UriConfig): String =
    scheme + ":"
}

object Scheme {

  def apply(scheme: String): Scheme = {
    if (scheme == null || scheme.isEmpty) throw new IllegalArgumentException("`scheme` cannot be `null` and cannot be empty.")
    new Scheme(scheme) {}
  }

  def option(scheme: String): Option[Scheme] =
    if (scheme == null || scheme.isEmpty) None else Option(apply(scheme))
}
