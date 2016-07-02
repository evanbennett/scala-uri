package com.netaporter.uri

import com.netaporter.uri.config.UriConfig

sealed abstract case class Scheme(scheme: String) {

  def copy(scheme: String = scheme): Scheme = Scheme(scheme)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit c: UriConfig): String =
    scheme.toLowerCase + ":" // TODO: According to the RFC section 3.1 this should be presented in lower-case.

  def toStringRaw(implicit c: UriConfig): String =
    scheme + ":"
}

object Scheme {

  private val REGEX = """[a-zA-Z][-+\.a-zA-Z0-9]+""".r

  def apply(scheme: String): Scheme = {
    if (scheme == null || scheme.isEmpty) throw new IllegalArgumentException("`scheme` cannot be `null` and cannot be empty.")
    // TODO: This stops invalid content from being input via calls other than from the parser.
    scheme match {
      case REGEX() => new Scheme(scheme) {}
      case _ => throw new IllegalArgumentException("`scheme` is not valid.")
    }
  }

  def option(scheme: String): Option[Scheme] =
    if (scheme == null || scheme.isEmpty) None else Option(apply(scheme))
}
