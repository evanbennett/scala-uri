package com.netaporter.uri

import com.netaporter.uri.config.UriConfig

sealed abstract case class Fragment(fragment: String) {

  def copy(fragment: String = fragment): Fragment = Fragment(fragment)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit c: UriConfig): String =
    "#" + c.fragmentEncoder.encode(fragment, c.charset)

  def toStringRaw(implicit c: UriConfig): String = toString(c.withNoEncoding)
}

// TODO: Make `Fragment` abstract, and add `StringFragment` and `ParameterFragment`? GitHub Issue 14; https://en.wikipedia.org/wiki/Fragment_identifier

object Fragment {

  def apply(fragment: String): Fragment = {
    if (fragment == null) throw new IllegalArgumentException("`fragment` cannot be `null`.")
    if (fragment == "") EmptyFragment else new Fragment(fragment) {}
  }

  def option(fragment: String): Option[Fragment] = {
    if (fragment == null) None else Option(apply(fragment))
  }
}

object EmptyFragment extends Fragment("")
