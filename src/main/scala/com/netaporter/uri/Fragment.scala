package com.netaporter.uri

/**
 * URI Fragment, based on RFC 3986 section 3.5.
 *
 * @param fragment the fragment contents
 */
sealed abstract case class Fragment(fragment: String) {

  def copy(fragment: String = fragment): Fragment = Fragment(fragment)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit config: UriConfig): String =
    "#" + config.fragmentEncoder.encode(fragment)
}

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
