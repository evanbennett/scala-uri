package com.netaporter.uri

/**
 * TODO: This comment can be removed with the deprecation:
 * Regular Expression used to replace existing `"???" -> Some("???")`:
 *   ("[^"]+") -> ("[^"]*"|None|Some\("[^"]*"\)|Option\("[^"]*"\))
 *   Parameter($1, $2)
 */
sealed abstract case class Parameter(key: String, value: Option[String]) {

  def mapKey(f: String => String): Parameter = Parameter(f(key), value)

  def mapValue(f: Option[String] => Option[String]): Parameter = Parameter(key, f(value))

  def copy(key: String = key, value: Option[String] = value): Parameter = Parameter(key, value)

  def withValue(newValue: Any): Parameter = Parameter(key, newValue)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(encoder: encoding.UriEncoder)(implicit config: UriConfig): String =
    encoder.encode(key) + value.map("=" + encoder.encode(_)).getOrElse("")
}

object Parameter {

  def apply(key: String, value: Option[String] = None): Parameter = {
    if (key == null) throw new IllegalArgumentException("`key` cannot be `null`.")
    if (value == null) throw new IllegalArgumentException("`value` cannot be `null`.")
    new Parameter(key, value) {}
  }

  def apply(key: String, value: Any): Parameter = {
    val newValue = value match {
      case null | None => None
      case Some(value) => Option(value.toString)
      case value => Option(value.toString)
    }
    apply(key, newValue)
  }
}
