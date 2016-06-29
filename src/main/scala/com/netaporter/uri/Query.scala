package com.netaporter.uri

import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.Parameters._

sealed abstract case class Query(parameters: Seq[Parameter]) extends Parameters {

  type Self = Query

  val separator = "&"

  def withParameters(newParameters: Seq[Parameter]): Query = Query(newParameters)

  def copy(parameters: Seq[Parameter] = parameters): Query = Query(parameters)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  @deprecated("Use `toString` instead. Not compatible when `parameters.isEmpty`.", "1.0.0")
  def queryToString(c: UriConfig): String =
    if (parameters.isEmpty) "" else toString(c)

  /**
   * TODO: Old behaviour has been changed, because you should now use `None` to represent no query:
   *   When `params` was empty, an empty string was returned rather than "?".
   */
  def toString(implicit c: UriConfig): String =
    "?" + toString(c.queryEncoder, c.charset)

  def toStringRaw(implicit c: UriConfig): String = toString(c.withNoEncoding)
}

object Query {

  def apply(parameters: Seq[Parameter]): Query = {
    if (parameters == null) throw new IllegalArgumentException("`parameters` cannot be `null`.")
    if (parameters.isEmpty) EmptyQuery else new Query(parameters) {}
  }

  def apply(parameters: Parameter*)(implicit di: DummyImplicit): Query = apply(parameters)

  def option(parameters: Seq[Parameter]): Option[Query] = {
    if (parameters == null) None else Option(apply(parameters))
  }

  def option(parameters: Parameter*)(implicit di: DummyImplicit): Option[Query] = option(parameters)
}

object EmptyQuery extends Query(Seq.empty)

object QueryString {

  @deprecated("Use `Query.apply` instead.", "1.0.0")
  def apply(params: ParamSeq) = Query(params)

  @deprecated("Use `Query.apply` instead.", "1.0.0")
  def create(params: Param*) = Query(params)

  @deprecated("Use `Query.unapply` instead.", "1.0.0")
  def unapply(queryString: QueryString): Option[(ParamSeq)] =
    if (queryString == null) None else Some((queryString.params))
}

@deprecated("Use `EmptyQuery` instead.", "1.0.0")
object EmptyQueryString extends Query(Seq.empty)
