package com.netaporter.uri

import com.netaporter.uri.Parameters._
import scala.collection.GenTraversableOnce

/**
 * URI Query, based on RFC 3986 section 3.4.
 * A `Query` can have a `queryString` OR `parameters`, but not both.
 */
sealed abstract class Query {

  @deprecated("Ensure you have a `ParametersQuery` and then use `parameters` instead.", "1.0.0")
  def params: ParamSeq

  def withQueryString(queryString: String = ""): Query = Query(queryString)

  def withParameters(newParameters: Seq[Parameter] = Seq.empty): ParameterQuery = ParameterQuery(newParameters)

  @deprecated("Use `withParameters` instead.", "1.0.0")
  def copy(params: ParamSeq): Query = withParameters(params)

  def append(parameter: Parameter): Query

  def append(parameters: Seq[Parameter]): Query

  def append(other: Parameters): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `values` instead.", "1.0.0")
  def params(key: String): Seq[Option[String]]

  @deprecated("Ensure you have a `ParametersQuery` and then use `valueFirst` instead.", "1.0.0")
  def param(key: String): Option[String]

  @deprecated("Ensure you have a `ParametersQuery` and then use `toMap` instead.", "1.0.0")
  def paramMap: Map[String, Seq[String]]

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  @deprecated("Ensure you have a `ParametersQuery` and then use `withParameters(Seq[Parameter])` instead.", "1.0.0")
  def withParams(params: ParamSeq): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `append` instead.", "1.0.0")
  def addParam(k: String): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `append` instead.", "1.0.0")
  def addParam(k: String, v: String): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `append` instead.", "1.0.0")
  def addParam(k: String, v: Option[String]): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `append(Seq[Parameter])` instead.", "1.0.0")
  def addParams(kvs: ParamSeq): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `append` instead.", "1.0.0")
  def addParams(other: Parameters): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `mapParameters(Parameter => Parameter)` instead.", "1.0.0")
  def mapParams(f: Param => Param): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `flatMapParameters(Parameter => GenTraversableOnce[Parameter])` instead.", "1.0.0")
  def flatMapParams(f: Param => GenTraversableOnce[Param]): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `mapKeys` instead.", "1.0.0")
  def mapParamNames(f: String => String): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `mapValues(Option[String] => Option[String])` instead.", "1.0.0")
  def mapParamValues(f: String => String): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `filterParameters(Parameter => Boolean)` instead.", "1.0.0")
  def filterParams(f: Param => Boolean): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `filterKeys` instead.", "1.0.0")
  def filterParamsNames(f: String => Boolean): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `filterValues(Option[String] => Boolean)` instead.", "1.0.0")
  def filterParamsValues(f: String => Boolean): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `filterValues` instead.", "1.0.0")
  def filterParamsOptions(f: Option[String] => Boolean): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `replaceMatching` instead.", "1.0.0")
  def replaceAll(key: String, vOpt: Option[Any]): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `removeMatching` instead.", "1.0.0")
  def removeAll(k: String): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `removeMatching` instead.", "1.0.0")
  def removeAll(a: Seq[String]): Query

  @deprecated("Ensure you have a `ParametersQuery` and then use `withParameters()` instead.", "1.0.0")
  def removeAll(): Query

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  @deprecated("Use `toString` instead.", "1.0.0")
  def paramsToString(e: encoding.UriEncoder, charset: String): String

  @deprecated("Use `toString` instead. Not compatible when `parameters.isEmpty`.", "1.0.0")
  def queryToString(c: UriConfig): String = {
    toString(c) match {
      case "?" => ""
      case queryString => queryString
    }
  }

  /**
   * THEON: Old behaviour has been changed, because you should now use `None` to represent no query:
   *   When `params` was empty, an empty string was returned rather than "?".
   */
  def toString(implicit config: UriConfig): String
}

object Query {

  def apply(queryString: String): Query =
    if (queryString != null && queryString.isEmpty) EmptyQuery else StringQuery(queryString)

  def apply(parameters: Seq[Parameter]): Query = ParameterQuery(parameters)

  def apply(parameters: Parameter*)(implicit di: DummyImplicit): Query = apply(parameters)

  def option(queryString: String): Option[Query] =
    if (queryString != null && queryString.isEmpty) Option(EmptyQuery) else StringQuery.option(queryString)

  def option(parameters: Seq[Parameter]): Option[Query] = ParameterQuery.option(parameters)

  def option(parameters: Parameter*)(implicit di: DummyImplicit): Option[Query] = option(parameters)

  def unapply(query: Query): Option[(Option[String], Option[Seq[Parameter]])] = {
    query match {
      case EmptyQuery => Option(None, None)
      case query: StringQuery => Option(Option(query.queryString), None)
      case query: ParameterQuery => Option(None, Option(query.parameters))
      case _ => None
    }
  }
}

/**
 * A `Query` with no `parameters`.
 */
sealed abstract case class StringQuery(queryString: String) extends Query  {

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def params = Vector.empty

  /** Convert the existing `queryString` into a `Parameter` (key) and then append the new `parameter`. */
  def append(parameter: Parameter): Query = Query(Parameter(queryString), parameter)

  /** Convert the existing `queryString` into a `Parameter` (key) and then append the new `parameters`. */
  def append(parameters: Seq[Parameter]): Query = Query(Parameter(queryString) +: parameters)

  /** Convert the existing `queryString` into a `Parameter` (key) and then append the new parameters from `other`. */
  def append(other: Parameters): Query = Query(Parameter(queryString) +: other.parameters)

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def params(key: String): Seq[Option[String]] = Seq.empty

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def param(key: String): Option[String] = None

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  lazy val paramMap: Map[String, Seq[String]] = Map.empty

  @deprecated("Use 'append' instead.", "1.0.0")
  def withParams(params: ParamSeq): Query = withParameters(paramsToSeqParameter(params))

  @deprecated("Use 'append' instead.", "1.0.0")
  def addParam(k: String): Query = append(Parameter(k))

  @deprecated("Use 'append' instead.", "1.0.0")
  def addParam(k: String, v: String): Query = append(Parameter(k, v))

  @deprecated("Use 'append' instead.", "1.0.0")
  def addParam(k: String, v: Option[String]): Query = append(Parameter(k, v))

  @deprecated("Use 'append' instead.", "1.0.0")
  def addParams(kvs: ParamSeq): Query = append(paramsToSeqParameter(kvs))

  @deprecated("Use 'append' instead.", "1.0.0")
  def addParams(other: Parameters): Query = append(other)

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def mapParams(f: Param => Param): Query = this

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def flatMapParams(f: Param => GenTraversableOnce[Param]): Query = this

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def mapParamNames(f: String => String): Query = this

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def mapParamValues(f: String => String): Query = this

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def filterParams(f: Param => Boolean): Query = this

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def filterParamsNames(f: String => Boolean): Query = this

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def filterParamsValues(f: String => Boolean): Query = this

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def filterParamsOptions(f: Option[String] => Boolean): Query = this

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def replaceAll(key: String, vOpt: Option[Any]): Query = this

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def removeAll(k: String): Query = this

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def removeAll(a: Seq[String]): Query = this

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def removeAll(): Query = this

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  @deprecated("Is only relavent to `ParameterQuery` and is being removed.", "1.0.0")
  def paramsToString(e: encoding.UriEncoder, charset: String): String = ""

  def toString(implicit config: UriConfig): String = "?" + config.queryEncoder.encode(queryString)
}

object StringQuery {

  def apply(queryString: String): StringQuery = {
    if (queryString == null || queryString.isEmpty) throw new IllegalArgumentException("`queryString` cannot be `null` or empty.")
    new StringQuery(queryString) {}
  }

  def option(queryString: String): Option[StringQuery] =
    if (queryString == null || queryString.isEmpty) None else Option(apply(queryString))
}

sealed abstract case class ParameterQuery(parameters: Seq[Parameter]) extends Query with Parameters {

  type Self = ParameterQuery

  val separator = "&"

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit config: UriConfig): String = "?" + paramString(config.queryEncoder)
}

object ParameterQuery {

  def apply(parameters: Seq[Parameter]): ParameterQuery = {
    if (parameters == null) throw new IllegalArgumentException("`parameters` cannot be `null`.")
    if (parameters.isEmpty) EmptyQuery else new ParameterQuery(parameters) {}
  }

  def apply(parameters: Parameter*)(implicit di: DummyImplicit): ParameterQuery = apply(parameters)

  def option(parameters: Seq[Parameter]): Option[ParameterQuery] =
    if (parameters == null) None else Option(apply(parameters))

  def option(parameters: Parameter*)(implicit di: DummyImplicit): Option[ParameterQuery] = option(parameters)

  // NOTE: `unapply` returns `Some(Seq.empty)` for `EmptyQuery`.
}

/**
 * NOTE: This should extend `Query` not `ParameterQuery`, but the `Parameters` methods will not allow this.
 *       In particular, the `withParameters` method must be able to handle `Seq.empty`, while still returning a `Parameters` instance.
 */
object EmptyQuery extends ParameterQuery(Seq.empty)

@deprecated("Use `Query` instead.", "1.0.0")
object QueryString {

  @deprecated("Use `Query.apply` instead.", "1.0.0")
  def apply(params: ParamSeq) = Query(params)

  @deprecated("Use `Query.apply` instead.", "1.0.0")
  def create(params: Param*) = Query(params)

  @deprecated("Use `ParameterQuery.unapply` instead.", "1.0.0")
  def unapply(queryString: QueryString): Option[(ParamSeq)] = {
    queryString match {
      case query: ParameterQuery => Option(query.parameters)
      case _ => None
    }
  }
}
