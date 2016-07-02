package com.netaporter.uri

import scala.collection.GenTraversableOnce
import scala.language.implicitConversions
import com.netaporter.uri.encoding.UriEncoder
import com.netaporter.uri.Parameters._

/**
 * Trait use to represent a list of key value parameters, such as query parameters and matrix parameters
 */
trait Parameters {

  type Self <: Parameters

  def separator: String

  @deprecated("Use `parameters` instead.", "1.0.0")
  def params: ParamSeq = parameters.map(parameter => (parameter.key, parameter.value))

  def parameters: Seq[Parameter]

  @deprecated("Use `values` instead.", "1.0.0")
  def params(key: String): Seq[Option[String]] = values(key)

  def values(existingKey: String): Seq[Option[String]] = parameters.collect {
    case Parameter(key, value) if key == existingKey => value
  }

  @deprecated("Use `valueFirst` instead.", "1.0.0")
  def param(key: String): Option[String] = valueFirst(key)

  def valueFirst(existingKey: String): Option[String] = parameters.collectFirst {
    case Parameter(key, Some(value)) if key == existingKey => value
  }

  @deprecated("Use `toMap` instead.", "1.0.0")
  lazy val paramMap: Map[String, Seq[String]] = toMap

  lazy val toMap: Map[String, Seq[String]] = parameters.foldLeft(Map.empty[String, Seq[String]]) { (previousMap, parameter) =>
    val previousValues = previousMap.getOrElse(parameter.key, Seq.empty)
    previousMap + (parameter.key -> parameter.value.fold(previousValues)(previousValues :+ _))
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  @deprecated("Use `withParameters(Seq[Parameter])` instead.", "1.0.0")
  def withParams(params: ParamSeq): Self =
    withParameters(paramsToSeqParameter(params))

  def withParameters(newParameters: Seq[Parameter] = Seq.empty): Self

  /**
   * Adds a new parameter key with no value.
   *
   * @return A new instance with the new parameter added
   */
  @deprecated("Use `append` instead.", "1.0.0")
  def addParam(k: String): Self = append(k)

  /**
   * Adds a new parameter key-value pair.
   *
   * @return A new instance with the new parameter added
   */
  @deprecated("Use `append` instead.", "1.0.0")
  def addParam(k: String, v: String): Self = append(k, v)

  /**
   * Append a new parameter with key and value.
   *
   * @return A new instance with the new parameter added
   */
  def append(key: String, value: String): Self = append(Parameter(key, value))

  /**
   * Adds a new parameter key-value pair. If the value for the parameter is None, then this
   * parameter will be rendered without an = sign (use Some("") if this is not what you want).
   *
   * @return A new instance with the new parameter added
   */
  @deprecated("Use `append` instead.", "1.0.0")
  def addParam(k: String, v: Option[String]): Self = append(k, v)

  /**
   * Append a new parameter with key and value. If the value for the parameter is None, then this
   * parameter will be rendered without an = sign (use Some("") if this is not what you want).
   *
   * @return A new instance with the new parameter added
   */
  def append(key: String, value: Option[String] = None): Self = append(Parameter(key, value))

  def append(parameter: Parameter): Self = withParameters(parameters :+ parameter)

  @deprecated("Use `append(Seq[Parameter])` instead.", "1.0.0")
  def addParams(kvs: ParamSeq): Self =
    append(paramsToSeqParameter(kvs))

  def append(parameters: Seq[Parameter]): Self = withParameters(this.parameters ++ parameters)

  @deprecated("Use `appendParameters` instead.", "1.0.0")
  def addParams(other: Parameters): Self = appendParameters(other)

  def appendParameters(other: Parameters): Self = withParameters(parameters ++ other.parameters)

  /**
   * Transforms each parameter by applying the specified Function
   */
  @deprecated("Use `mapParameters(Parameter => Parameter)` instead.", "1.0.0")
  def mapParams(f: Param => Param): Self =
    withParameters(parameters.map(parameter => paramToParameter(f((parameter.key, parameter.value)))))

  /**
   * Transforms each parameter by applying the specified Function
   */
  def mapParameters(f: Parameter => Parameter): Self = withParameters(parameters.map(f))

  /**
   * Transforms each parameter by applying the specified Function
   *
   * @param f A function that returns a collection of Parameters when applied to each parameter
   */
  @deprecated("Use `flatMapParameters(Parameter => GenTraversableOnce[Parameter])` instead.", "1.0.0")
  def flatMapParams(f: Param => GenTraversableOnce[Param]): Self =
    withParameters(parameters.flatMap(parameter => f((parameter.key, parameter.value)).toSeq.map(param => Parameter(param._1, param._2))))

  /**
   * Transforms each parameter by applying the specified Function
   *
   * @param f A function that returns a collection of Parameters when applied to each parameter
   */
  def flatMapParameters(f: Parameter => GenTraversableOnce[Parameter]): Self =
    withParameters(parameters.flatMap(f))

  @deprecated("Use `mapKeys` instead.", "1.0.0")
  def mapParamNames(f: String => String) = mapKeys(f)

  /**
   * Transforms each parameter key by applying the specified Function
   */
  def mapKeys(f: String => String): Self =
    withParameters(parameters.map(_.mapKey(f)))

  /**
   * Transforms each parameter value by applying the specified Function
   *
   * NOTE: This ignores `None` values, so you CANNOT transform them.
   */
  @deprecated("Use `mapValues(Option[String] => Option[String])` instead.", "1.0.0")
  def mapParamValues(f: String => String): Self =
    withParameters(parameters.map(parameter => Parameter(parameter.key, parameter.value.map(f))))

  /**
   * Transforms each parameter value by applying the specified Function
   */
  def mapValues(f: Option[String] => Option[String]): Self =
    withParameters(parameters.map(_.mapValue(f)))

  /**
   * Filters out just the parameters for which the provided function holds true
   *
   * TODO: This description was wrong. Copied correct description from `Uri` for non-deprecated function.
   */
  @deprecated("Use `filterParameters(Parameter => Boolean)` instead.", "1.0.0")
  def filterParams(f: Param => Boolean): Self = withParameters(parameters.filter(parameter => f((parameter.key, parameter.value))))

  /**
   * Removes any parameters that return false when applied to the given Function.
   */
  def filterParameters(f: Parameter => Boolean): Self = withParameters(parameters.filter(f))

  /**
   * Filters out just the parameters for which the provided function holds true when applied to the parameter name
   *
   * TODO: This description was wrong. Copied correct description from `Uri` for non-deprecated function.
   */
  @deprecated("Use `filterKeys` instead.", "1.0.0")
  def filterParamsNames(f: String => Boolean) = filterKeys(f)

  /**
   * Removes any parameters that return false when their key is applied to the given Function.
   */
  def filterKeys(f: String => Boolean): Self =
    withParameters(parameters.filter(parameter => f(parameter.key)))

  /**
   * Filters out just the parameters for which the provided function holds true when applied to the parameter value
   *
   * TODO: This description was wrong. Copied correct description from `Uri` for non-deprecated function.
   *
   * NOTE: This removes parameters with a value of `None`.
   */
  @deprecated("Use `filterValues(Option[String] => Boolean)` instead.", "1.0.0")
  def filterParamsValues(f: String => Boolean): Self =
    filterValues(valueOption => valueOption match {
      case Some(value) => f(value)
      case _ => false
    })

  /**
   * Filters out just the parameters for which the provided function holds true when applied to the parameter value
   *
   * TODO: This description was wrong. Copied correct description from `Uri` for non-deprecated function.
   */
  @deprecated("Use `filterValues` instead.", "1.0.0")
  def filterParamsOptions(f: Option[String] => Boolean): Self = filterValues(f)

  /**
   * Removes any parameters that return false when their value is applied to the given Function.
   */
  def filterValues(f: Option[String] => Boolean): Self =
    withParameters(parameters.filter(parameter => f(parameter.value)))

  /**
   * Replaces the existing Query parameters with the specified key with a single Query parameter
   * with the specified value.
   *
   * @param key Key for the Query parameter(s) to replace
   * @param value value to replace with
   * @return A new Query with the result of the replace
   */
  @deprecated("Use `replaceMatching` instead.", "1.0.0")
  def replaceAll(key: String, value: Option[Any]): Self =
    withParameters(parameters.filterNot(_.key == key) :+ Parameter(key, value.map(_.toString)))

  /**
   * Removes the existing query parameters with the specified key and appends a new query parameter
   * with the specified value.
   *
   * TODO: Old behaviour has been removed, that was not expected:
   *   If the `existingKey` did not exist, a new parameter is appended without any being removed.
   *
   * @param existingKey Key for the Query parameter(s) to replace
   * @param newValue value to replace with
   * @return A new Query with the result of the replace
   */
  def replaceMatching(existingKey: String, newValue: Option[Any]): Self = {
    val filteredParameters = parameters.filterNot(_.key == existingKey)
    if (parameters.length == filteredParameters.length) withParameters(parameters) // TODO: `withParameters(parameters)` should be `this`, but I could not get it to work.
    else withParameters(filteredParameters :+ Parameter(existingKey, newValue.map(_.toString)))
  }

  /**
   * Removes the Query parameters with the specified key.
   *
   * @param key Key for the Query parameter(s) to remove
   */
  @deprecated("Use `removeMatching` instead.", "1.0.0")
  def removeAll(key: String): Self = filterKeys(_ != key)

  def removeMatching(existingKey: String): Self = filterKeys(_ != existingKey)

  @deprecated("Use `removeMatching` instead.", "1.0.0")
  def removeAll(keys: Seq[String]): Self = filterKeys(!keys.contains(_))

  def removeMatching(existingKeys: Seq[String]): Self = filterKeys(!existingKeys.contains(_))

  @deprecated("Use `withParameters()` instead.", "1.0.0")
  def removeAll(): Self = withParameters()

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  @deprecated("Use `toString` instead.", "1.0.0")
  def paramsToString(e: UriEncoder, charset: String): String = toString(e, charset)

  protected def toString(encoder: UriEncoder, charset: String): String =
    parameters.map(_.toString(encoder, charset)).mkString(separator)
}

object Parameters {

  @deprecated("Use `Parameter` instead.", "1.0.0")
  type Param = (String, Option[String])
  @deprecated("Use `Seq[Parameter]` instead.", "1.0.0")
  type ParamSeq = Seq[Param]

  @deprecated("Only for use while using depracated `Param`.", "1.0.0")
  implicit def paramToParameter(param: Param): Parameter = Parameter(param._1, param._2)

  @deprecated("Only for use while using depracated `ParamSeq`.", "1.0.0")
  implicit def paramsToSeqParameter(params: ParamSeq): Seq[Parameter] = params.map(param => Parameter(param._1, param._2))

  @deprecated("Only for use while using depracated `Param`.", "1.0.0")
  implicit def parameterToParam(parameter: Parameter): Param = (parameter.key, parameter.value)

  @deprecated("Only for use while using depracated `ParamSeq`.", "1.0.0")
  implicit def seqParametersToParamSeq(parameters: Seq[Parameter]): ParamSeq = parameters.map(parameter => (parameter.key, parameter.value))
}
