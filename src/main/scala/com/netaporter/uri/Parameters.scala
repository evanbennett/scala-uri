package com.netaporter.uri

import scala.collection.GenTraversableOnce
import scala.language.implicitConversions
import com.netaporter.uri.Parameters._

/**
 * Trait used to represent a list of key value parameters. (e.g. query parameters and matrix parameters)
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
   * Append a new parameter key with no value.
   *
   * @return A new instance with the new parameter added
   */
  @deprecated("Use `append` instead.", "1.0.0")
  def addParam(k: String): Self = append(k)

  /**
   * Append a new parameter key-value pair.
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
  def append(key: String, value: Any = null): Self = append(Parameter(key, value))

  /**
   * Append a new parameter key-value pair. If the value for the parameter is None, then this
   * parameter will be rendered without an = sign (use Some("") if this is not what you want).
   *
   * @return A new instance with the new parameter added
   */
  @deprecated("Use `append` instead.", "1.0.0")
  def addParam(k: String, v: Option[String]): Self = append(k, v)

  def append(parameter: Parameter): Self = withParameters(parameters :+ parameter)

  @deprecated("Use `append(Seq[Parameter])` instead.", "1.0.0")
  def addParams(kvs: ParamSeq): Self =
    append(paramsToSeqParameter(kvs))

  def append(parameters: Seq[Parameter]): Self = withParameters(this.parameters ++ parameters)

  @deprecated("Use `append` instead.", "1.0.0")
  def addParams(other: Parameters): Self = append(other)

  def append(other: Parameters): Self = withParameters(parameters ++ other.parameters)

  /**
   * Transforms each parameter by applying the specified function.
   *
   * @param f A function that returns the new parameter when applied to each parameter
   */
  @deprecated("Use `mapParameters(Parameter => Parameter)` instead.", "1.0.0")
  def mapParams(f: Param => Param): Self =
    withParameters(parameters.map(parameter => paramToParameter(f((parameter.key, parameter.value)))))

  /**
   * Transform each parameter by applying the specified function.
   *
   * @param f A function that returns the new parameter when applied to each parameter
   */
  def mapParameters(f: Parameter => Parameter): Self = withParameters(parameters.map(f))

  /**
   * Transforms each parameter by applying the specified function.
   *
   * @param f A function that returns the new parameter(s) when applied to each parameter
   */
  @deprecated("Use `flatMapParameters(Parameter => GenTraversableOnce[Parameter])` instead.", "1.0.0")
  def flatMapParams(f: Param => GenTraversableOnce[Param]): Self =
    withParameters(parameters.flatMap(parameter => f((parameter.key, parameter.value)).toSeq.map(param => Parameter(param._1, param._2))))

  /**
   * Transform each parameter by applying the specified function.
   *
   * @param f A function that returns the new parameter(s) when applied to each parameter
   */
  def flatMapParameters(f: Parameter => scala.collection.GenTraversableOnce[Parameter]): Self =
    withParameters(parameters.flatMap(f))

  @deprecated("Use `mapKeys` instead.", "1.0.0")
  def mapParamNames(f: String => String) = mapKeys(f)

  /**
   * Transform each parameter key by applying the specified function.
   *
   * @param f A function that returns the new parameter key when applied to each parameter key
   */
  def mapKeys(f: String => String): Self =
    withParameters(parameters.map(_.mapKey(f)))

  /**
   * Transforms each parameter value by applying the specified function
   * NOTE: This ignores `None` values, so you CANNOT transform them.
   *
   * @param f A function that returns the new parameter value when applied to each parameter value
   */
  @deprecated("Use `mapValues(Option[String] => Option[String])` instead.", "1.0.0")
  def mapParamValues(f: String => String): Self =
    withParameters(parameters.map(parameter => Parameter(parameter.key, parameter.value.map(f))))

  /**
   * Transform each parameter value by applying the specified function.
   *
   * @param f A function that returns the new parameter value when applied to each parameter value
   */
  def mapValues(f: Option[String] => Option[String]): Self =
    withParameters(parameters.map(_.mapValue(f)))

  /**
   * Filters out just the parameters for which the provided function holds true
   *
   * THEON: This description was wrong. Copied correct description from `Uri` for non-deprecated function.
   */
  @deprecated("Use `filterParameters(Parameter => Boolean)` instead.", "1.0.0")
  def filterParams(f: Param => Boolean): Self = withParameters(parameters.filter(parameter => f((parameter.key, parameter.value))))

  /**
   * Remove any parameters that return false when applied to the given function.
   */
  def filterParameters(f: Parameter => Boolean): Self = withParameters(parameters.filter(f))

  /**
   * Filters out just the parameters for which the provided function holds true when applied to the parameter name
   *
   * THEON: This description was wrong. Copied correct description from `Uri` for non-deprecated function.
   */
  @deprecated("Use `filterKeys` instead.", "1.0.0")
  def filterParamsNames(f: String => Boolean) = filterKeys(f)

  /**
   * Remove any parameters that return false when their key is applied to the given function.
   */
  def filterKeys(f: String => Boolean): Self =
    withParameters(parameters.filter(parameter => f(parameter.key)))

  /**
   * Filters out just the parameters for which the provided function holds true when applied to the parameter value
   * NOTE: This removes parameters with a value of `None`.
   *
   * THEON: This description was wrong. Copied correct description from `Uri` for non-deprecated function.
   */
  @deprecated("Use `filterValues(Option[String] => Boolean)` instead.", "1.0.0")
  def filterParamsValues(f: String => Boolean): Self =
    filterValues(valueOption => valueOption match {
      case None => false
      case Some(value) => f(value)
    })

  /**
   * Filters out just the parameters for which the provided function holds true when applied to the parameter value
   *
   * THEON: This description was wrong. Copied correct description from `Uri` for non-deprecated function.
   */
  @deprecated("Use `filterValues` instead.", "1.0.0")
  def filterParamsOptions(f: Option[String] => Boolean): Self = filterValues(f)

  /**
   * Remove any parameters that return false when their value is applied to the given function.
   */
  def filterValues(f: Option[String] => Boolean): Self =
    withParameters(parameters.filter(parameter => f(parameter.value)))

  /**
   * Replaces the existing parameters with the specified key with a single parameter with the specified value.
   *
   * @param key Key for the Query parameter(s) to replace
   * @param value value to replace with
   * @return A new Query with the result of the replace
   */
  @deprecated("Use `replaceMatching` instead.", "1.0.0")
  def replaceAll(key: String, vOpt: Option[Any]): Self = replaceMatching(key, vOpt)

  /**
   * Remove the existing parameters with the specified key and append a new parameter with the specified value.
   *
   * @param existingKey Key for the parameter(s) to replace
   * @param newValue Value to replace with
   * @return A new Query with the result of the replace
   */
  def replaceMatching(existingKey: String, newValue: Any): Self =
    withParameters(parameters.filterNot(_.key == existingKey) :+ Parameter(existingKey, newValue))

  /**
   * Removes the Query parameters with the specified key.
   *
   * @param key Key for the Query parameter(s) to remove
   */
  @deprecated("Use `removeMatching` instead.", "1.0.0")
  def removeAll(k: String): Self = filterKeys(_ != k)

  def removeMatching(existingKey: String): Self = filterKeys(_ != existingKey)

  @deprecated("Use `removeMatching` instead.", "1.0.0")
  def removeAll(a: Seq[String]): Self = filterKeys(!a.contains(_))

  def removeMatching(existingKeys: Seq[String]): Self = filterKeys(!existingKeys.contains(_))

  @deprecated("Use `withParameters()` instead.", "1.0.0")
  def removeAll(): Self = withParameters()

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  @deprecated("Use `toString` instead.", "1.0.0")
  def paramsToString(e: encoding.UriEncoder, charset: String): String = paramString(e)(UriConfig(charset = java.nio.charset.Charset.forName(charset)))

  protected def paramString(encoder: encoding.UriEncoder)(implicit config: UriConfig): String =
    parameters.map(_.toString(encoder)).mkString(separator)
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
