package com.netaporter.uri

import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.decoding._
import com.netaporter.uri.dsl._
import com.netaporter.uri.encoding._
import com.netaporter.uri.parsing._
import Parameters._

class DeprecatedTests extends TestSpec {

  "`@deprecated` code" should "`dsl.encoderToChainedEncoder`" in {
    val chainedUriEncoder: ChainedUriEncoder = PercentEncoder.default
  }

  it should "`UriDecoder.decodeTuple`" in {
    PercentDecoder.decodeTuple("key" -> Some("value"), "?key=value") should equal(parameterToParam(PercentDecoder.decodeParameter(Parameter("key", "value"), "?key=value")))
  }

  it should "`encoding.percentEncode`" in {
    percentEncode should equal(PercentEncoder.default)
  }

  it should "`encoding.percentEncode(...)`" in {
    percentEncode('a', 'b', 'c') should equal(PercentEncoder('a', 'b', 'c'))
  }

  it should "`encoding.encodeCharAs`" in {
    encodeCharAs('a', "A") should equal(EncodeCharAs('a', "A"))
  }

  it should "`encoding.spaceAsPlus`" in {
    spaceAsPlus should equal(EncodeCharAs.spaceAsPlus)
  }

  it should "`PercentEncoder.ascii`" in {
    PercentEncoder.default.ascii('c') should equal(true)
  }

// TODO: I could not get this to fail:
//  it should "`UriParser.parseQuery` ParseError" in {
//    intercept[java.net.URISyntaxException] {
//      println("WOW:" + UriParser.parseQuery("queryParam#Key=queryParamValue", UriConfig.default) + ":")
//    }
//  }

  it should "`UriParser.parseQuery` exception" in {
    intercept[java.net.URISyntaxException] {
      UriParser.parseQuery("?queryParamKey=queryParam%Value", UriConfig.default)
    }
  }

  it should "`Parameters.params`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.params should equal(query.parameters.map(parameterToParam))
  }

  it should "`Parameters.params(String)`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.params("queryParamKey") should equal(query.values("queryParamKey"))
  }

  it should "`Parameters.param`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.param("queryParamKey") should equal(query.valueFirst("queryParamKey"))
  }

  it should "`Parameters.paramMap`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.paramMap should equal(query.toMap)
  }

  it should "`Parameters.withParams`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    val parameters = Seq(Parameter("queryParamKey1", Some("queryParamValue1")))
    query.withParams(parameters) should equal(query.withParameters(parameters))
  }

  it should "`Parameters.addParam(String)`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.addParam("queryParamKey1") should equal(query.append("queryParamKey1"))
  }

  it should "`Parameters.addParam(String, String)`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.addParam("queryParamKey1", "queryParamValue1") should equal(query.append("queryParamKey1", "queryParamValue1"))
  }

  it should "`Parameters.addParam(String, Option[String])`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.addParam("queryParamKey1", Some("queryParamValue1")) should equal(query.append("queryParamKey1", Some("queryParamValue1")))
  }

  it should "`Parameters.addParams(ParamSeq)`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    val parameters = Seq(Parameter("queryParamKey1", Some("queryParamValue1")))
    query.addParams(parameters) should equal(query.append(parameters))
  }

  it should "`Parameters.addParams(Parameters)`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.addParams(query) should equal(query.appendParameters(query))
  }

  it should "`Parameters.mapParams`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.mapParams(p => p) should equal(query.mapParameters(p => p))
  }

  it should "`Parameters.flatMapParams`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.flatMapParams(p => Seq(p)) should equal(query.flatMapParameters(p => Seq(p)))
  }

  it should "`Parameters.mapParamNames`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.mapParamNames(a => a) should equal(query.mapKeys(a => a))
  }

  it should "`Parameters.mapParamValues`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.mapParamValues(a => a) should equal(query.mapValues(a => a))
  }

  it should "`Parameters.filterParams`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.filterParams(a => true) should equal(query.filterParameters(a => true))
  }

  it should "`Parameters.filterParamsNames`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.filterParamsNames(a => true) should equal(query.filterKeys(a => true))
  }

  it should "`Parameters.filterParamsValues`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")), Parameter("queryParamKey2", None))
    query.filterParamsValues(a => false) should equal(query.filterValues(a => false))
  }

  it should "`Parameters.filterParamsOptions`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.filterParamsOptions(a => true) should equal(query.filterValues(a => true))
  }

  it should "`Parameters.replaceAll`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.replaceAll("queryParamKey", Some(true)) should equal(query.replaceMatching("queryParamKey", Some(true)))
  }

  it should "`Parameters.removeAll(String)`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.removeAll("queryParamKey") should equal(query.removeMatching("queryParamKey"))
  }

  it should "`Parameters.removeAll(Seq[String])`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.removeAll(Seq("queryParamKey")) should equal(query.removeMatching(Seq("queryParamKey")))
  }

  it should "`Parameters.removeAll()`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.removeAll should equal(query.withParameters())
  }

  it should "`Parameters.paramsToString`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    "?" + query.paramsToString(UriConfig.default.queryEncoder, UriConfig.default.charset) should equal(query.toString(UriConfig.default))
  }

  it should "`Query.queryToString`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.queryToString(UriConfig.default) should equal(query.toString(UriConfig.default))
  }

  it should "`EmptyQuery.queryToString`" in {
    ("?" + EmptyQuery.queryToString(UriConfig.default)) should equal(EmptyQuery.toString(UriConfig.default))
  }

  it should "`QueryString.apply`" in {
    QueryString(Seq("queryParamKey" -> Some("queryParamValue"))) should equal(Query(Parameter("queryParamKey", Some("queryParamValue"))))
  }

  it should "`QueryString.create`" in {
    QueryString.create("queryParamKey" -> Some("queryParamValue")) should equal(Query(Parameter("queryParamKey", Some("queryParamValue"))))
  }

  it should "`QueryString.unapply`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    QueryString.unapply(query) should equal(Query.unapply(query).map(seqParametersToParamSeq))
  }

  it should "`QueryString.unapply` with `null`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    QueryString.unapply(null) should equal(Query.unapply(null))
  }

  it should "`Segment.part`" in {
    val segment = StringSegment("segment")
    segment.part should equal(segment.segment)
  }

  it should "`Segment.map`" in {
    val segment = StringSegment("segment")
    segment.map(s => s) should equal(segment.mapSegments(s => s))
  }

  it should "`Segment.addParam`" in {
    val segment = StringSegment("segment")
    val parameter = Parameter("queryParamKey1", Some("queryParamValue1"))
    segment.addParam(parameter) should equal(segment.append(parameter))
  }

  it should "`Segment.partToString`" in {
    val segment = StringSegment("segment")
    segment.partToString(UriConfig.default) should equal(segment.toString(UriConfig.default))
  }

  it should "`StringSegment.params`" in {
    val segment = StringSegment("segment")
    segment.params should equal(segment.parameters)
  }

  it should "`PathPart.apply` as `StringSegment`" in {
    PathPart("path") should equal(Segment("path"))
  }

  it should "`PathPart.apply` as `MatrixParametersSegment`" in {
    PathPart("path", Seq("matrixParamKey" -> Some("matrixParamValue"))) should equal(Segment("path", Parameter("matrixParamKey", Some("matrixParamValue"))))
  }

  it should "`StringPathPart.apply`" in {
    StringPathPart("path") should equal(StringSegment("path"))
  }

  it should "`StringPathPart.unapply`" in {
    val segment = StringSegment("segment")
    StringPathPart.unapply(segment) should equal(StringSegment.unapply(segment))
  }

  it should "`StringPathPart.unapply` with `null`" in {
    StringPathPart.unapply(null) should equal(StringSegment.unapply(null))
  }

  it should "`MatrixParams.apply`" in {
    MatrixParams("path") should equal(MatrixParametersSegment("path"))
  }

  it should "`MatrixParams.unapply`" in {
    val segment = MatrixParametersSegment("segment")
    MatrixParams.unapply(segment) should equal(MatrixParametersSegment.unapply(segment).map(t => (t._1, t._2.map(parameterToParam))))
  }

  it should "`MatrixParams.unapply` with `null`" in {
    MatrixParams.unapply(null) should equal(MatrixParametersSegment.unapply(null))
  }

  it should "`Uri.protocol`" in {
    val uri = Uri(Scheme.option("http"), None, AbsolutePath.option(StringSegment("path")), None, None)
    uri.protocol.value should equal(uri.scheme.value.scheme)
  }

  it should "`Uri.pathParts`" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path")), None, None)
    uri.pathParts should equal(uri.pathSegments)
  }

  it should "`Uri.pathPartOption`" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path")), None, None)
    uri.pathPartOption("path") should equal(uri.pathSegmentOption("path"))
  }

  it should "`Uri.pathPart`" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path")), None, None)
    uri.pathPart("path") should equal(uri.pathSegment("path"))
  }

  it should "`Uri.matrixParams` without" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path")), None, None)
    uri.matrixParams should equal(uri.matrixParametersOfLastSegment)
  }

  it should "`Uri.matrixParams` with" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path", Seq(Parameter("parameterKey", "parameterValue")))), None, None)
    uri.matrixParams should equal(uri.matrixParametersOfLastSegment.map(parameterToParam))
  }

  it should "`Uri.matrixParams` empty" in {
    EmptyRelativeReference.matrixParams should equal(EmptyRelativeReference.matrixParametersOfLastSegment)
  }

  it should "`Uri.queryValue`" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path", Seq(Parameter("parameterKey", "parameterValue")))), None, None)
    uri.queryValue should equal(uri.query.getOrElse(EmptyQueryString))
  }

  it should "`Uri.fragmentString`" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path")), None, Fragment.option("fragment"))
    uri.fragmentString.value should equal(uri.fragment.value.fragment)
  }

  it should "`Uri.addMatrixParam(String, String, String)`" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path")), None, None)
    uri.addMatrixParam("path", "key", "value") should equal(uri.appendMatrixParameter("path", "key", "value"))
  }

  it should "`Uri.addMatrixParam(String, String)`" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path")), None, None)
    uri.addMatrixParam("key", "value") should equal(uri.appendMatrixParameterToLastSegment("key", "value"))
  }

  it should "`Uri.addParam`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.addParam("key", "value") should equal(uri.queryAppend("key", "value"))
  }

  it should "`Uri.addParams`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.addParams(Seq("key" -> "value")) should equal(uri.queryAppend(Seq("key" -> "value")))
  }

  it should "`Uri.mapQuery`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.mapQuery(a => a) should equal(uri.queryMapParameters(a => a))
  }

  it should "`Uri.flatMapQuery`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.flatMapQuery(a => Seq(a)) should equal(uri.queryFlatMapParameters(a => Seq(a)))
  }

  it should "`Uri.mapQueryNames`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.mapQueryNames(a => a) should equal(uri.queryMapKeys(a => a))
  }

  it should "`Uri.mapQueryValues`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.mapQueryValues(a => a) should equal(uri.queryMapValues(a => a))
  }

  it should "`Uri.filterQuery`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.filterQuery(a => true) should equal(uri.queryFilterParameters(a => true))
  }

  it should "`Uri.filterQueryNames`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.filterQueryNames(a => true) should equal(uri.queryFilterKeys(a => true))
  }

  it should "`Uri.filterQueryValues`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.filterQueryValues(a => false) should equal(uri.queryFilterValues(a => false))
  }

  it should "`Uri.replaceParams`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.replaceParams("key", "value") should equal(uri.queryReplaceMatching("key", "value"))
  }

  it should "`Uri.replaceAllParams`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.replaceAllParams("key" -> Some("value")) should equal(uri.withQuery(Parameter("key", "value")))
  }

  it should "`Uri.removeParams`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.removeParams("key") should equal(uri.queryRemoveMatching("key"))
  }

  it should "`Uri.removeParams` set" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.removeParams(Seq("key")) should equal(uri.queryRemoveMatching(Seq("key")))
  }

  it should "`Uri.removeAllParams`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.removeAllParams() should equal(uri.withQuery())
  }

  it should "`Uri.copyOld`" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.copyOld() should equal(uri)
  }

  it should "`Uri.pathRaw`" in {
    val uri = Uri.parse("/path")
    uri.pathRaw should equal(uri.pathToStringRaw)
  }

  it should "`Uri.queryString`" in {
    val uri = Uri.parse("?queryParamKey=queryParamValue")
    uri.queryString should equal(uri.queryToString)
  }

  it should "`Uri.queryStringRaw`" in {
    val uri = Uri.parse("?queryParamKey=queryParamValue")
    uri.queryStringRaw should equal(uri.queryToStringRaw)
  }

  it should "`Uri.apply` old case class generated" in {
    Uri(Some("http"), None, None, Some("test.com"), None, Seq.empty, null, Some("fragment")) should equal(Uri(Scheme.option("http"), Authority.option(host = "test.com"), None, None, Fragment.option("fragment")))
  }

  it should "`Uri.parseQuery`" in {
    Uri.parseQuery("?queryParamKey=queryParamValue") should equal(Uri.parse("?queryParamKey=queryParamValue").query.get)
  }

  it should "`Uri.parseQuery` without starting '?'" in {
    Uri.parseQuery("queryParamKey=queryParamValue") should equal(Uri.parse("?queryParamKey=queryParamValue").query.get)
  }
}
