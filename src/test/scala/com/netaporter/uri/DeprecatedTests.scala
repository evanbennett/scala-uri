package com.netaporter.uri

import com.netaporter.uri.decoding._
import com.netaporter.uri.dsl.{ encoderToChainedEncoder, uriToUriDsl, stringToUri, stringToUriDsl, queryParamToUriDsl, uriToString, UriDsl }
import com.netaporter.uri.encoding._
import com.netaporter.uri.parsing._
import Parameters._

class DeprecatedTests extends TestSpec {

  "`@deprecated` code" should "UriConfig.apply(userInfoEncoder: UriEncoder, pathEncoder: UriEncoder, queryEncoder: UriEncoder, fragmentEncoder: UriEncoder, userInfoDecoder: UriDecoder, pathDecoder: UriDecoder, queryDecoder: UriDecoder, fragmentDecoder: UriDecoder, matrixParams: Boolean, charset: String)" in {
    UriConfig.apply(PercentEncoder(PercentEncoder.USER_INFO_CHARS_TO_ENCODE),
                    PercentEncoder(PercentEncoder.PATH_CHARS_TO_ENCODE),
                    PercentEncoder(PercentEncoder.QUERY_CHARS_TO_ENCODE),
                    PercentEncoder(PercentEncoder.FRAGMENT_CHARS_TO_ENCODE),
                    PercentDecoder, PercentDecoder, PercentDecoder, PercentDecoder,
                    false, "UTF-8") shouldBe a[UriConfig]
  }

  it should "UriConfig.apply(encoder: UriEncoder, decoder: UriDecoder, matrixParams: Boolean, charset: String)" in {
    UriConfig.apply(NoopEncoder, NoopDecoder, false, "UTF-8") shouldBe a[UriConfig]
  }

  it should "UriConfig.apply(encoder: UriEncoder, decoder: UriDecoder, matrixParams: Boolean)" in {
    UriConfig.apply(NoopEncoder, NoopDecoder, false) shouldBe a[UriConfig]
  }

  it should "UriConfig.apply(encoder: UriEncoder, decoder: UriDecoder)" in {
    UriConfig.apply(NoopEncoder, NoopDecoder) shouldBe a[UriConfig]
  }

  it should "UriConfig.apply(decoder: UriDecoder, matrixParams: Boolean, charset: String)" in {
    UriConfig.apply(NoopDecoder, false, "UTF-8") shouldBe a[UriConfig]
  }

  it should "UriConfig.apply(decoder: UriDecoder, matrixParams: Boolean)" in {
    UriConfig.apply(NoopDecoder, false) shouldBe a[UriConfig]
  }

  it should "UriConfig.apply(matrixParams: Boolean, charset: String)" in {
    UriConfig.apply(false, "UTF-8") shouldBe a[UriConfig]
  }

  it should "`dsl.encoderToChainedEncoder`" in {
    val chainedUriEncoder: ChainedUriEncoder = PercentEncoder()
  }

  it should "`dsl.uriToString`" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    val uriString: String = uri
    uriString should equal(uri.toString)
  }

  it should "`UriDsl.*`" in {
    val uriDsl = new UriDsl(Uri("http://test.com?queryKey"))
    uriDsl / Uri("path/?queryKey2") should not equal (null)
    uriDsl ? Uri("?queryKey") should not equal (null)
    new UriDsl(Uri("http://test.com/")) `#` Uri("path#fragment") should not equal (null)
  }

  it should "`UriDecoder.decodeTuple`" in {
    PercentDecoder.decodeTuple("key" -> Some("value"), "?key=value") should equal(parameterToParam(PercentDecoder.decodeParameter(Parameter("key", "value"), "?key=value")(UriConfig.DEFAULT)))
  }

  it should "`encoding.percentEncode`" in {
    percentEncode should equal(PercentEncoder())
  }

  it should "`encoding.percentEncode(...)`" in {
    percentEncode('a', 'b', 'c') should equal(PercentEncoder('a', 'b', 'c'))
  }

  it should "`encoding.encodeCharAs`" in {
    encodeCharAs('a', "A") should equal(EncodeCharAs('a', "A"))
  }

  it should "`encoding.spaceAsPlus`" in {
    spaceAsPlus should equal(EncodeCharAs.SPACE_AS_PLUS)
  }

  it should "`UriEncoder.encode(s: String, charset: String)`" in {
    PercentEncoder().encode("s", "UTF-8") should equal("s")
  }

  it should "`NoopEncoder.encodeChar`" in {
    an [UnsupportedOperationException] should be thrownBy {
      NoopEncoder.encodeChar('e')
    }
  }

  it should "`PercentEncoder.ascii`" in {
    PercentEncoder().ascii('c') should equal(true)
  }

  it should "`UriParser.extractInt`" in {
    val uriParser = new DefaultUriParser("", UriConfig.DEFAULT)
    uriParser.extractInt("100") should equal(100)
  }

  it should "`UriParser.parse`" in {
    val uriString = "http://www.example.com:8080/"
    UriParser.parse(uriString, UriConfig.default) should equal(Uri(uriString)(UriConfig.DEFAULT))
  }

  it should "`UriParser.parse` `Failure(pe@ParseError`" in {
    a [java.net.URISyntaxException] should be thrownBy {
      UriParser.parse("http://test.net:8o8o", UriConfig.default)
    }
  }

// TODO: I cannot get this to fail:
//  it should "`UriParser.parseQuery` ParseError" in {
//    a [java.net.URISyntaxException] should be thrownBy {
//      UriParser.parseQuery("queryParam#Key=queryParam%Value", UriConfig.default)
//    }
//  }

  it should "`UriParser.parseQuery` exception" in {
    a [java.net.URISyntaxException] should be thrownBy {
      UriParser.parseQuery("?queryParamKey=queryParam%Value", UriConfig.default)
    }
  }

  it should "`Parameters.params`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.params should equal(segment.parameters.map(parameterToParam))
  }

  it should "`Parameters.params(String)`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.params("queryParamKey") should equal(segment.values("queryParamKey"))
  }

  it should "`Parameters.param`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.param("queryParamKey") should equal(segment.valueFirst("queryParamKey"))
  }

  it should "`Parameters.paramMap`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.paramMap should equal(segment.toMap)
  }

  it should "`Parameters.withParams`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    val parameters = Seq(Parameter("queryParamKey1", Some("queryParamValue1")))
    segment.withParams(parameters) should equal(segment.withParameters(parameters))
  }

  it should "`Parameters.addParam(String)`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.addParam("queryParamKey1") should equal(segment.append("queryParamKey1"))
  }

  it should "`Parameters.addParam(String, String)`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.addParam("queryParamKey1", "queryParamValue1") should equal(segment.append("queryParamKey1", "queryParamValue1"))
  }

  it should "`Parameters.addParam(String, Option[String])`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.addParam("queryParamKey1", Some("queryParamValue1")) should equal(segment.append("queryParamKey1", Some("queryParamValue1")))
  }

  it should "`Parameters.addParams(ParamSeq)`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    val parameters = Seq(Parameter("queryParamKey1", Some("queryParamValue1")))
    segment.addParams(parameters) should equal(segment.append(parameters))
  }

  it should "`Parameters.addParams(Parameters)`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.addParams(segment) should equal(segment.append(segment))
  }

  it should "`Parameters.mapParams`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.mapParams(p => p) should equal(segment.mapParameters(p => p))
  }

  it should "`Parameters.flatMapParams`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.flatMapParams(p => Seq(p)) should equal(segment.flatMapParameters(p => Seq(p)))
  }

  it should "`Parameters.mapParamNames`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.mapParamNames(a => a) should equal(segment.mapKeys(a => a))
  }

  it should "`Parameters.mapParamValues`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.mapParamValues(a => a) should equal(segment.mapValues(a => a))
  }

  it should "`Parameters.filterParams`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.filterParams(a => true) should equal(segment.filterParameters(a => true))
  }

  it should "`Parameters.filterParamsNames`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.filterParamsNames(a => true) should equal(segment.filterKeys(a => true))
  }

  it should "`Parameters.filterParamsValues`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")), Parameter("queryParamKey2", None))
    segment.filterParamsValues(a => false) should equal(segment.filterValues(a => false))
  }

  it should "`Parameters.filterParamsOptions`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.filterParamsOptions(a => true) should equal(segment.filterValues(a => true))
  }

  it should "`Parameters.replaceAll`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.replaceAll("queryParamKey", Some(true)) should equal(segment.replaceMatching("queryParamKey", Some(true)))
  }

  it should "`Parameters.removeAll(String)`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.removeAll("queryParamKey") should equal(segment.removeMatching("queryParamKey"))
  }

  it should "`Parameters.removeAll(Seq[String])`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.removeAll(Seq("queryParamKey")) should equal(segment.removeMatching(Seq("queryParamKey")))
  }

  it should "`Parameters.removeAll()`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    segment.removeAll should equal(segment.withParameters())
  }

  it should "`Parameters.paramsToString`" in {
    val segment = MatrixParametersSegment("segment", Parameter("queryParamKey", Some("queryParamValue")))
    "segment;" + segment.paramsToString(UriConfig.default.queryEncoder, UriConfig.default.charset.displayName) should equal(segment.toString(UriConfig.DEFAULT))
  }

  it should "`Query.copy`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.copy(Seq(Parameter("newQueryParamKey", Some("newQueryParamValue")))) should equal(query.withParameters(Seq(Parameter("newQueryParamKey", Some("newQueryParamValue")))))
  }

  it should "`Query.queryToString`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    query.queryToString(UriConfig.default) should equal(query.toString(UriConfig.DEFAULT))
  }

  it should "`StringQuery.params`" in {
    StringQuery("queryParam").params should equal(Seq.empty)
  }

  it should "`StringQuery.params(String)`" in {
    StringQuery("queryParam").params("") should equal(Seq.empty)
  }

  it should "`StringQuery.param`" in {
    StringQuery("queryParam").param("") should equal(None)
  }

  it should "`StringQuery.paramMap`" in {
    StringQuery("queryParam").paramMap should equal(Map.empty)
  }

  it should "`StringQuery.withParams`" in {
    val query = StringQuery("queryParam")
    query.withParams(Seq(Parameter("newQueryParamKey", Some("newQueryParamValue")))) should equal(query.withParameters(Seq(Parameter("newQueryParamKey", Some("newQueryParamValue")))))
  }

  it should "`StringQuery.addParam(String)`" in {
    val query = StringQuery("queryParam")
    query.addParam("newQueryParamKey") should equal(query.append(Parameter("newQueryParamKey")))
  }

  it should "`StringQuery.addParam(String, String)`" in {
    val query = StringQuery("queryParam")
    query.addParam("newQueryParamKey", "newQueryParamValue") should equal(query.append(Parameter("newQueryParamKey", "newQueryParamValue")))
  }

  it should "`StringQuery.addParam(String, Option[String])`" in {
    val query = StringQuery("queryParam")
    query.addParam("newQueryParamKey", Some("newQueryParamValue")) should equal(query.append(Parameter("newQueryParamKey", Some("newQueryParamValue"))))
  }

  it should "`StringQuery.addParams(ParamSeq)`" in {
    val query = StringQuery("queryParam")
    val params = Seq("newQueryParamKey" -> Some("newQueryParamValue"))
    query.addParams(params) should equal(query.append(paramsToSeqParameter(params)))
  }

  it should "`StringQuery.addParams(Parameters)`" in {
    val query = StringQuery("queryParam")
    val other = ParameterQuery(Parameter("newQueryParamKey", "newQueryParamValue"))
    query.addParams(other) should equal(query.append(other))
  }

  it should "`StringQuery.paramsToString`" in {
    StringQuery("queryParam").paramsToString(PercentEncoder(PercentEncoder.CharsetsToEncode.QUERY), "UTF-8") should equal("")
  }

  it should "`EmptyQuery.queryToString`" in {
    ("?" + EmptyQuery.queryToString(UriConfig.default)) should equal(EmptyQuery.toString(UriConfig.DEFAULT))
  }

  it should "`QueryString.apply`" in {
    QueryString(Seq("queryParamKey" -> Some("queryParamValue"))) should equal(Query(Parameter("queryParamKey", Some("queryParamValue"))))
  }

  it should "`QueryString.create`" in {
    QueryString.create("queryParamKey" -> Some("queryParamValue")) should equal(Query(Parameter("queryParamKey", Some("queryParamValue"))))
  }

  it should "`QueryString.unapply`" in {
    val query = ParameterQuery(Parameter("queryParamKey", Some("queryParamValue")))
    QueryString.unapply(query) should equal(ParameterQuery.unapply(query).map(seqParametersToParamSeq(_)))
  }

  it should "`QueryString.unapply` with `null`" in {
    QueryString.unapply(null) should equal(ParameterQuery.unapply(null))
  }

  it should "`Segment.part`" in {
    val segment = StringSegment("segment")
    segment.part should equal(segment.segment)
  }

  it should "`Segment.map`" in {
    val segment = StringSegment("segment")
    segment.map(s => s) should equal(segment.mapSegment(s => s))
  }

  it should "`Segment.addParam`" in {
    val segment = StringSegment("segment")
    val parameter = Parameter("queryParamKey1", Some("queryParamValue1"))
    segment.addParam(parameter) should equal(segment.append(parameter))
  }

  it should "`Segment.partToString`" in {
    val segment = StringSegment("segment")
    segment.partToString(UriConfig.default) should equal(segment.toString(UriConfig.DEFAULT))
  }

  it should "`StringSegment.params`" in {
    val segment = StringSegment("segment")
    segment.params should equal(Seq.empty)
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
    MatrixParams("path", Seq.empty) should equal(MatrixParametersSegment("path"))
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
    EmptyReference.matrixParams should equal(EmptyReference.matrixParametersOfLastSegment)
  }

  it should "`Uri.queryValue`" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path", Seq(Parameter("parameterKey", "parameterValue")))), None, None)
    uri.queryValue should equal(uri.query.getOrElse(EmptyQuery))
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
    uri.pathRaw should equal(uri.pathToString(UriConfig.DEFAULT.withNoEncoding))
  }

  it should "`Uri.queryStringRaw`" in {
    val uri = Uri.parse("?queryParamKey=queryParamValue")
    uri.queryStringRaw should equal(uri.queryToString(UriConfig.DEFAULT.withNoEncoding))
  }

  it should "`Uri.toStringRaw`" in {
    implicit val config = UriConfig(registeredNameMustBeDomainName = false)
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.exämple.com"), None, None, None)
    uri.toStringRaw should equal("http://www.exämple.com")
  }

  it should "`Uri.apply` old case class generated" in {
    Uri(Some("http"), None, None, Some("test.com"), None, Seq.empty, null, Some("fragment")) should equal(Uri(Scheme.option("http"), Authority.option(registeredName = "test.com"), None, None, Fragment.option("fragment")))
  }

  it should "`Uri.parseQuery`" in {
    Uri.parseQuery("?queryParamKey=queryParamValue") should equal(Uri.parse("?queryParamKey=queryParamValue").query.get)
  }

  it should "`Uri.parseQuery` without starting '?'" in {
    Uri.parseQuery("queryParamKey=queryParamValue") should equal(Uri.parse("?queryParamKey=queryParamValue").query.get)
  }
}
