package com.netaporter.uri

class QueryTests extends TestSpec {

  "`Uri.queryString`" should "return the query string" in {
    val uri = Uri(None, None, None, Query.option("queryParamKey1=queryParamValue1&queryParamKey2="), None)
    uri.queryString.value should equal("queryParamKey1=queryParamValue1&queryParamKey2=")
  }

  it should "return `None` the query has parameters" in {
    Uri(None, None, None, Query.option(Parameter("queryParamKey1", Some("queryParamValue1")), Parameter("queryParamKey2", Some("")), Parameter("queryParamKey3", None), Parameter("queryParamKey4", Some("queryParamValue4"))), None).queryString should equal(None)
  }

  it should "return `None` when the query is `EmptyReference`" in {
    EmptyReference.queryString should equal(None)
  }

  "`Uri.queryParameters`" should "return all the query parameters" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey1", Some("queryParamValue1")), Parameter("queryParamKey2", Some("")), Parameter("queryParamKey3", None), Parameter("queryParamKey4", Some("queryParamValue4"))), None)
    uri.queryParameters.value should equal(Seq(Parameter("queryParamKey1", Some("queryParamValue1")), Parameter("queryParamKey2", Some("")), Parameter("queryParamKey3", None), Parameter("queryParamKey4", Some("queryParamValue4"))))
  }

  it should "return `None` when the query is empty" in {
    Uri(None, None, None, Option(EmptyQuery), None).queryParameters should equal(None)
  }

  it should "return `None` when the query is `EmptyReference`" in {
    EmptyReference.queryParameters should equal(None)
  }

  "`Uri.queryValues(String)`" should "return the matching query parameters" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey1", Some("queryParamValue1")), Parameter("queryParamKey2", Some("")), Parameter("queryParamKey1", None), Parameter("queryParamKey4", Some("queryParamValue4"))), None)
    uri.queryValues("queryParamKey1") should equal(Seq(Some("queryParamValue1"), None))
  }

  it should "return empty when the query has no matching parameters" in {
    Uri(None, None, None, Option(EmptyQuery), None).queryValues("queryParamKey") should equal(Seq.empty)
  }

  it should "return empty when the query is `None`" in {
    EmptyReference.queryValues("queryParamKey") should equal(Seq.empty)
  }

  "`Uri.queryValueFirst(String)`" should "return the first matching query parameter" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey1", Some("queryParamValue1")), Parameter("queryParamKey2", Some("")), Parameter("queryParamKey1", None), Parameter("queryParamKey4", Some("queryParamValue4"))), None)
    uri.queryValueFirst("queryParamKey1") should equal(Some("queryParamValue1"))
  }

  it should "return `None` when the query has no matching parameters" in {
    Uri(None, None, None, Option(EmptyQuery), None).queryValueFirst("queryParamKey") should equal(None)
  }

  it should "return `None` when the query is `None`" in {
    EmptyReference.queryValueFirst("queryParamKey1") should equal(None)
  }

  "`Uri.queryMap`" should "return all the query parameters" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey1", Some("queryParamValue1")), Parameter("queryParamKey1", Some("")), Parameter("queryParamKey3", None), Parameter("queryParamKey4", Some("queryParamValue4"))), None)
    uri.queryMap should equal(Map(
      "queryParamKey1" -> Seq("queryParamValue1", ""),
      "queryParamKey3" -> Seq.empty,
      "queryParamKey4" -> Seq("queryParamValue4")
    ))
  }

  it should "return empty when the query is empty" in {
    Uri(None, None, None, Option(EmptyQuery), None).queryMap should equal(Map.empty)
  }

  it should "return empty when the query is `None`" in {
    EmptyReference.queryMap should equal(Map.empty)
  }

  "`Uri.withQuery`" should "change the query when provided a `Uri`" in {
    val query = Query.option(Parameter("queryParamKey", Some("queryParamValue")))
    EmptyReference.withQuery(Uri(None, None, None, query, None)).query should equal(query)
  }

  it should "change the query when provided a `Query`" in {
    val query = Query.option(Parameter("queryParamKey", Some("queryParamValue")))
    val uri = Uri(None, None, None, query, None)
    uri.query should equal(query)
    val newQuery = Query(Parameter("queryParamKeyNew", Some("queryParamValueNew")))
    uri.withQuery(newQuery).query.value should equal(newQuery)
  }

  it should "change the query when provided a `Parameter`" in {
    val query = Query.option(Parameter("queryParamKey", Some("queryParamValue")))
    val uri = Uri(None, None, None, query, None)
    uri.query should equal(query)
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    uri.withQuery(parameter1).query.value should equal(Query(parameter1))
  }

  it should "change the query when provided multiple `Parameter`" in {
    val query = Query.option(Parameter("queryParamKey", Some("queryParamValue")))
    val uri = Uri(None, None, None, query, None)
    uri.query should equal(query)
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    uri.withQuery(parameter1, parameter2, parameter3).query.value should equal(Query(parameter1, parameter2, parameter3))
  }

  it should "change the query when provided a `Seq[Parameter]`" in {
    val query = Query.option(Parameter("queryParamKey", Some("queryParamValue")))
    val uri = Uri(None, None, None, query, None)
    uri.query should equal(query)
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    uri.withQuery(Seq(parameter1, parameter2, parameter3)).query.value should equal(Query(parameter1, parameter2, parameter3))
  }

  it should "remove the query when provided nothing" in {
    val query = Query.option(Parameter("queryParamKey", Some("queryParamValue")))
    val uri = Uri(None, None, None, query, None)
    uri.query should equal(query)
    uri.withQuery().query should equal(None)
  }

  it should "not change the query when query was empty, and provided empty query" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, Option(EmptyQuery), None)
    uri.query.value should equal(EmptyQuery)
    uri.withQuery(EmptyQuery) should equal(uri)
  }

  it should "not change the query when query was `None`, and provided nothing" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.query should equal(None)
    uri.withQuery() should equal(uri)
  }

  "`Uri.queryAppend`" should "append a parameter key and value (None) to an existing query" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2), None)
    uri.queryAppend("queryParamKey3", None).query.value should equal(Query(parameter1, parameter2, parameter3))
  }

  it should "append a parameter key and value (null) to an existing query" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2), None)
    uri.queryAppend("queryParamKey3", null).query.value should equal(Query(parameter1, parameter2, parameter3))
  }

  it should "append a parameter key and value (int) to an existing query" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", Some("3"))
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2), None)
    uri.queryAppend("queryParamKey3", 3).query.value should equal(Query(parameter1, parameter2, parameter3))
  }

  it should "append a parameter key and value (boolean) to an existing query" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", Some("false"))
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2), None)
    uri.queryAppend("queryParamKey3", false).query.value should equal(Query(parameter1, parameter2, parameter3))
  }

  it should "append a parameter key to an existing query" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2), None)
    uri.queryAppend("queryParamKey3").query.value should equal(Query(parameter1, parameter2, parameter3))
  }

  it should "append a parameter key and value without an existing query" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    EmptyReference.queryAppend("queryParamKey1", Some("queryParamValue1")).query.value should equal(Query(parameter1))
  }

  it should "append a parameter sequence to an existing query" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1), None)
    uri.queryAppend(Seq(("queryParamKey2", Some("")), ("queryParamKey3", null))).query.value should equal(Query(parameter1, parameter2, parameter3))
  }

  it should "append a parameter sequence without an existing query" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter3 = Parameter("queryParamKey3", None)
    EmptyReference.queryAppend(Seq(("queryParamKey1", "queryParamValue1"), ("queryParamKey3", None))).query.value should equal(Query(parameter1, parameter3))
  }

  it should "append parameters to an existing query" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1), None)
    uri.queryAppend(parameter2, parameter3).query.value should equal(Query(parameter1, parameter2, parameter3))
  }

  it should "append parameter to an existing query" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2), None)
    uri.queryAppend(parameter3).query.value should equal(Query(parameter1, parameter2, parameter3))
  }

  it should "append parameters without an existing query" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    EmptyReference.queryAppend(parameter1, parameter2, parameter3).query.value should equal(Query(parameter1, parameter2, parameter3))
  }

  it should "append query parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2), None)
    uri.queryAppend(ParameterQuery(parameter2, parameter3)).queryParameters.value should equal(Seq(parameter1, parameter2, parameter2, parameter3))
  }

  it should "use the other query when called with an EmptyQuery" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val query = ParameterQuery(parameter1, parameter2)
    Uri(query = EmptyQuery).queryAppend(query).query.value should equal(query)
  }

  it should "use the other query when called on EmptyReference" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val query = ParameterQuery(parameter1, parameter2)
    EmptyReference.queryAppend(query).query.value should equal(query)
  }

  "`Uri.queryMapParameters`" should "transform all parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, ParameterQuery.option(parameter1, parameter2, parameter3), None)
    uri.queryMapParameters(parameter => parameter.withValue(parameter.value.map(_ + "Extension"))).query.value should equal(Query(Parameter("queryParamKey1", Some("queryParamValue1Extension")), Parameter("queryParamKey2", Some("Extension")), Parameter("queryParamKey3", None)))
  }

  it should "transform all parameters using `withValue(String)`" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, ParameterQuery.option(parameter1, parameter2, parameter3), None)
    uri.queryMapParameters(parameter => parameter.withValue(parameter.value.getOrElse("") + "Extension")).query.value should equal(Query(Parameter("queryParamKey1", Some("queryParamValue1Extension")), Parameter("queryParamKey2", Some("Extension")), Parameter("queryParamKey3", Some("Extension"))))
  }

  it should "transform parameters with Some() values by flipping the key and value" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some("queryParamValue2"))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, ParameterQuery.option(parameter1, parameter2, parameter3), None)
    uri.queryMapParameters {
      case Parameter(key, Some(value)) => Parameter(value, Some(key))
      case parameter => parameter
    }.query.value should equal(Query(Parameter("queryParamValue1", Some("queryParamKey1")), Parameter("queryParamValue2", Some("queryParamKey2")), Parameter("queryParamKey3", None)))
  }

  it should "change nothing when query was `None`" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1")), None, None)
    uri.queryMapParameters(parameter => parameter.withValue(parameter.value.getOrElse("") + "Extension")) should equal(uri)
  }

  "`Uri.queryFlatMapParameters`" should "transform all parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryFlatMapParameters(parameter => Seq(parameter.withValue(parameter.value.map(_ + "Extension")))).query.value should equal(Query(Parameter("queryParamKey1", Some("queryParamValue1Extension")), Parameter("queryParamKey2", Some("Extension")), Parameter("queryParamKey3", None)))
  }

  it should "change nothing when query was `None`" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1")), None, None)
    uri.queryFlatMapParameters(parameter => Seq(parameter.withValue(parameter.value + "Extension"))) should equal(uri)
  }

  "`Uri.queryMapKeys`" should "transform some parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryMapKeys(value => value.substring(10)).query.value should equal(Query(Parameter("Key1", Some("queryParamValue1")), Parameter("Key2", Some("")), Parameter("Key3", None)))
  }

  it should "change nothing when query was `None`" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1")), None, None)
    uri.queryMapKeys(value => value.substring(10)) should equal(uri)
  }

  "`Uri.queryMapValues`" should "transform some parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some("queryParamValue2"))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryMapValues(_.map(value => value.substring(10))).query.value should equal(Query(Parameter("queryParamKey1", Some("Value1")), Parameter("queryParamKey2", Some("Value2")), Parameter("queryParamKey3", None)))
  }

  it should "change nothing when query was `None`" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1")), None, None)
    uri.queryMapValues(_.map(value => value.substring(10))) should equal(uri)
  }

  "`Uri.queryFilterParameters`" should "filter some parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryFilterParameters {
      case Parameter(key, None) => key.length < 16
      case Parameter(key, Some(value)) => (key + value).length < 16
    }.query.value should equal(Query(parameter2, parameter3))
  }

  it should "filter all parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryFilterParameters(_ == false).query.value should equal(Query())
  }

  it should "change nothing when query was `None`" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1")), None, None)
    uri.queryFilterParameters(_ == false) should equal(uri)
  }

  "`Uri.queryFilterKeys`" should "filter some parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryFilterKeys(_ == "queryParamKey1").query.value should equal(Query(parameter1))
  }

  it should "change nothing when query was `None`" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1")), None, None)
    uri.queryFilterKeys(_ == "queryParamKey1") should equal(uri)
  }

  "`Uri.queryFilterValues`" should "filter some parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryFilterValues(_ == Some("")).query.value should equal(Query(parameter2))
  }

  it should "change nothing when query was `None`" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1")), None, None)
    uri.queryFilterValues(_ == Some("")) should equal(uri)
  }

  "`Uri.queryReplaceMatching`" should "replace matching parameter with string" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryReplaceMatching("queryParamKey1", "newValue").query.value should equal(Query(parameter2, parameter3, Parameter("queryParamKey1", Some("newValue"))))
  }

  it should "replace matching parameter with empty string" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryReplaceMatching("queryParamKey1", "").query.value should equal(Query(parameter2, parameter3, Parameter("queryParamKey1", Some(""))))
  }

  it should "replace matching parameter with int" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryReplaceMatching("queryParamKey1", 3).query.value should equal(Query(parameter2, parameter3, Parameter("queryParamKey1", Some("3"))))
  }

  it should "replace matching parameter with float" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryReplaceMatching("queryParamKey1", 0.5f).query.value should equal(Query(parameter2, parameter3, Parameter("queryParamKey1", Some("0.5"))))
  }

  it should "replace matching parameter with boolean" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryReplaceMatching("queryParamKey1", true).query.value should equal(Query(parameter2, parameter3, Parameter("queryParamKey1", Some("true"))))
  }

  it should "replace matching parameter with some string" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryReplaceMatching("queryParamKey1", Some("newValue")).query.value should equal(Query(parameter2, parameter3, Parameter("queryParamKey1", Some("newValue"))))
  }

  it should "replace matching parameter with `None`" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryReplaceMatching("queryParamKey1", None).query.value should equal(Query(parameter2, parameter3, Parameter("queryParamKey1", None)))
  }

  it should "replace matching parameter with `null`" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryReplaceMatching("queryParamKey1", null).query.value should equal(Query(parameter2, parameter3, Parameter("queryParamKey1", None)))
  }

  it should "replace multiple matching parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey1", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryReplaceMatching("queryParamKey1", 3).query.value should equal(Query(parameter2, Parameter("queryParamKey1", Some("3"))))
  }

  it should "change nothing when no parameter keys match" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryReplaceMatching("queryParamKey4", "").query.value should equal(Query(parameter1, parameter2, parameter3, Parameter("queryParamKey4", "")))
  }

  it should "change nothing when query was `None`" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1")), None, None)
    uri.queryReplaceMatching("queryParamKey2", "queryParamValue2") should equal(uri)
  }

  "`Uri.queryRemoveMatching(String)`" should "remove matching parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryRemoveMatching("queryParamKey2").query.value should equal(Query(parameter1, parameter3))
  }

  it should "remove multiple matching parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey2", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryRemoveMatching("queryParamKey2").query.value should equal(Query(parameter1))
  }

  it should "change nothing when query was `None`" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1")), None, None)
    uri.queryRemoveMatching("queryParamKey2") should equal(uri)
  }

  "`Uri.queryRemoveMatching(Seq[String])`" should "remove all matching parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryRemoveMatching(Seq("queryParamKey2", "queryParamKey3")).query.value should equal(Query(parameter1))
  }

  it should "change nothing when query was `None`" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1")), None, None)
    uri.queryRemoveMatching(Seq("queryParamKey2", "queryParamKey3")) should equal(uri)
  }

  "`Uri.queryRemoveAllParameters`" should "remove all parameters" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val parameter2 = Parameter("queryParamKey2", Some(""))
    val parameter3 = Parameter("queryParamKey3", None)
    val uri = Uri(None, None, None, Query.option(parameter1, parameter2, parameter3), None)
    uri.queryRemoveAllParameters.query.value should equal(Query())
  }

  it should "change nothing when query was `None`" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1")), None, None)
    uri.queryRemoveAllParameters should equal(uri)
  }

  "`Uri.queryToString` and therefore `Query.toString`" should "work with a single query parameter with key and value" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue"))), None)
    uri.queryToString should equal("?queryParamKey=queryParamValue")
  }

  it should "work with a single query parameter with key and empty value" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some(""))), None)
    uri.queryToString should equal("?queryParamKey=")
  }

  it should "work with a single query parameter with key and no value" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", None)), None)
    uri.queryToString should equal("?queryParamKey")
  }

  it should "work with multiple query parameters" in {
    val uri = Uri(None, None, None, Query.option(Parameter("queryParamKey", Some("queryParamValue")), Parameter("queryParamKey", Some("")), Parameter("queryParamKey", None)), None)
    uri.queryToString should equal("?queryParamKey=queryParamValue&queryParamKey=&queryParamKey")
  }

  it should "work without a query" in {
    EmptyReference.queryToString should equal("")
  }

  "`Query.withQueryString`" should "succeed with passed a string" in {
    val query = Query("queryParamKey=queryParamValue").withQueryString("newQuery")
    query shouldBe a[StringQuery]
    query.asInstanceOf[StringQuery].queryString should equal("newQuery")
  }

  it should "succeed with passed an empty string" in {
    Query("queryParamKey=queryParamValue").withQueryString("") should equal(EmptyQuery)
  }

  it should "succeed with passed nothing" in {
    Query("queryParamKey=queryParamValue").withQueryString() should equal(EmptyQuery)
  }

  "`Query.apply`" should "succeed when passed a string" in {
    val query = Query("queryParamKey=queryParamValue")
    query shouldBe a[StringQuery]
    query.asInstanceOf[StringQuery].queryString should equal("queryParamKey=queryParamValue")
  }

  it should "succeed when passed an empty string" in {
    Query("") should equal(EmptyQuery)
  }

  it should "succeed when passed a `Parameter`" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val query = Query(parameter1)
    query shouldBe a[ParameterQuery]
    query.asInstanceOf[ParameterQuery].parameters should equal(Seq(parameter1))
  }

  it should "succeed when passed `Parameter`s" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val query = Query(parameter1, Parameter("queryParamKey2"))
    query shouldBe a[ParameterQuery]
    query.asInstanceOf[ParameterQuery].parameters should equal(Seq(parameter1, Parameter("queryParamKey2")))
  }

  it should "succeed when passed an empty sequence" in {
    Query(Seq.empty) should equal(EmptyQuery)
  }

  it should "fail when passed `null: String`" in {
    an [IllegalArgumentException] should be thrownBy {
      Query(null: String)
    }
  }

  it should "fail when passed `null: Seq[Parameter]`" in {
    an [IllegalArgumentException] should be thrownBy {
      Query(null: Seq[Parameter])
    }
  }

  "`Query.option`" should "return Some when passed a string" in {
    val query = Query.option("queryParamKey=queryParamValue").value
    query shouldBe a[StringQuery]
    query.asInstanceOf[StringQuery].queryString should equal("queryParamKey=queryParamValue")
  }

  it should "return None when passed an empty string" in {
    Query.option("").value should equal(EmptyQuery)
  }

  it should "return None when passed `null: String`" in {
    Query.option(null: String) should equal(None)
  }

  it should "return Some when passed a `Parameter`" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val query = Query.option(parameter1).value
    query shouldBe a[ParameterQuery]
    query.asInstanceOf[ParameterQuery].parameters should equal(Seq(parameter1))
  }

  it should "return Some when passed `Parameter`s" in {
    val parameter1 = Parameter("queryParamKey1", Some("queryParamValue1"))
    val query = Query.option(parameter1).value
    query shouldBe a[ParameterQuery]
    query.asInstanceOf[ParameterQuery].parameters should equal(Seq(parameter1))
  }

  it should "return Some(EmptyQuery) when passed an empty sequence" in {
    Query.option(Seq.empty).value should equal(EmptyQuery)
  }

  it should "return None when passed `null: Seq[Parameter]`" in {
    Query.option(null: Seq[Parameter]) should equal(None)
  }

  "`Query.unapply`" should "succeed with `StringQuery`" in {
    val query = Query("queryParamKey=queryParamValue")
    Query.unapply(query).value should equal((Some("queryParamKey=queryParamValue"), None))
  }

  it should "succeed with `ParameterQuery`" in {
    val query = Query(Parameter("queryParamKey", Some("queryParamValue")))
    Query.unapply(query).value should equal((None, Some(Seq(Parameter("queryParamKey", Some("queryParamValue"))))))
  }

  it should "succeed with `EmptyQuery`" in {
    Query.unapply(EmptyQuery).value should equal((None, None))
  }

  it should "succeed with `null`" in {
    Query.unapply(null) should equal(None)
  }

  "`StringQuery.append`" should "succeed with a `Parameter`" in {
    val query = Query("queryParamKey=queryParamValue")
    val parameter = Parameter("queryParamKey", Some("queryParamValue"))
    val newQuery = query.append(parameter)
    newQuery shouldBe a[ParameterQuery]
    newQuery.asInstanceOf[ParameterQuery].parameters should equal(Seq(Parameter("queryParamKey=queryParamValue", None), parameter))
  }

  it should "succeed with a `Seq[Parameter]`" in {
    val query = Query("queryParamKey")
    val parameters = Seq(Parameter("queryParamKey1", Some("queryParamValue1")), Parameter("queryParamKey2", Some("")), Parameter("queryParamKey3", None))
    val newQuery = query.append(parameters)
    newQuery shouldBe a[ParameterQuery]
    newQuery.asInstanceOf[ParameterQuery].parameters should equal(Parameter("queryParamKey") +: parameters)
  }

  it should "succeed with a `Parameters`" in {
    val query = Query("query")
    val otherQuery = ParameterQuery(Parameter("queryParamKey", Some("queryParamValue")))
    val newQuery = query.append(otherQuery)
    newQuery shouldBe a[ParameterQuery]
    newQuery.asInstanceOf[ParameterQuery].parameters should equal(Parameter("query") +: otherQuery.parameters)
  }

  "`StringQuery.unapply`" should "succeed with `StringQuery`" in {
    val query = StringQuery("queryParamKey=queryParamValue")
    StringQuery.unapply(query).value should equal("queryParamKey=queryParamValue")
  }

  it should "succeed with `null`" in {
    StringQuery.unapply(null) should equal(None)
  }

  "`ParameterQuery.unapply`" should "succeed with `ParameterQuery`" in {
    val query = ParameterQuery(Parameter("queryParamKey", Some("queryParamValue")))
    ParameterQuery.unapply(query).value should equal(Seq(Parameter("queryParamKey", Some("queryParamValue"))))
  }

  it should "succeed with `null`" in {
    ParameterQuery.unapply(null) should equal(None)
  }

  "`Parameter.copy`" should "succeed with key and value" in {
    Parameter("key", Some("value")).copy("newKey", Option("newValue")) should equal(Parameter("newKey", "newValue"))
  }

  it should "succeed with key only" in {
    Parameter("key", Some("value")).copy("newKey") should equal(Parameter("newKey", "value"))
  }

  it should "succeed with value only" in {
    Parameter("key", Some("value")).copy(value = Option("newValue")) should equal(Parameter("key", "newValue"))
  }

  "`Parameter.apply`" should "succeed with key and value" in {
    val parameter = Parameter("key", Some("value"))
    parameter.key should equal("key")
    parameter.value.value should equal("value")
  }

  it should "succeed with an empty key and a value" in {
    val parameter = Parameter("", Some("value"))
    parameter.key should equal("")
    parameter.value.value should equal("value")
  }

  it should "succeed with key only" in {
    val parameter = Parameter("key")
    parameter.key should equal("key")
    parameter.value should equal(None)
  }

  it should "succeed with an empty key only" in {
    val parameter = Parameter("")
    parameter.key should equal("")
    parameter.value should equal(None)
  }

  it should "succeed with `null` value (String)" in {
    val parameter = Parameter("key", null: String)
    parameter.key should equal("key")
    parameter.value should equal(None)
  }

  it should "fail when passed `null` key" in {
    an [IllegalArgumentException] should be thrownBy {
      Parameter(null)
    }
  }

  it should "fail when passed `null` value (Option[String])" in {
    an [IllegalArgumentException] should be thrownBy {
      Parameter("key", null: Option[String])
    }
  }
}
