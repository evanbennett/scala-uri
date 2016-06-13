package com.netaporter.uri

import org.scalatest.{Matchers, FlatSpec}

class ApplyTests extends FlatSpec with Matchers {

  "Uri apply method" should "accept String scheme, String host and path" in {
    val uri = Uri(scheme = "http", host = "theon.github.com", pathParts = Seq(StringPathPart("blah")))
    uri.protocol should equal(Some("http"))
    uri.host should equal(Some("theon.github.com"))
    uri.pathToString should equal("/blah")
    uri.query should equal(EmptyQueryString)
  }

  "Uri apply method" should "accept String scheme, String host and QueryString" in {
    val qs = QueryString(Vector("testKey" -> Some("testVal")))
    val uri = Uri(scheme = "http", host = "theon.github.com", query = qs)
    uri.protocol should equal(Some("http"))
    uri.host should equal(Some("theon.github.com"))
    uri.query should equal(qs)
  }

  "Uri apply method" should "accept Option[String] scheme, String host and QueryString" in {
    val qs = QueryString(Vector("testKey" -> Some("testVal")))
    val uri = Uri(scheme = "http", host = "theon.github.com", query = qs)
    uri.scheme should equal(Some("http"))
    uri.host should equal(Some("theon.github.com"))
    uri.query should equal(qs)
  }

  "Uri apply method" should "accept QueryString" in {
    val qs = QueryString(Vector("testKey" -> Some("testVal")))
    val uri = Uri(query = qs)
    uri.protocol should equal(None)
    uri.host should equal(None)
    uri.query should equal(qs)
  }
}
