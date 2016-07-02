package com.netaporter.uri

import com.netaporter.uri.dsl._

class DslTests extends TestSpec { // TODO: This needs to be fully reviewed. It should purely test the DSL, not other methods as well.

  // NOTE: DSL is also tested in EncodingTests.

//  "A simple absolute URI" should "render correctly" in {
//    val uri: Uri = "http://theon.github.com/uris-in-scala.html"
//    uri.toString should equal("http://theon.github.com/uris-in-scala.html")
//  }
//
//  "A simple relative URI" should "render correctly" in {
//    val uri: Uri = "/uris-in-scala.html"
//    uri.toString should equal("/uris-in-scala.html")
//  }
//
//  "An absolute URI with query parameters" should "render correctly" in {
//    val uri = "http://theon.github.com/uris-in-scala.html" ? ("testOne" -> "1") & ("testTwo" -> "2")
//    uri.toString should equal("http://theon.github.com/uris-in-scala.html?testOne=1&testTwo=2")
//  }
//
//  "A relative URI with query parameters" should "render correctly" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1") & ("testTwo" -> "2")
//    uri.toString should equal("/uris-in-scala.html?testOne=1&testTwo=2")
//  }
//
//  "Multiple query parameters with the same key" should "render correctly" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1") & ("testOne" -> "2")
//    uri.toString should equal("/uris-in-scala.html?testOne=1&testOne=2")
//  }
//
//  "Replace param method" should "replace single parameters with a String argument" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1")
//    uri match { case uri: AbsolutePathRelativeReference =>
//      val newUri = uri.replaceParams("testOne", "2")
//      newUri.toString should equal("/uris-in-scala.html?testOne=2")
//    }
//  }
//
//  it should "replace multiple parameters with a String argument" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1") & ("testOne" -> "2")
//    uri match { case uri: AbsolutePathRelativeReference =>
//      val newUri = uri.replaceParams("testOne", "2")
//      newUri.toString should equal("/uris-in-scala.html?testOne=2")
//    }
//  }
//
//  it should "replace parameters with a Some argument" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1")
//    uri match { case uri: AbsolutePathRelativeReference =>
//      val newUri = uri.replaceParams("testOne", Some("2"))
//      newUri.toString should equal("/uris-in-scala.html?testOne=2")
//    }
//  }
//
//  it should "not affect other parameters" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1") & ("testTwo" -> "2")
//    uri match { case uri: AbsolutePathRelativeReference =>
//      val newUri = uri.replaceParams("testOne", "3")
//      newUri.toString should equal("/uris-in-scala.html?testTwo=2&testOne=3")
//    }
//  }
//
//  it should "remove multiple parameters" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1") & ("testOne" -> "2")
//    uri match { case uri: AbsolutePathRelativeReference =>
//      val newUri = uri.removeParams("testOne")
//      newUri.toString should equal("/uris-in-scala.html")
//    }
//  }
//
//  it should "remove single parameters" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1")
//    uri match { case uri: AbsolutePathRelativeReference =>
//      val newUri = uri.removeParams("testOne")
//      newUri.toString should equal("/uris-in-scala.html")
//    }
//  }
//
//  it should "not remove other parameters" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1") & ("testTwo" -> "2")
//    uri match { case uri: AbsolutePathRelativeReference =>
//      val newUri = uri.removeParams("testOne")
//      newUri.toString should equal("/uris-in-scala.html?testTwo=2")
//    }
//  }
//
//  it should "remove parameters contained in SeqLike" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1") & ("testTwo" -> "2")
//    uri match { case uri: AbsolutePathRelativeReference =>
//      val newUri = uri.removeParams(List("testOne", "testTwo"))
//      newUri.toString should equal("/uris-in-scala.html")
//    }
//  }
//
//  it should "not remove parameters uncontained in List" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1") & ("testTwo" -> "2")
//    uri match { case uri: AbsolutePathRelativeReference =>
//      val newUri = uri.removeParams(List("testThree", "testFour"))
//      newUri.toString should equal("/uris-in-scala.html?testOne=1&testTwo=2")
//    }
//  }
//
//  it should "remove parameters contained in List and not remove parameters uncontained in List" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1") & ("testTwo" -> "2")
//    uri match { case uri: AbsolutePathRelativeReference =>
//      val newUri = uri.removeParams(List("testOne", "testThree"))
//      newUri.toString should equal("/uris-in-scala.html?testTwo=2")
//    }
//  }
//
//  "Replace all params method" should "replace all query params" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1") & ("testTwo" -> "2")
//    uri match { case uri: AbsolutePathRelativeReference =>
//      val newUri = uri.replaceAllParams(("testThree", Some("3")), ("testFour", Some("4")))
//      newUri.toString should equal("/uris-in-scala.html?testThree=3&testFour=4")
//    }
//  }
//
//  "Remove all params method" should "remove all query params" in {
//    val uri = "/uris-in-scala.html" ? ("testOne" -> "1") & ("testTwo" -> "2")
//    uri match { case uri: AbsolutePathRelativeReference =>
//      val newUri = uri.removeAllParams
//      newUri.toString should equal("/uris-in-scala.html")
//    }
//  }
//
//  "Scheme setter method" should "copy the URI with the new scheme" in {
//    val uri = "http://coldplay.com/chris-martin.html" ? ("testOne" -> "1")
//    val newUri = uri.withScheme("https")
//    newUri.toString should equal("https://coldplay.com/chris-martin.html?testOne=1")
//  }
//
//  "Host setter method" should "copy the URI with the new host" in {
//    val uri = "http://coldplay.com/chris-martin.html" ? ("testOne" -> "1")
//    val newUri = uri.withHost("jethrotull.com")
//    newUri.toString should equal("http://jethrotull.com/chris-martin.html?testOne=1")
//  }
//
//  "Port setter method" should "copy the URI with the new port" in {
//    val uri = "http://coldplay.com/chris-martin.html" ? ("testOne" -> "1")
//    val newUri = uri.withPort(8080)
//    newUri.toString should equal("http://coldplay.com:8080/chris-martin.html?testOne=1")
//  }
//
//  "Path with fragment" should "render correctly" in {
//    val uri = "http://google.com/test" `#` "fragment"
//    uri.toString should equal("http://google.com/test#fragment")
//  }
//
//  "Path with query and fragment" should "render correctly" in {
//    val uri = "http://google.com/test" ? ("q" -> "scala-uri") `#` "fragment"
//    uri.toString should equal("http://google.com/test?q=scala-uri#fragment")
//  }
//
//  "hostParts" should "return the dot separated host" in {
//    val uri = "http://theon.github.com/test" ? ("q" -> "scala-uri")
//    uri.hostParts should equal(Vector("theon", "github", "com"))
//  }
//
//  "subdomain" should "return the first dot separated part of the host" in {
//    val uri = "http://theon.github.com/test" ? ("q" -> "scala-uri")
//    uri.subdomain should equal(Some("theon"))
//  }
//
//  "Uri with user info" should "render correctly" in {
//    val uri = "http://user:password@moonpig.com/" `#` "hi"
//    uri.toString should equal("http://user:password@moonpig.com/#hi")
//  }
//
//  "Uri with a changed user" should "render correctly" in {
//    val uri = "http://user:password@moonpig.com/" `#` "hi"
//    uri.withUser("ian").toString should equal("http://ian:password@moonpig.com/#hi")
//  }
//
//  "Uri with a changed password" should "render correctly" in {
//    val uri = "http://user:password@moonpig.com/" `#` "hi"
//    uri.withPassword("not-so-secret").toString should equal("http://user:not-so-secret@moonpig.com/#hi")
//  }
//
//  "Matrix params" should "be added mid path" in {
//    val uri = "http://stackoverflow.com/pathOne/pathTwo"
//    val uriTwo = uri.addMatrixParam("pathOne", "key", "val")
//
//    uriTwo.pathSegment("pathOne").params should equal(Vector(Parameter("key", Some("val"))))
//    uriTwo.toString should equal("http://stackoverflow.com/pathOne;key=val/pathTwo")
//  }
//
//  it should "be added to the end of the path" in {
//    val uri = "http://stackoverflow.com/pathOne/pathTwo"
//    val uriTwo = uri.addMatrixParam("key", "val")
//
//    uriTwo.matrixParams should equal(Vector(Parameter("key", Some("val"))))
//    uriTwo.pathSegment("pathTwo").params should equal(Vector(Parameter("key", Some("val"))))
//    uriTwo.toString should equal("http://stackoverflow.com/pathOne/pathTwo;key=val")
//  }
//
//  "A list of query params" should "get added successsfully" in {
//    val p = ("key", true) :: ("key2", false) :: Nil
//    val uri = "http://example.com".addParams(p)
//    uri.queryParams("key") should equal(Some("true") :: Nil)
//    uri.queryParams("key2") should equal(Some("false") :: Nil)
//    uri.toString should equal("http://example.com?key=true&key2=false")
//  }
//
//  it should "get added to a URL already with query params successsfully" in {
//    val p = ("key", true) :: ("key2", false) :: Nil
//    val uri = ("http://example.com" ? ("key" -> Some("param1"))).addQueryParams(p)
//    uri.queryParams("key") should equal(Vector(Some("param1"), Some("true")))
//    uri.queryParams("key2") should equal(Some("false") :: Nil)
//  }
//
//  "Path and query DSL" should "be possible to use together" in {
//    val uri = "http://host" / "path" / "to" / "resource" ? ("a" -> "1") & ("b" -> "2")
//    uri.toString should equal("http://host/path/to/resource?a=1&b=2")
//  }
//
//  "Path and fragment DSL" should "be possible to use together" in {
//    val uri = "http://host" / "path" / "to" / "resource" `#` "hellyeah"
//    uri.toString should equal("http://host/path/to/resource#hellyeah")
//  }
//
//  "Path and query and fragment DSL" should "be possible to use together" in {
//    val uri = "http://host" / "path" / "to" / "resource" ? ("a" -> "1") & ("b" -> "2") `#` "wow"
//    uri.toString should equal("http://host/path/to/resource?a=1&b=2#wow")
//  }
//
//  "Latter fragments DSLs" should "overwrite earlier fragments" in {
//    val uri = "http://host" / "path" / "to" `#` "weird" / "resource" ? ("a" -> "1" ) & ("b" -> "2") `#` "wow"
//    uri.toString should equal("http://host/path/to/resource?a=1&b=2#wow")
//  }
//
//  "/? operator" should "add a slash to the path and a query param" in {
//    val uri = "http://host" /? ("a" -> "1" )
//    uri.toString should equal("http://host/?a=1")
//  }
//
//  it should "work alongside the / operator" in {
//    val uri = "http://host" / "path" /? ("a" -> "1" )
//    uri.toString should equal("http://host/path/?a=1")
//  }
//
//  it should "work alongside the & operator" in {
//    val uri = "http://host" /? ("a" -> "1" ) & ("b" -> "2" )
//    uri.toString should equal("http://host/?a=1&b=2")
//  }
//
//  it should "work alongside the / and & operators together" in {
//    val uri = "http://host" / "path" /? ("a" -> "1" ) & ("b" -> "2" )
//    uri.toString should equal("http://host/path/?a=1&b=2")
//  }
}
