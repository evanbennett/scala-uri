package com.netaporter.uri

import com.netaporter.uri.config.UriConfig

class ParsingTests extends TestSpec {

  "Parsing an `AbsoluteUri` (must have scheme and host) with `default` UriConfig" should "successfully parse without user, password, port, path, query and fragment" in {
    val uriString = "http://test.com"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsoluteUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfo should equal(None)
    uri.host.value should equal("test.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, port, path, query and fragment" in {
    val uriString = "http://evan:password@test.com:8080/path?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsoluteUri]
    uri.scheme.value.scheme should equal("http")
    uri.user.value should equal("evan")
    uri.password.value should equal("password")
    uri.host.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with port, path, query and fragment, and without user and password" in {
    val uriString = "http://test.com:8080/path?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsoluteUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfo should equal(None)
    uri.host.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, port, path, query and fragment, and without password" in {
    val uriString = "http://evan@test.com:8080/path?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsoluteUri]
    uri.scheme.value.scheme should equal("http")
    uri.user.value should equal("evan")
    uri.password should equal(None)
    uri.host.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, path, query and fragment, and without port" in {
    val uriString = "http://evan:password@test.com/path?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsoluteUri]
    uri.scheme.value.scheme should equal("http")
    uri.user.value should equal("evan")
    uri.password.value should equal("password")
    uri.host.value should equal("test.com")
    uri.port should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, port, query and fragment, and without path" in {
    val uriString = "http://evan:password@test.com:8080?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsoluteUri]
    uri.scheme.value.scheme should equal("http")
    uri.user.value should equal("evan")
    uri.password.value should equal("password")
    uri.host.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, port, path and fragment, and without query" in {
    val uriString = "http://evan:password@test.com:8080/path#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsoluteUri]
    uri.scheme.value.scheme should equal("http")
    uri.user.value should equal("evan")
    uri.password.value should equal("password")
    uri.host.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, port, path and query, and without fragment" in {
    val uriString = "http://evan:password@test.com:8080/path?queryKey=queryValue"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsoluteUri]
    uri.scheme.value.scheme should equal("http")
    uri.user.value should equal("evan")
    uri.password.value should equal("password")
    uri.host.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path, and without user, password, port, query and fragment" in {
    val uriString = "http://theon.github.com/uris-in-scala.html"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsoluteUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfo should equal(None)
    uri.host.value should equal("theon.github.com")
    uri.port should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("uris-in-scala.html")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user and empty password only" in {
    val uriString = "ftp://theon:@github.com"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsoluteUri]
    uri.scheme.value.scheme should equal("ftp")
    uri.user.value should equal("theon")
    uri.password.value should equal("")
    uri.host.value should equal("github.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with query containing '@' " in {
    val uriString = "http://www.mywebsite.com?a=b@"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsoluteUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfo should equal(None)
    uri.host.value should equal("www.mywebsite.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq(Parameter("a", Some("b@"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  "Parsing an `AuthorityReferenceUri` (must start with an authority and have host) with `default` UriConfig" should "successfully parse without user, password, port, path, query and fragment" in {
    val uriString = "//test.com"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AuthorityReferenceUri]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.host.value should equal("test.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, port, path, query and fragment" in {
    val uriString = "//evan:password@test.com:8080/path?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AuthorityReferenceUri]
    uri.scheme should equal(None)
    uri.user.value should equal("evan")
    uri.password.value should equal("password")
    uri.host.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with port, path, query and fragment, and without user and password" in {
    val uriString = "//test.com:8080/path?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AuthorityReferenceUri]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.host.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, port, path, query and fragment, and without password" in {
    val uriString = "//evan@test.com:8080/path?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AuthorityReferenceUri]
    uri.scheme should equal(None)
    uri.user.value should equal("evan")
    uri.password should equal(None)
    uri.host.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, path, query and fragment, and without port" in {
    val uriString = "//evan:password@test.com/path?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AuthorityReferenceUri]
    uri.scheme should equal(None)
    uri.user.value should equal("evan")
    uri.password.value should equal("password")
    uri.host.value should equal("test.com")
    uri.port should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, port, query and fragment, and without path" in {
    val uriString = "//evan:password@test.com:8080?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AuthorityReferenceUri]
    uri.scheme should equal(None)
    uri.user.value should equal("evan")
    uri.password.value should equal("password")
    uri.host.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, port, path and fragment, and without query" in {
    val uriString = "//evan:password@test.com:8080/path#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AuthorityReferenceUri]
    uri.scheme should equal(None)
    uri.user.value should equal("evan")
    uri.password.value should equal("password")
    uri.host.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, port, path and query, and without fragment" in {
    val uriString = "//evan:password@test.com:8080/path?queryKey=queryValue"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AuthorityReferenceUri]
    uri.scheme should equal(None)
    uri.user.value should equal("evan")
    uri.password.value should equal("password")
    uri.host.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path, and without user, password, port, query and fragment" in {
    val uriString = "//theon.github.com/uris-in-scala.html"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AuthorityReferenceUri]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.host.value should equal("theon.github.com")
    uri.port should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("uris-in-scala.html")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  "Parsing an `AbsolutePathReferenceUri` (must start with an absolute path) with `default` UriConfig" should "successfully parse without query and fragment" in {
    val uriString = "/path"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsolutePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with query and fragment" in {
    val uriString = "/path?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsolutePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with fragment, and without query" in {
    val uriString = "/path#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsolutePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with query, and without fragment" in {
    val uriString = "/path?queryKey=queryValue"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsolutePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty path, and without query and fragment" in {
    val uriString = "/"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsolutePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty path, query and fragment" in {
    val uriString = "/?#"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsolutePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value should equal(EmptyFragment)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty path and a fragment, and without query" in {
    val uriString = "/#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsolutePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty path and a query, and without fragment" in {
    val uriString = "/?queryKey=queryValue"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsolutePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing empty segments, and without query and fragment" in {
    val uriString = "/path1//path3/"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsolutePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path1"), EmptySegment, StringSegment("path3"), EmptySegment))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing matrix parameters, and without query and fragment" in {
    val uriString = "/path1;matrixKeyOne=matrixValueOne;matrixKeyTwo=matrixValueTwo/path2;matrixKey=matrixValue"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsolutePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path1;matrixKeyOne=matrixValueOne;matrixKeyTwo=matrixValueTwo"), StringSegment("path2;matrixKey=matrixValue")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  "Parsing an absolute path reference URI (must start with an absolute path) with `UriConfig(matrixParams = true)`" should "successfully parse with path containing matrix parameters, and without query and fragment" in {
    implicit val c = UriConfig(matrixParams = true)
    val uriString = "/path1;matrixKeyOne=matrixValueOne;matrixKeyTwo=matrixValueTwo/path2;matrixKey=matrixValue"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsolutePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path1", Seq(Parameter("matrixKeyOne", Some("matrixValueOne")), Parameter("matrixKeyTwo", Some("matrixValueTwo")))), MatrixParametersSegment("path2", Seq(Parameter("matrixKey", Some("matrixValue"))))))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing matrix parameters (with no value, empty value and a value), and without query and fragment" in {
    implicit val c = UriConfig(matrixParams = true)
    val uriString = "/path;matrixKeyOne;matrixKeyTwo=;matrixKeyThree=matrixValueThree"
    val uri = Uri.parse(uriString)
    uri shouldBe an[AbsolutePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path", Seq(Parameter("matrixKeyOne", None), Parameter("matrixKeyTwo", Some("")), Parameter("matrixKeyThree", Some("matrixValueThree"))))))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing some empty matrix parameters, and without query and fragment" in {
    implicit val c = UriConfig(matrixParams = true)
    val uri = Uri.parse("/path;;matrixKeyTwo=matrixValueTwo;;matrixKeyFour=matrixValueFour;")
    uri shouldBe an[AbsolutePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path", Seq(Parameter("matrixKeyTwo", Some("matrixValueTwo")), Parameter("matrixKeyFour", Some("matrixValueFour"))))))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal("/path;matrixKeyTwo=matrixValueTwo;matrixKeyFour=matrixValueFour")
  }

  "Parsing a `RelativePathReferenceUri` (must start with a relative path) with `default` UriConfig" should "successfully parse without query and fragment" in {
    val uriString = "path"
    val uri = Uri.parse(uriString)
    uri shouldBe a[RelativePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with query and fragment" in {
    val uriString = "path?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe a[RelativePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with fragment, and without query" in {
    val uriString = "path#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe a[RelativePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with query, and without fragment" in {
    val uriString = "path?queryKey=queryValue"
    val uri = Uri.parse(uriString)
    uri shouldBe a[RelativePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing empty segments, and without query and fragment" in {
    val uriString = "path1//path3/"
    val uri = Uri.parse(uriString)
    uri shouldBe a[RelativePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path1"), EmptySegment, StringSegment("path3"), EmptySegment))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing matrix parameters, and without query and fragment" in {
    val uriString = "path1;matrixKeyOne=matrixValueOne;matrixKeyTwo=matrixValueTwo/path2;matrixKey=matrixValue"
    val uri = Uri.parse(uriString)
    uri shouldBe a[RelativePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path1;matrixKeyOne=matrixValueOne;matrixKeyTwo=matrixValueTwo"), StringSegment("path2;matrixKey=matrixValue")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  "Parsing a relative path reference URI (must start with a relative path) with `UriConfig(matrixParams = true)`" should "successfully parse with path containing matrix parameters, and without query and fragment" in {
    implicit val c = UriConfig(matrixParams = true)
    val uriString = "path1;matrixKeyOne=matrixValueOne;matrixKeyTwo=matrixValueTwo/path2;matrixKey=matrixValue"
    val uri = Uri.parse(uriString)
    uri shouldBe a[RelativePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[RootlessPath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path1", Seq(Parameter("matrixKeyOne", Some("matrixValueOne")), Parameter("matrixKeyTwo", Some("matrixValueTwo")))), MatrixParametersSegment("path2", Seq(Parameter("matrixKey", Some("matrixValue"))))))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing matrix parameters (with no value, empty value and a value), and without query and fragment" in {
    implicit val c = UriConfig(matrixParams = true)
    val uriString = "path;matrixKeyOne;matrixKeyTwo=;matrixKeyThree=matrixValueThree"
    val uri = Uri.parse(uriString)
    uri shouldBe a[RelativePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[RootlessPath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path", Seq(Parameter("matrixKeyOne", None), Parameter("matrixKeyTwo", Some("")), Parameter("matrixKeyThree", Some("matrixValueThree"))))))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing some empty matrix parameters, and without query and fragment" in {
    implicit val c = UriConfig(matrixParams = true)
    val uri = Uri.parse("path;;matrixKeyTwo=matrixValueTwo;;matrixKeyFour=matrixValueFour;")
    uri shouldBe a[RelativePathReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[RootlessPath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path", Seq(Parameter("matrixKeyTwo", Some("matrixValueTwo")), Parameter("matrixKeyFour", Some("matrixValueFour"))))))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal("path;matrixKeyTwo=matrixValueTwo;matrixKeyFour=matrixValueFour")
  }

  "Parsing a `QueryReferenceUri` (must start with a query) with `default` UriConfig" should "successfully parse without fragment" in {
    val uriString = "?queryKey=queryValue"
    val uri = Uri.parse(uriString)
    uri shouldBe a[QueryReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with fragment" in {
    val uriString = "?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe a[QueryReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with empty query parameter values, and without fragment" in {
    val uriString = "?queryKeyOne=&queryKeyTwo=&queryKeyThree="
    val uri = Uri.parse(uriString)
    uri shouldBe a[QueryReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq(Parameter("queryKeyOne", Some("")), Parameter("queryKeyTwo", Some("")), Parameter("queryKeyThree", Some(""))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty query parameter value and a fragment" in {
    val uriString = "?queryKey=#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe a[QueryReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some(""))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with no query parameter values, and without fragment" in {
    val uriString = "?queryKeyOne&queryKeyTwo&queryKeyThree"
    val uri = Uri.parse(uriString)
    uri shouldBe a[QueryReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq(Parameter("queryKeyOne", None), Parameter("queryKeyTwo", None), Parameter("queryKeyThree", None)))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with a no query parameter value and a fragment" in {
    val uriString = "?queryKey#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe a[QueryReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq(Parameter("queryKey", None)))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty query, and without fragment" in {
    val uriString = "?"
    val uri = Uri.parse(uriString)
    uri shouldBe a[QueryReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty query and fragment" in {
    val uriString = "?#"
    val uri = Uri.parse(uriString)
    uri shouldBe a[QueryReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value should equal(EmptyFragment)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty query and a fragment" in {
    val uriString = "?#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe a[QueryReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an improperly encoded query parameter value, and without fragment" in {
    val uri = Uri.parse("?query_param_one=hello=world&query_param_two=false")
    uri shouldBe a[QueryReferenceUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq(Parameter("query_param_one", Some("hello=world")), Parameter("query_param_two", Some("false"))))
    uri.fragment should equal(None)
    uri.toString should equal("?query_param_one=hello%3Dworld&query_param_two=false")
  }

  "Parsing a `SameDocumentUri` (must be a fragment) with `default` UriConfig" should "successfully parse" in {
    val uriString = "#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SameDocumentUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty fragment" in {
    val uriString = "#"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SameDocumentUri]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value should equal(EmptyFragment)
    uri.toString should equal(uriString)
  }

  "Parsing an `EmptyUri` with `default` UriConfig" should "successfully parse" in {
    Uri.parse("") should equal(EmptyUri)
    EmptyUri.toString should equal("")
  }

  "Parsing a `SchemeWithAbsolutePathUri` (must have scheme and absolute path; must not have authority) with `default` UriConfig" should "successfully parse without query and fragment" in {
    val uriString = "test:/path"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with query and fragment" in {
    val uriString = "test:/path?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with fragment, and without query" in {
    val uriString = "test:/path#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with query, and without fragment" in {
    val uriString = "test:/path?queryKey=queryValue"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  "Parsing a `SchemeWithRootlessPathUri` (must have scheme and rootless path; must not have authority) with `default` UriConfig" should "successfully parse without query and fragment" in {
    val uriString = "test:path"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with query and fragment" in {
    val uriString = "test:path?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with fragment, and without query" in {
    val uriString = "test:path#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with query, and without fragment" in {
    val uriString = "test:path?queryKey=queryValue"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse a simple mailto URI" in {
    val uriString = "mailto:evan@test.com"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("mailto")
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("evan@test.com")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse a more complete mailto URI" in {
    val uriString = "mailto:evan@test.com?subject=Subject&body=Message"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("mailto")
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("evan@test.com")))
    uri.queryParameters should equal(Seq(Parameter("subject", Some("Subject")), Parameter("body", Some("Message"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse a ISBN URI" in {
    val uriString = "urn:isbn:1-84356-028-3"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("urn")
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("isbn:1-84356-028-3")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse a UUID URI" in {
    val uriString = "urn:uuid:123e4567-e89b-12d3-a456-426655440000"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("urn")
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("uuid:123e4567-e89b-12d3-a456-426655440000")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  "Parsing a `SchemeWithQueryUri` (must have scheme and query; must not have authority and path) with `default` UriConfig" should "successfully parse without fragment" in {
    val uriString = "test:?queryKey=queryValue"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithQueryUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with fragment" in {
    val uriString = "test:?queryKey=queryValue#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithQueryUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  "Parsing a `SchemeWithFragmentUri` (must have scheme and fragment; must not have authority path and query) with `default` UriConfig" should "successfully parse" in {
    val uriString = "test:#fragment"
    val uri = Uri.parse(uriString)
    uri shouldBe a[SchemeWithFragmentUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  "Parsing an invalid URI"  should "fail with a invalid scheme" in {
    intercept[java.net.URISyntaxException] {
      Uri.parse("htt[://test.com/path")
    }
  }
}
