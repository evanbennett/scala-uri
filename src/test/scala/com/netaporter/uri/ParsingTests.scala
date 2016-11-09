package com.netaporter.uri

import com.netaporter.uri.parsing.ParseUri

class ParsingTests extends TestSpec {

  "Parsing an `SchemeWithAuthorityUri` (must have scheme and authority) with the default `UriConfig`" should "successfully parse with host, and without user, password, port, path, query and fragment" in {
    val uriString = "http://test.com"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfo should equal(None)
    uri.hostString.value should equal("test.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, host, port, path, query and fragment" in {
    val uriString = "http://evan:password@test.com:8080/path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfoString.value should equal("evan:password")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host, port, path, query and fragment, and without user and password" in {
    val uriString = "http://test.com:8080/path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfo should equal(None)
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, host, port, path, query and fragment, and without password" in {
    val uriString = "http://evan@test.com:8080/path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfoString.value should equal("evan")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host, port, path, query and fragment, and with empty user" in {
    val uriString = "http://:password@test.com:8080/path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfoString.value should equal(":password")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, host, port, path, query and fragment, and with empty password" in {
    val uriString = "http://evan:@test.com:8080/path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfoString.value should equal("evan:")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host, port, path, query and fragment, and with empty user and password" in {
    val uriString = "http://:@test.com:8080/path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfoString.value should equal(":")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host, port, path, query and fragment, and with empty user info" in {
    val uriString = "http://@test.com:8080/path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfo.value should equal(EmptyUserInfo)
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal("http://test.com:8080/path?queryKey=queryValue#fragment")
    uri.toString(UriConfig(emptyComponentNormalization = false)) should equal(uriString)
  }

  it should "successfully parse with empty authority (without user, password, host and port), path, query and fragment" in {
    val uriString = "http:///path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.authority.value should equal(EmptyAuthority)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, host, path, query and fragment, and without port" in {
    val uriString = "http://evan:password@test.com/path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfoString.value should equal("evan:password")
    uri.hostString.value should equal("test.com")
    uri.port should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, host, path, query and fragment, and with empty port" in {
    val uriString = "http://evan:password@test.com:/path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfoString.value should equal("evan:password")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(-1)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal("http://evan:password@test.com/path?queryKey=queryValue#fragment")
    uri.toString(UriConfig(emptyComponentNormalization = false)) should equal(uriString)
  }

  it should "successfully parse with user, password, host, port, query and fragment, and without path" in {
    val uriString = "http://evan:password@test.com:8080?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfoString.value should equal("evan:password")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, host, port, path and fragment, and without query" in {
    val uriString = "http://evan:password@test.com:8080/path#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfoString.value should equal("evan:password")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, host, port, path and query, and without fragment" in {
    val uriString = "http://evan:password@test.com:8080/path?queryKey=queryValue"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfoString.value should equal("evan:password")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host and empty absolute path, and without user, password, port, query and fragment" in {
    val uriString = "http://test.com/"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfo should equal(None)
    uri.hostString.value should equal("test.com")
    uri.port should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host and path, and without user, password, port, query and fragment" in {
    val uriString = "http://theon.github.com/uris-in-scala.html"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfo should equal(None)
    uri.hostString.value should equal("theon.github.com")
    uri.port should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("uris-in-scala.html")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with query containing '@' " in {
    val uriString = "http://www.mywebsite.com?a=b@"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfo should equal(None)
    uri.hostString.value should equal("www.mywebsite.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("a", Some("b@"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  {
    implicit val config = UriConfig(userPasswordParsing = true)

    "Parsing an `SchemeWithAuthorityUri` (must have scheme and authority) with the default `UriConfig(userPasswordParsing = true)`" should "successfully parse with user, host, port, path, query and fragment, and with empty password" in {
      val uriString = "http://evan:@test.com:8080/path?queryKey=queryValue#fragment"
      val uri = ParseUri(uriString)
      uri shouldBe a[SchemeWithAuthorityUri]
      uri.scheme.value.scheme should equal("http")
      uri.user.value should equal("evan")
      uri.password.value should equal("")
      uri.hostString.value should equal("test.com")
      uri.port.value should equal(8080)
      uri.path.value shouldBe an[AbsolutePath]
      uri.pathSegments should equal(Seq(StringSegment("path")))
      uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
      uri.fragment.value.fragment should equal("fragment")
      uri.toString should equal("http://evan@test.com:8080/path?queryKey=queryValue#fragment")
      uri.toString(UriConfig(emptyComponentNormalization = false)) should equal(uriString)
    }

    it should "successfully parse with host, port, path, query and fragment, and with empty user and password" in {
      val uriString = "http://:@test.com:8080/path?queryKey=queryValue#fragment"
      val uri = ParseUri(uriString)
      uri shouldBe a[SchemeWithAuthorityUri]
      uri.scheme.value.scheme should equal("http")
      uri.user.value should equal("")
      uri.password.value should equal("")
      uri.hostString.value should equal("test.com")
      uri.port.value should equal(8080)
      uri.path.value shouldBe an[AbsolutePath]
      uri.pathSegments should equal(Seq(StringSegment("path")))
      uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
      uri.fragment.value.fragment should equal("fragment")
      uri.toString should equal("http://test.com:8080/path?queryKey=queryValue#fragment")
      uri.toString(UriConfig(emptyComponentNormalization = false)) should equal(uriString)
    }
  }

  "Parsing a `SchemeWithAbsolutePathUri` (must have scheme and absolute path, and must not have authority) with the default `UriConfig`" should "successfully parse without query and fragment" in {
    val uriString = "test:/path"
    val uri = ParseUri(uriString)
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
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with fragment, and without query" in {
    val uriString = "test:/path#fragment"
    val uri = ParseUri(uriString)
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
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  "Parsing a `SchemeWithRootlessPathUri` (must have scheme and rootless path, and must not have authority) with the default `UriConfig`" should "successfully parse without query and fragment" in {
    val uriString = "test:path"
    val uri = ParseUri(uriString)
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
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with fragment, and without query" in {
    val uriString = "test:path#fragment"
    val uri = ParseUri(uriString)
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
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse a simple mailto URI" in {
    val uriString = "mailto:evan@test.com"
    val uri = ParseUri(uriString)
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
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("mailto")
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("evan@test.com")))
    uri.queryParameters.value should equal(Seq(Parameter("subject", Some("Subject")), Parameter("body", Some("Message"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse a ISBN URI" in {
    val uriString = "urn:isbn:1-84356-028-3"
    val uri = ParseUri(uriString)
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
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("urn")
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("uuid:123e4567-e89b-12d3-a456-426655440000")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  "Parsing a `SchemeWithQueryUri` (must have scheme and query, and must not have authority and path) with the default `UriConfig`" should "successfully parse without fragment" in {
    val uriString = "test:?queryKey=queryValue"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithQueryUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with fragment" in {
    val uriString = "test:?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithQueryUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  "Parsing a `SchemeWithFragmentUri` (must have scheme and fragment, and must not have authority, path and query) with the default `UriConfig`" should "successfully parse" in {
    val uriString = "test:#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeWithFragmentUri]
    uri.scheme.value.scheme should equal("test")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  "Parsing a `SchemeUri` (must have scheme, and must not have authority, path, query and fragment) with the default `UriConfig`" should "successfully parse" in {
    val uriString = "dav:"
    val uri = ParseUri(uriString)
    uri shouldBe a[SchemeUri]
    uri.scheme.value.scheme should equal("dav")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  "Parsing an `NetworkPathReference` (must start with an authority) with the default `UriConfig`" should "successfully parse with host (registered name), and without user, password, port, path, query and fragment" in {
    val uriString = "//test.com"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.hostString.value should equal("test.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, host, port, path, query and fragment" in {
    val uriString = "//evan:password@test.com:8080/path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfoString.value should equal("evan:password")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host, port, path, query and fragment, and without user and password" in {
    val uriString = "//test.com:8080/path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, host, port, path, query and fragment, and without password" in {
    val uriString = "//evan@test.com:8080/path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfoString.value should equal("evan")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with empty authority (without user, password, host and port), path, query and fragment" in {
    val uriString = "///path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.authority.value should equal(EmptyAuthority)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, host, path, query and fragment, and without port" in {
    val uriString = "//evan:password@test.com/path?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfoString.value should equal("evan:password")
    uri.hostString.value should equal("test.com")
    uri.port should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, host, port, query and fragment, and without path" in {
    val uriString = "//evan:password@test.com:8080?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfoString.value should equal("evan:password")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, host, port, path and fragment, and without query" in {
    val uriString = "//evan:password@test.com:8080/path#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfoString.value should equal("evan:password")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with user, password, host, port, path and query, and without fragment" in {
    val uriString = "//evan:password@test.com:8080/path?queryKey=queryValue"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfoString.value should equal("evan:password")
    uri.hostString.value should equal("test.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host and path, and without user, password, port, query and fragment" in {
    val uriString = "//theon.github.com/uris-in-scala.html"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.hostString.value should equal("theon.github.com")
    uri.port should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("uris-in-scala.html")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv4 address) only" in {
    val uriString = "//192.168.205.1"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipv4Address.value should equal("192.168.205.1")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 1 hextet at start) only" in {
    val uriString = "//[39c2::]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[39c2::]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 1 hextet at end) only" in {
    val uriString = "//[::abe8]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[::abe8]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 2 hextets at start) only" in {
    val uriString = "//[4af2:36a2::]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[4af2:36a2::]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 2 hextets at end) only" in {
    val uriString = "//[::28d4:fffe]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[::28d4:fffe]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 2 hextets) only" in {
    val uriString = "//[ac3e::27f9]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[ac3e::27f9]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 3 hextets) only" in {
    val uriString = "//[6248::5e2a:3]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[6248::5e2a:3]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 4 hextets) only" in {
    val uriString = "//[::3a9c:9a2c:a:0020]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[::3a9c:9a2c:a:0020]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 5 hextets) only" in {
    val uriString = "//[284:7:3a23:5000::36]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[284:7:3a23:5000::36]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 6 hextets) only" in {
    val uriString = "//[02:586:39ce:aaa:29f3:d284::]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[02:586:39ce:aaa:29f3:d284::]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 7 hextets) only" in {
    val uriString = "//[10:cc0c::16d4:0fff:3957:234:12]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[10:cc0c::16d4:0fff:3957:234:12]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 8 hextets) only" in {
    val uriString = "//[0000:0000:2956:ffff:0000:0000:0000:1234]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[0000:0000:2956:ffff:0000:0000:0000:1234]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - just an IPv4 address) only" in {
    val uriString = "//[::192.168.13.8]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[::192.168.13.8]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 2 hextets at start with IPv4 address) only" in {
    val uriString = "//[28f:36a2::10.8.14.57]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[28f:36a2::10.8.14.57]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 2 hextets at end with IPv4 address) only" in {
    val uriString = "//[::e2:8349:3.248.156.24]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[::e2:8349:3.248.156.24]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPv6 address - 2 hextets with IPv4 address) only" in {
    val uriString = "//[c::a3b2:84.16.3.74]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[c::a3b2:84.16.3.74]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with host (IPvFuture) only" in {
    val uriString = "//[v9.somethingAsYetUndefined]"
    val uri = ParseUri(uriString)
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[v9.somethingAsYetUndefined]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal("//[v9.somethingasyetundefined]")
  }

  "Parsing an `AbsolutePathReference` (must start with an absolute path) with the default `UriConfig`" should "successfully parse without query and fragment" in {
    val uriString = "/path"
    val uri = ParseUri(uriString)
    uri shouldBe an[AbsolutePathReference]
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
    val uri = ParseUri(uriString)
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with fragment, and without query" in {
    val uriString = "/path#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe an[AbsolutePathReference]
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
    val uri = ParseUri(uriString)
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty path, and without query and fragment" in {
    val uriString = "/"
    val uri = ParseUri(uriString)
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty path, query and fragment" in {
    val uriString = "/?#"
    val uri = ParseUri(uriString)
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value should equal(EmptyFragment)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty path and a fragment, and without query" in {
    val uriString = "/#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty path and a query, and without fragment" in {
    val uriString = "/?queryKey=queryValue"
    val uri = ParseUri(uriString)
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing empty segments, and without query and fragment" in {
    val uriString = "/path1//path3/"
    val uri = ParseUri(uriString)
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path1"), EmptySegment, StringSegment("path3"), EmptySegment))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing matrix parameters, and without query and fragment" in {
    val uriString = "/path1;matrixKey1=matrixValue1;matrixKey2=matrixValue2/path2;matrixKey=matrixValue"
    val uri = ParseUri(uriString)
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path1;matrixKey1=matrixValue1;matrixKey2=matrixValue2"), StringSegment("path2;matrixKey=matrixValue")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  "Parsing an absolute path reference URI (must start with an absolute path) with `UriConfig(matrixParameterParsing = true)`" should "successfully parse with path containing matrix parameters, and without query and fragment" in {
    implicit val c = UriConfig(matrixParameterParsing = true)
    val uriString = "/path1;matrixKey1=matrixValue1;matrixKey2=matrixValue2/path2;matrixKey=matrixValue"
    val uri = ParseUri(uriString)
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path1", Seq(Parameter("matrixKey1", Some("matrixValue1")), Parameter("matrixKey2", Some("matrixValue2")))), MatrixParametersSegment("path2", Seq(Parameter("matrixKey", Some("matrixValue"))))))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing matrix parameters (with no value, empty value and a value), and without query and fragment" in {
    implicit val c = UriConfig(matrixParameterParsing = true)
    val uriString = "/path;matrixKey1;matrixKey2=;matrixKey3=matrixValue3"
    val uri = ParseUri(uriString)
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path", Seq(Parameter("matrixKey1", None), Parameter("matrixKey2", Some("")), Parameter("matrixKey3", Some("matrixValue3"))))))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing some empty matrix parameters, and without query and fragment" in {
    implicit val c = UriConfig(matrixParameterParsing = true)
    val uri = ParseUri("/path;;matrixKey2=matrixValue2;;matrixKey4=matrixValue4;")
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path", Seq(Parameter("matrixKey2", Some("matrixValue2")), Parameter("matrixKey4", Some("matrixValue4"))))))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal("/path;matrixKey2=matrixValue2;matrixKey4=matrixValue4")
  }

  "Parsing an absolute path reference URI (must start with an absolute path) with `UriConfig(delimiterParsing = true, matrixParameterParsing = true)`" should "successfully parse with path containing matrix parameters, and without query and fragment" in {
    implicit val c = UriConfig(delimiterParsing = true, matrixParameterParsing = true)
    val uri = ParseUri("/path1;matrixKey1=matrixValue1;matrixKey2=matrix value2/path2;matrixKey=matrixValue")
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path1", Seq(Parameter("matrixKey1", Some("matrixValue1")), Parameter("matrixKey2", Some("matrix value2")))), MatrixParametersSegment("path2", Seq(Parameter("matrixKey", Some("matrixValue"))))))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal("/path1;matrixKey1=matrixValue1;matrixKey2=matrix%20value2/path2;matrixKey=matrixValue")
  }

  "Parsing a `RelativePathReference` (must start with a relative path) with the default `UriConfig`" should "successfully parse without query and fragment" in {
    val uriString = "path"
    val uri = ParseUri(uriString)
    uri shouldBe a[RelativePathReference]
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
    val uri = ParseUri(uriString)
    uri shouldBe a[RelativePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with fragment, and without query" in {
    val uriString = "path#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[RelativePathReference]
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
    val uri = ParseUri(uriString)
    uri shouldBe a[RelativePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing empty segments, and without query and fragment" in {
    val uriString = "path1//path3/"
    val uri = ParseUri(uriString)
    uri shouldBe a[RelativePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path1"), EmptySegment, StringSegment("path3"), EmptySegment))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing matrix parameters, and without query and fragment" in {
    val uriString = "path1;matrixKey1=matrixValue1;matrixKey2=matrixValue2/path2;matrixKey=matrixValue"
    val uri = ParseUri(uriString)
    uri shouldBe a[RelativePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(StringSegment("path1;matrixKey1=matrixValue1;matrixKey2=matrixValue2"), StringSegment("path2;matrixKey=matrixValue")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  "Parsing a relative path reference URI (must start with a relative path) with `UriConfig(matrixParameterParsing = true)`" should "successfully parse with path containing matrix parameters, and without query and fragment" in {
    implicit val c = UriConfig(matrixParameterParsing = true)
    val uriString = "path1;matrixKey1=matrixValue1;matrixKey2=matrixValue2/path2;matrixKey=matrixValue"
    val uri = ParseUri(uriString)
    uri shouldBe a[RelativePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path1", Seq(Parameter("matrixKey1", Some("matrixValue1")), Parameter("matrixKey2", Some("matrixValue2")))), MatrixParametersSegment("path2", Seq(Parameter("matrixKey", Some("matrixValue"))))))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing matrix parameters (with no value, empty value and a value), and without query and fragment" in {
    implicit val c = UriConfig(matrixParameterParsing = true)
    val uriString = "path;matrixKey1;matrixKey2=;matrixKey3=matrixValue3"
    val uri = ParseUri(uriString)
    uri shouldBe a[RelativePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path", Seq(Parameter("matrixKey1", None), Parameter("matrixKey2", Some("")), Parameter("matrixKey3", Some("matrixValue3"))))))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with path containing some empty matrix parameters, and without query and fragment" in {
    implicit val c = UriConfig(matrixParameterParsing = true)
    val uri = ParseUri("path;;matrixKey2=matrixValue2;;matrixKey4=matrixValue4;")
    uri shouldBe a[RelativePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path", Seq(Parameter("matrixKey2", Some("matrixValue2")), Parameter("matrixKey4", Some("matrixValue4"))))))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toString should equal("path;matrixKey2=matrixValue2;matrixKey4=matrixValue4")
  }

  "Parsing a `QueryReference` (must start with a query) with the default `UriConfig`" should "successfully parse without fragment" in {
    val uriString = "?queryKey=queryValue"
    val uri = ParseUri(uriString)
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with fragment" in {
    val uriString = "?queryKey=queryValue#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with empty query parameter values, and without fragment" in {
    val uriString = "?queryKey1=&queryKey2=&queryKey3="
    val uri = ParseUri(uriString)
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey1", Some("")), Parameter("queryKey2", Some("")), Parameter("queryKey3", Some(""))))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty query parameter value and a fragment" in {
    val uriString = "?queryKey=#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some(""))))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with no query parameter values, and without fragment" in {
    val uriString = "?queryKey1&queryKey2&queryKey3"
    val uri = ParseUri(uriString)
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey1", None), Parameter("queryKey2", None), Parameter("queryKey3", None)))
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with a no query parameter value and a fragment" in {
    val uriString = "?queryKey#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", None)))
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty query, and without fragment" in {
    val uriString = "?"
    val uri = ParseUri(uriString)
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragment should equal(None)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty query and fragment" in {
    val uriString = "?#"
    val uri = ParseUri(uriString)
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value should equal(EmptyFragment)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty query and a fragment" in {
    val uriString = "?#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an improperly encoded query parameter value, and without fragment" in {
    val uri = ParseUri("?query_param_one=hello=world&query_param_two=false")
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("query_param_one", Some("hello=world")), Parameter("query_param_two", Some("false"))))
    uri.fragment should equal(None)
    uri.toString should equal("?query_param_one=hello%3Dworld&query_param_two=false")
  }

  "Parsing a `FragmentReference` (must be a fragment) with the default `UriConfig`" should "successfully parse" in {
    val uriString = "#fragment"
    val uri = ParseUri(uriString)
    uri shouldBe a[FragmentReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragment")
    uri.toString should equal(uriString)
  }

  it should "successfully parse with an empty fragment" in {
    val uriString = "#"
    val uri = ParseUri(uriString)
    uri shouldBe a[FragmentReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value should equal(EmptyFragment)
    uri.toString should equal(uriString)
  }

  it should "successfully parse with a fragment containing '#' with default `UriConfig`" in {
    val uriString = "#fragmentContaining#"
    val uri = ParseUri(uriString)
    uri shouldBe a[FragmentReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragmentContaining#")
    uri.toString should equal(uriString)
  }

  it should "fail parsing with a fragment containing '#' with `UriConfig.RFC3986`" in {
    a[java.net.URISyntaxException] should be thrownBy {
      ParseUri("#fragmentContaining#")(UriConfig.RFC3986)
    }
  }

  "Parsing a `FragmentReference` (must be a fragment) with the `UriConfig` with `delimiterParsing`" should "successfully parse" in {
    implicit val config = UriConfig(delimiterParsing = true)
    val uriString = "#fragmentContaining#"
    val uri = ParseUri(uriString)
    uri shouldBe a[FragmentReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("fragmentContaining#")
    uri.toString should equal(uriString)
  }

  "Parsing an `EmptyReference` with the default `UriConfig`" should "successfully parse" in {
    ParseUri("") should equal(EmptyReference)
    EmptyReference.toString should equal("")
  }

  "Parsing an invalid URI" should "fail with a invalid scheme" in {
    a[java.net.URISyntaxException] should be thrownBy {
      ParseUri("htt[://test.com/path")
    }
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  {
    val uriStringSimple = "scheme://registeredName/path?queryKey#fragment"
    val uriStringNotDns = "scheme://registered..name/path?queryKey#fragment"
    val uriStringFragmentWithHash = "scheme://registeredName/path?queryKey#fragment#"
    val uriStringComplex = "scheme://user:@registeredName:8080/path1/path2;key1=value1;key2;key3/path3;key4=value4/?queryKey1&queryKey2=queryValue2&queryKey3=#fragment"

    val stringSegments = Seq(StringSegment("path1"), StringSegment("path2;key1=value1;key2;key3"), StringSegment("path3;key4=value4"), EmptySegment)
    val matrixParameterSegments = Seq(StringSegment("path1"), MatrixParametersSegment("path2", Parameter("key1", Some("value1")), Parameter("key2"), Parameter("key3")), MatrixParametersSegment("path3", Parameter("key4", Some("value4"))), EmptySegment)
    val queryParameters = Seq(Parameter("queryKey1"), Parameter("queryKey2", Some("queryValue2")), Parameter("queryKey3", Some("")))

    "`Uri.option`" should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      Uri.option(uriStringFragmentWithHash) should equal(None)
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = false, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = false, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.userInfoString.value should equal("user:")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryString.value should equal("queryKey")
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = false, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      val uriNotDns = Uri.option(uriStringNotDns).value
      uriNotDns shouldBe a[SchemeWithAuthorityUri]
      uriNotDns.scheme.value.scheme should equal("scheme")
      uriNotDns.userInfo should equal(None)
      uriNotDns.hostString.value should equal("registered..name")
      uriNotDns.port should equal(None)
      uriNotDns.path.value shouldBe an[AbsolutePath]
      uriNotDns.pathSegments should equal(Seq(StringSegment("path")))
      uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriNotDns.fragment.value.fragment should equal("fragment")
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = false, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(stringSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = false)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = false, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryString.value should equal("queryKey")
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryString.value should equal("queryKey")
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryString.value should equal("queryKey1&queryKey2=queryValue2&queryKey3=")
      uriComplex.fragment.value.fragment should equal("fragment")
    }

    it should "perform as expected with `UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)`" in {
      implicit val config = UriConfig(delimiterParsing = true, userPasswordParsing = true, registeredNameMustBeDomainName = true, matrixParameterParsing = true, queryParameterParsing = true, fragmentAllowHashParsing = true)
      val uriSimple = Uri.option(uriStringSimple).value
      uriSimple shouldBe a[SchemeWithAuthorityUri]
      uriSimple.scheme.value.scheme should equal("scheme")
      uriSimple.userInfo should equal(None)
      uriSimple.hostString.value should equal("registeredName")
      uriSimple.port should equal(None)
      uriSimple.path.value shouldBe an[AbsolutePath]
      uriSimple.pathSegments should equal(Seq(StringSegment("path")))
      uriSimple.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriSimple.fragment.value.fragment should equal("fragment")
      Uri.option(uriStringNotDns) should equal(None)
      val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
      uriFragmentWithHash shouldBe a[SchemeWithAuthorityUri]
      uriFragmentWithHash.scheme.value.scheme should equal("scheme")
      uriFragmentWithHash.userInfo should equal(None)
      uriFragmentWithHash.hostString.value should equal("registeredName")
      uriFragmentWithHash.port should equal(None)
      uriFragmentWithHash.path.value shouldBe an[AbsolutePath]
      uriFragmentWithHash.pathSegments should equal(Seq(StringSegment("path")))
      uriFragmentWithHash.queryParameters.value should equal(Seq(Parameter("queryKey")))
      uriFragmentWithHash.fragment.value.fragment should equal("fragment#")
      val uriComplex = Uri.option(uriStringComplex).value
      uriComplex shouldBe a[SchemeWithAuthorityUri]
      uriComplex.scheme.value.scheme should equal("scheme")
      uriComplex.user.value should equal("user")
      uriComplex.password.value should equal("")
      uriComplex.hostString.value should equal("registeredName")
      uriComplex.port.value should equal(8080)
      uriComplex.path.value shouldBe an[AbsolutePath]
      uriComplex.pathSegments should equal(matrixParameterSegments)
      uriComplex.queryParameters.value should equal(queryParameters)
      uriComplex.fragment.value.fragment should equal("fragment")
    }
  }
}
