package com.netaporter.uri

class UriTests extends TestSpec {

  // NOTE: Uri.toString(...) tested in ParsingTests.
  // NOTE: Uri.apply(uriString: String) tested in DecodingTests.
  // NOTE: Uri.apply(...) tested in EncodingTests.

  "`Uri.copy`" should "should work" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme should not equal (None)
    uri.authority should not equal (None)
    uri.path should equal(None)
    uri.fragment should equal(None)
    val uri2 = uri.copy(path = AbsolutePath.option(StringSegment("path")))
    uri2 shouldBe a[SchemeWithAuthorityUri]
    uri2.path should not equal (None)
    val uri3 = uri2.copy(None)
    uri3 shouldBe a[NetworkPathReference]
    uri3.scheme should equal(None)
    val uri4 = uri3.copy(authority = None, fragment = Fragment.option("fragment"))
    uri4 shouldBe an[AbsolutePathReference]
    uri4.authority should equal(None)
    uri4.fragment should not equal (None)
    val uri5Path = RootlessPath.option(StringSegment("rootlessPath"))
    val uri5 = uri4.copy(Scheme.option("test"), path = uri5Path)
    uri5 shouldBe a[SchemeWithRootlessPathUri]
    uri5.scheme should not equal (None)
    uri5.path should equal(uri5Path)
  }

  "`Uri.equals`" should "should return `true` for the same Uri" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.equals(uri) should equal(true)
  }

  it should "should return `true` for an equivalent Uri" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    val uri2 = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.equals(uri2) should equal(true)
  }

  it should "should return `false` for a different Uri" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    val uri2 = Uri(Scheme.option("https"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.equals(uri2) should equal(false)
  }

  it should "should return `false` for `null`" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.equals(null) should equal(false)
  }

  it should "should return `false` for a `String`" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.equals("aString") should equal(false)
  }

  "`Uri.hashCode`" should "handle simple URI" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.hashCode should equal(-611751205)
  }

  "`Uri.toString`" should "handle simple URI" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.toString() should equal("http://www.example.com")
  }

  "`Uri.toUri` and `Uri.apply(java.net.URI)`" should "handle simple URI" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    val javaUri = uri.toURI
    javaUri.getScheme should equal("http")
    javaUri.getUserInfo should equal(null)
    javaUri.getHost should equal("www.example.com")
    javaUri.getPath should equal("")
    javaUri.getQuery should equal(null)
    javaUri.getFragment should equal(null)
    javaUri.toASCIIString should equal(uri.toString)
    val uriFromJava = Uri(javaUri)
    uriFromJava should equal(uriFromJava)
  }

  it should "handle scheme-less URI" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), AbsolutePath.option(StringSegment("test")), None, None)
    val javaUri = uri.toURI
    javaUri.getScheme should equal(null)
    javaUri.getUserInfo should equal(null)
    javaUri.getHost should equal("www.example.com")
    javaUri.getPath should equal("/test")
    javaUri.getQuery should equal(null)
    javaUri.getFragment should equal(null)
    javaUri.toASCIIString should equal(uri.toString)
    val uriFromJava = Uri(javaUri)
    uriFromJava should equal(uriFromJava)
  }

  it should "handle authenticated URI" in {
    val uri = Uri(Scheme.option("https"), Authority.option("user", "password", "www.example.com"), AbsolutePath.option(StringSegment("test")), None, None)
    val javaUri = uri.toURI
    javaUri.getScheme should equal("https")
    javaUri.getUserInfo should equal("user:password")
    javaUri.getHost should equal("www.example.com")
    javaUri.getPath should equal("/test")
    javaUri.getQuery should equal(null)
    javaUri.getFragment should equal(null)
    javaUri.toASCIIString should equal(uri.toString)
    val uriFromJava = Uri(javaUri)
    uriFromJava should equal(uriFromJava)
  }

  it should "handle exotic/reserved characters in query" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), AbsolutePath.option(StringSegment("test")), Query.option(Parameter("weird=&key", Some("strange%value")), Parameter("arrow", Some("⇔"))), None)
    val javaUri = uri.toURI
    javaUri.getScheme should equal("http")
    javaUri.getUserInfo should equal(null)
    javaUri.getHost should equal("www.example.com")
    javaUri.getPath should equal("/test")
    javaUri.getQuery should equal("weird=&key=strange%value&arrow=⇔")
    javaUri.getRawQuery should equal("weird%3D%26key=strange%25value&arrow=%E2%87%94")
    javaUri.toString should equal(uri.toString)
    javaUri.toASCIIString should equal(uri.toString)
    val uriFromJava = Uri(javaUri)
    uriFromJava should equal(uriFromJava)
  }

  "`SchemeWithAuthorityUri`" should "`apply` with mandatory arguments" in {
    SchemeWithAuthorityUri(Scheme("http"), Authority(registeredName = "test.com"), None, None, None)
  }

  it should "`apply` with all arguments" in {
    SchemeWithAuthorityUri(Scheme("http"), Authority(registeredName = "test.com"), AbsolutePath.option(StringSegment("path")), Query.option(Parameter("queryKey", Some("queryValue"))), Fragment.option("fragment"))
  }

  it should "be creatable from `Uri.apply` with `Option` arguments" in {
    val scheme = Scheme.option("http")
    val authority = Authority.option(registeredName = "test.com")
    val path = AbsolutePath.option(StringSegment("path"))
    val query = Query.option(Parameter("queryKey", Some("queryValue")))
    val fragment = Fragment.option("fragment")
    SchemeWithAuthorityUri(scheme.get, authority.get, path, query, fragment) should equal(Uri(scheme, authority, path, query, fragment))
  }

  "`SchemeWithAbsolutePathUri`" should "`apply` with mandatory arguments" in {
    SchemeWithAbsolutePathUri(Scheme("http"), AbsolutePath(StringSegment("path")), None, None)
  }

  it should "`apply` with all arguments" in {
    SchemeWithAbsolutePathUri(Scheme("http"), AbsolutePath(StringSegment("path")), Query.option(Parameter("queryKey", Some("queryValue"))), Fragment.option("fragment"))
  }

  it should "be creatable from `Uri.apply` with `Option` arguments" in {
    val scheme = Scheme.option("http")
    val path = AbsolutePath.option(StringSegment("path"))
    val query = Query.option(Parameter("queryKey", Some("queryValue")))
    val fragment = Fragment.option("fragment")
    SchemeWithAbsolutePathUri(scheme.get, path.get, query, fragment) should equal(Uri(scheme, None, path, query, fragment))
  }

  it should "fail `apply` with invalid arguments" in {
    an [IllegalArgumentException] should be thrownBy {
      SchemeWithAbsolutePathUri(Scheme("http"), AbsolutePath(StringSegment(""), StringSegment("path2")), Query.option(Parameter("queryKey", Some("queryValue"))), Fragment.option("fragment"))
    }
  }

  "`SchemeWithRootlessPathUri`" should "`apply` with mandatory arguments" in {
    SchemeWithRootlessPathUri(Scheme("http"), RootlessPath(StringSegment("path")), None, None)
  }

  it should "`apply` with all arguments" in {
    SchemeWithRootlessPathUri(Scheme("http"), RootlessPath(StringSegment("path")), Query.option(Parameter("queryKey", Some("queryValue"))), Fragment.option("fragment"))
  }

  it should "be creatable from `Uri.apply` with `Option` arguments" in {
    val scheme = Scheme.option("http")
    val path = RootlessPath.option(StringSegment("path"))
    val query = Query.option(Parameter("queryKey", Some("queryValue")))
    val fragment = Fragment.option("fragment")
    SchemeWithRootlessPathUri(scheme.get, path.get, query, fragment) should equal(Uri(scheme, None, path, query, fragment))
  }

  "`SchemeWithQueryUri`" should "`apply` with mandatory arguments" in {
    SchemeWithQueryUri(Scheme("http"), Query(Parameter("queryKey", Some("queryValue"))), None)
  }

  it should "`apply` with all arguments" in {
    SchemeWithQueryUri(Scheme("http"), Query(Parameter("queryKey", Some("queryValue"))), Fragment.option("fragment"))
  }

  it should "be creatable from `Uri.apply` with `Option` arguments" in {
    val scheme = Scheme.option("http")
    val query = Query.option(Parameter("queryKey", Some("queryValue")))
    val fragment = Fragment.option("fragment")
    SchemeWithQueryUri(scheme.get, query.get, fragment) should equal(Uri(scheme, None, None, query, fragment))
  }

  "`SchemeWithFragmentUri`" should "`apply` with mandatory arguments" in {
    SchemeWithFragmentUri(Scheme("http"), Fragment("fragment"))
  }

  it should "be creatable from `Uri.apply` with `Option` arguments" in {
    val scheme = Scheme.option("http")
    val fragment = Fragment.option("fragment")
    SchemeWithFragmentUri(scheme.get, fragment.get) should equal(Uri(scheme, None, None, None, fragment))
  }

  "`SchemeUri`" should "`apply` with mandatory arguments" in {
    SchemeUri(Scheme("http"))
  }

  it should "be creatable from `Uri.apply` with `Option` arguments" in {
    val scheme = Scheme.option("http")
    SchemeUri(scheme.get) should equal(Uri(scheme, None, None, None, None))
  }

  "`NetworkPathReference`" should "`apply` with mandatory arguments" in {
    NetworkPathReference(Authority(registeredName = "test.com"), None, None, None)
  }

  it should "`apply` with all arguments" in {
    NetworkPathReference(Authority(registeredName = "test.com"), AbsolutePath.option(StringSegment("path")), Query.option(Parameter("queryKey", Some("queryValue"))), Fragment.option("fragment"))
  }

  it should "be creatable from `Uri.apply` with `Option` arguments" in {
    val authority = Authority.option(registeredName = "test.com")
    val path = AbsolutePath.option(StringSegment("path"))
    val query = Query.option(Parameter("queryKey", Some("queryValue")))
    val fragment = Fragment.option("fragment")
    NetworkPathReference(authority.get, path, query, fragment) should equal(Uri(None, authority, path, query, fragment))
  }

  "`AbsolutePathReference`" should "`apply` with mandatory arguments" in {
    AbsolutePathReference(AbsolutePath(StringSegment("path")), None, None)
  }

  it should "`apply` with all arguments" in {
    AbsolutePathReference(AbsolutePath(StringSegment("path")), Query.option(Parameter("queryKey", Some("queryValue"))), Fragment.option("fragment"))
  }

  it should "be creatable from `Uri.apply` with `Option` arguments" in {
    val path = AbsolutePath.option(StringSegment("path"))
    val query = Query.option(Parameter("queryKey", Some("queryValue")))
    val fragment = Fragment.option("fragment")
    AbsolutePathReference(path.get, query, fragment) should equal(Uri(None, None, path, query, fragment))
  }

  it should "fail `apply` with invalid arguments" in {
    an [IllegalArgumentException] should be thrownBy {
      AbsolutePathReference(AbsolutePath(StringSegment(""), StringSegment("path2")), Query.option(Parameter("queryKey", Some("queryValue"))), Fragment.option("fragment"))
    }
  }

  "`RelativePathReference`" should "`apply` with mandatory arguments" in {
    RelativePathReference(RootlessPath(StringSegment("path")), None, None)
  }

  it should "`apply` with all arguments" in {
    RelativePathReference(RootlessPath(StringSegment("path")), Query.option(Parameter("queryKey", Some("queryValue"))), Fragment.option("fragment"))
  }

  it should "be creatable from `Uri.apply` with `Option` arguments" in {
    val path = RootlessPath.option(StringSegment("path"))
    val query = Query.option(Parameter("queryKey", Some("queryValue")))
    val fragment = Fragment.option("fragment")
    RelativePathReference(path.get, query, fragment) should equal(Uri(None, None, path, query, fragment))
  }

  it should "prepend a \".\" segment when the first provided segment contains ':'" in {
    val uri = RelativePathReference(RootlessPath(StringSegment("pathwith:")), None, None)
    uri.path.value.segments should equal(Seq(StringSegment("."), StringSegment("pathwith:")))
  }

  "`QueryReference`" should "`apply` with mandatory arguments" in {
    QueryReference(Query(Parameter("queryKey", Some("queryValue"))), None)
  }

  it should "`apply` with all arguments" in {
    QueryReference(Query(Parameter("queryKey", Some("queryValue"))), Fragment.option("fragment"))
  }

  it should "be creatable from `Uri.apply` with `Option` arguments" in {
    val query = Query.option(Parameter("queryKey", Some("queryValue")))
    val fragment = Fragment.option("fragment")
    QueryReference(query.get, fragment) should equal(Uri(None, None, None, query, fragment))
  }

  "`FragmentReference`" should "`apply` with mandatory arguments" in {
    FragmentReference(Fragment("fragment"))
  }

  it should "be creatable from `Uri.apply` with `Option` arguments" in {
    val fragment = Fragment.option("fragment")
    FragmentReference(fragment.get) should equal(Uri(None, None, None, None, fragment))
  }

  "`EmptyReference`" should "be creatable from `Uri.apply` with `Option` arguments" in {
    EmptyReference should equal(Uri(None, None, None, None, None))
  }

  "`Uri.apply` with `Option` arguments" should "fail with an invalid argument set (scheme and authority with rootless path)" in {
    an [IllegalArgumentException] should be thrownBy {
      Uri(Scheme.option("http"), Authority.option(registeredName = "www.test.com"), RootlessPath.option(StringSegment("path")), None, None)
    }
  }

  it should "fail with an invalid argument set (authority with rootless path)" in {
    an [IllegalArgumentException] should be thrownBy {
      Uri(None, Authority.option(registeredName = "www.test.com"), RootlessPath.option(StringSegment("path")), None, None)
    }
  }

  "`Uri.apply` with `null` default arguments" should "accept String scheme, String host and path" in {
    val uri = Uri(scheme = "http", host = "theon.github.com", port = 8080, pathParts = Seq(StringSegment("blah")))
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value should equal(Scheme("http"))
    uri.port.value should equal(8080)
    uri.pathSegments should equal(Seq(StringSegment("blah")))
    uri.query should equal(None)
  }

  it should "accept String scheme, String host and query" in {
    val qs = Query(Seq(Parameter("testKey", Some("testVal"))))
    val uri = Uri(scheme = "http", host = "theon.github.com", query = qs)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value should equal(Scheme("http"))
    uri.hostString.value should equal("theon.github.com")
    uri.query.value should equal(qs)
  }

  it should "accept query" in {
    val qs = Query(Seq(Parameter("testKey", Some("testVal"))))
    val uri = Uri(query = qs)
    uri shouldBe a[QueryReference]
    uri.query.value should equal(qs)
  }

  "`Uri.unapply`" should "work properly" in {
    val scheme = Scheme.option("http")
    val authority = Authority.option(registeredName = "www.test.com")
    val path = AbsolutePath.option(StringSegment("path"))
    val query = Query.option(Parameter("queryKey", Some("queryValue")))
    val fragment = Fragment.option("fragment")
    Uri.unapply(Uri(scheme, authority, path, query, fragment)) should equal(Some(scheme, authority, path, query, fragment))
  }

  it should "return `None` when passed `null`" in {
    Uri.unapply(null) should equal(None)
  }
}
