package com.netaporter.uri

class SchemeTests extends TestSpec {

  "`Uri.withScheme`" should "change the scheme when provided a `Uri`" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.scheme.value.scheme should equal("http")
    val uri2 = Uri(Scheme.option("https"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.withScheme(uri2).scheme.value.scheme should equal("https")
  }

  it should "change the scheme when provided a `Scheme`" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.scheme.value.scheme should equal("http")
    uri.withScheme(Scheme("https")).scheme.value.scheme should equal("https")
  }

  it should "change the scheme when provided a `String`" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.scheme.value.scheme should equal("http")
    uri.withScheme("https").scheme.value.scheme should equal("https")
  }

  it should "remove the scheme when provided nothing" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.scheme.value.scheme should equal("http")
    uri.withScheme().scheme should equal(None)
  }

  "`Uri.schemeToString` and therefore `Scheme.toString`" should "work with a simple URI" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.schemeToString should equal("http:")
  }

  it should "work without a scheme" in {
    EmptyReference.schemeToString should equal("")
  }

  it should "always output in lowercase" in {
    val uri = Uri(Scheme.option("HTTP"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.schemeToString should equal("http:")
    val uri2 = Uri(Scheme.option("HtTp"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri2.schemeToString should equal("http:")
    val uri3 = Uri(Scheme.option("httP"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri3.schemeToString should equal("http:")
  }

  "`Scheme.apply`" should "succeed" in {
    Scheme("https").scheme should equal("https")
  }

  it should "fail when passed an invalid string (trailing ':')" in {
    an [IllegalArgumentException] should be thrownBy {
      Scheme("https:")
    }
  }

  it should "fail when passed an invalid string (illegal characters)" in {
    an [IllegalArgumentException] should be thrownBy {
      Scheme("ht$p")
    }
  }

  it should "fail when passed an empty string" in {
    an [IllegalArgumentException] should be thrownBy {
      Scheme("")
    }
  }

  it should "fail when passed `null`" in {
    an [IllegalArgumentException] should be thrownBy {
      Scheme(null)
    }
  }

  "`Scheme.option`" should "return Some" in {
    Scheme.option("http").value.scheme should equal("http")
  }

  it should "return None when passed an empty string" in {
    Scheme.option("") should equal(None)
  }

  it should "return None when passed `null`" in {
    Scheme.option(null) should equal(None)
  }

  "`Scheme.unapply`" should "succeed" in {
    Scheme.unapply(Scheme("https")).value should equal("https")
  }
}
