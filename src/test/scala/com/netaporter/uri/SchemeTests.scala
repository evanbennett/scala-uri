package com.netaporter.uri

class SchemeTests extends TestSpec {

  "`Uri.withScheme`" should "change the scheme when provided a `Uri`" in {
    val uri = Uri(Scheme.option("http"), Authority.option(host = "www.example.com"), None, None, None)
    uri.scheme.value.scheme should equal("http")
    val uri2 = Uri(Scheme.option("https"), Authority.option(host = "www.example.com"), None, None, None)
    uri.withScheme(uri2).scheme.value.scheme should equal("https")
  }

  it should "change the scheme when provided a `Scheme`" in {
    val uri = Uri(Scheme.option("http"), Authority.option(host = "www.example.com"), None, None, None)
    uri.scheme.value.scheme should equal("http")
    uri.withScheme(Scheme("https")).scheme.value.scheme should equal("https")
  }

  it should "change the scheme when provided a `String`" in {
    val uri = Uri(Scheme.option("http"), Authority.option(host = "www.example.com"), None, None, None)
    uri.scheme.value.scheme should equal("http")
    uri.withScheme("https").scheme.value.scheme should equal("https")
  }

  it should "remove the scheme when provided nothing" in {
    val uri = Uri(Scheme.option("http"), Authority.option(host = "www.example.com"), None, None, None)
    uri.scheme.value.scheme should equal("http")
    uri.withScheme().scheme should equal(None)
  }

  "`Uri.schemeToString` and therefore `Scheme.toString`" should "work with a simple URI" in {
    val uri = Uri(Scheme.option("http"), Authority.option(host = "www.example.com"), None, None, None)
    uri.schemeToString should equal("http:")
  }

  it should "work without a scheme" in {
    EmptyRelativeReference.schemeToString should equal("")
  }

  it should "always output in lowercase" in {
    val uri = Uri(Scheme.option("HTTP"), Authority.option(host = "www.example.com"), None, None, None)
    uri.schemeToString should equal("http:")
    val uri2 = Uri(Scheme.option("HtTp"), Authority.option(host = "www.example.com"), None, None, None)
    uri2.schemeToString should equal("http:")
    val uri3 = Uri(Scheme.option("httP"), Authority.option(host = "www.example.com"), None, None, None)
    uri3.schemeToString should equal("http:")
  }

  "`Uri.schemeToStringRaw` and therefore `Scheme.toStringRaw`" should "work with a simple URI" in {
    val uri = Uri(Scheme.option("http"), Authority.option(host = "www.example.com"), None, None, None)
    uri.schemeToStringRaw should equal("http:")
  }

  it should "work without a scheme" in {
    EmptyRelativeReference.schemeToStringRaw should equal("")
  }

  it should "always output in input case" in {
    val uri = Uri(Scheme.option("HTTP"), Authority.option(host = "www.example.com"), None, None, None)
    uri.schemeToStringRaw should equal("HTTP:")
    val uri2 = Uri(Scheme.option("HtTp"), Authority.option(host = "www.example.com"), None, None, None)
    uri2.schemeToStringRaw should equal("HtTp:")
    val uri3 = Uri(Scheme.option("httP"), Authority.option(host = "www.example.com"), None, None, None)
    uri3.schemeToStringRaw should equal("httP:")
  }

  "`Scheme.copy`" should "succeed" in {
    Scheme("http").copy("https").scheme should equal("https")
  }

  it should "fail when passed an empty string" in {
    intercept[IllegalArgumentException] {
      Scheme("http").copy("")
    }
  }

  it should "fail when passed `null`" in {
    intercept[IllegalArgumentException] {
      Scheme("http").copy(null)
    }
  }

  "`Scheme.apply`" should "succeed" in {
    Scheme("https").scheme should equal("https")
  }

  it should "fail when passed an invalid string (trailing ':')" in {
    intercept[IllegalArgumentException] {
      Scheme("https:")
    }
  }
  it should "fail when passed an invalid string (illegal characters)" in {
    intercept[IllegalArgumentException] {
      Scheme("ht$p")
    }
  }


  it should "fail when passed an empty string" in {
    intercept[IllegalArgumentException] {
      Scheme("")
    }
  }

  it should "fail when passed `null`" in {
    intercept[IllegalArgumentException] {
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
}
