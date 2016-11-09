package com.netaporter.uri

import com.netaporter.uri.decoding._

class DecodingTests extends TestSpec {

  "Default `PercentDecoder` in `Uri(uriString: String)`" should "decode exotic/reserved characters in the user info" in {
    val uri = Uri("//user%3A:p%40ssword%E2%87%94@theon.github.com")
    uri.scheme should equal(None)
    uri.userInfoString.value should equal("user::p@ssword⇔") // THEON: I am not sure how to handle this. The encoded '%' will not be re-encoded.
    uri.hostString.value should equal("theon.github.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "decode reserved characters in the path" in {
    val uri = Uri("/path%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D%7B%7D%5C%0A%0D")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path:/?#[]@!$&'()*+,;={}\\\n\r")))
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "decode exotic/reserved characters in the query" in {
    val uri = Uri("?weird%3D%26key=strange%25value&arrow=%E2%87%94")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq.empty)
    uri.queryParameters.value should equal(Seq(Parameter("weird=&key", Some("strange%value")), Parameter("arrow", Some("⇔"))))
    uri.fragment should equal(None)
  }

  it should "decode reserved characters in the query parameter value" in {
    val uri = Uri("?reserved=%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D%7B%7D%5C%0A%0D")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq.empty)
    uri.queryParameters.value should equal(Seq(Parameter("reserved", Some(":/?#[]@!$&'()*+,;={}\\\n\r"))))
    uri.fragment should equal(None)
  }

  it should "decode reserved characters in the fragment" in {
    val uri = Uri("#%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D%7B%7D%5C%0A%0D")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq.empty)
    uri.query should equal(None)
    uri.query should equal(None)
    uri.fragment.value.fragment should equal(":/?#[]@!$&'()*+,;={}\\\n\r")
  }

  it should "decode 2-byte groups" in {
    val uri = Uri("/%C2%A2?cents_sign=%C2%A2")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("¢")))
    uri.queryParameters.value should equal(Seq(Parameter("cents_sign", Some("¢"))))
    uri.fragment should equal(None)
  }

  it should "decode 3-byte groups" in {
    val uri = Uri("/%E2%82%AC?euro_sign=%E2%82%AC")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("€")))
    uri.queryParameters.value should equal(Seq(Parameter("euro_sign", Some("€"))))
    uri.fragment should equal(None)
  }

  it should "decode 4-byte groups" in {
    val uri = Uri("/%F0%9F%82%A0?ace_of_spades=%F0%9F%82%A1")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("\uD83C\uDCA0")))
    uri.queryParameters.value should equal(Seq(Parameter("ace_of_spades", Some("\uD83C\uDCA1"))))
    uri.fragment should equal(None)
  }

  it should "fail with a non-percent encoded user containing '%'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//us%er:password@test.com")
    }
  }

  it should "fail with a non-percent encoded password containing '%'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//user:pass%word@test.com")
    }
  }

  it should "fail with a non-percent encoded absolute path ending with '%'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("/path%")
    }
  }

  it should "fail with a non-percent encoded relative path starting with '%'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("%path")
    }
  }

  it should "fail with a non-percent encoded query containing '%'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("?query=abc%yum&john=hello")
    }
  }

  it should "fail with a non-percent encoded fragment containing '%'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("#frag%ment")
    }
  }

  "`NoopDecoder` in `Uri(...)`" should "parse with a non-percent encoded user containing '%'" in {
    implicit val c = UriConfig.DEFAULT.withDecoding(NoopDecoder)
    val uri = Uri("//%user:password@test.com")
    uri.scheme should equal(None)
    uri.userInfoString.value should equal("%user:password")
    uri.hostString.value should equal("test.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(None)
    uri.fragment should equal(None)
  }

  it should "parse with a non-percent encoded password containing '%'" in {
    implicit val config = UriConfig.DEFAULT.withDecoding(NoopDecoder)
    val uri = Uri("//user:pass%word@test.com")
    uri.scheme should equal(None)
    uri.userInfoString.value should equal("user:pass%word")
    uri.hostString.value should equal("test.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(None)
    uri.fragment should equal(None)
  }

  it should "parse with a non-percent encoded path containing '%'" in {
    implicit val config = UriConfig.DEFAULT.withDecoding(NoopDecoder)
    val uri = Uri("/path%")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path%")))
    uri.queryParameters should equal(None)
    uri.fragment should equal(None)
  }

  it should "parse with a non-percent encoded query containing '%'" in {
    implicit val config = UriConfig.DEFAULT.withDecoding(NoopDecoder)
    val uri = Uri("?query=abc%yum&john=hello")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("query", Some("abc%yum")), Parameter("john", Some("hello"))))
    uri.fragment should equal(None)
  }

  it should "parse with a non-percent encoded fragment containing '%'" in {
    implicit val config = UriConfig.DEFAULT.withDecoding(NoopDecoder)
    val uri = Uri("#frag%ment")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("frag%ment")
  }

  it should "successfully parse with user, password, host, port, path, query and fragment" in {
    val uri = Uri("http://user%3A:p%40ssword%E2%87%94@theon.github.com:8080/path%3A?queryKey=queryValue#fragment%3A")(UriConfig.DEFAULT.withNoDecoding)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("http")
    uri.userInfoString.value should equal("user%3A:p%40ssword%E2%87%94")
    uri.hostString.value should equal("theon.github.com")
    uri.port.value should equal(8080)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path%3A")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", Some("queryValue"))))
    uri.fragment.value.fragment should equal("fragment%3A")
  }

  "`PermissiveDecoder` in `Uri(...)`" should "parse with a non-percent encoded user containing '%' and encoded password" in {
    implicit val config = UriConfig.DEFAULT.withDecoding(PermissivePercentDecoder)
    val uri = Uri("//%user:p%40ssword@test.com")
    uri.scheme should equal(None)
    uri.userInfoString.value should equal("%user:p%40ssword")
    uri.hostString.value should equal("test.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  "`UriConfig(percentEncodingNormalization = false)`" should "parse with a non-percent encoded user containing '%' and encoded password" in {
    implicit val config = UriConfig(percentEncodingNormalization = false)
    val uri = Uri("//%user:p%40ssword@test.com")
    uri.scheme should equal(None)
    uri.userInfoString.value should equal("%user:p%40ssword")
    uri.hostString.value should equal("test.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }
}
