package com.netaporter.uri

import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.decoding._

class DecodingTests extends TestSpec {

  "Default `PercentDecoder` in `Uri.parse(...)`" should "decode exotic/reserved characters in the user info" in {
    val uri = Uri.parse("//user%3A:p%40ssword%E2%87%94@theon.github.com")
    uri.scheme should equal(None)
    uri.user.value should equal("user:")
    uri.password.value should equal("p@ssword⇔")
    uri.host.value should equal("theon.github.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toStringRaw should equal("//user::p@ssword⇔@theon.github.com")
  }

  it should "decode reserved characters in the path" in {
    val uri = Uri.parse("/path%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D%7B%7D%5C%0A%0D")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path:/?#[]@!$&'()*+,;={}\\\n\r")))
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toStringRaw should equal("/path:/?#[]@!$&'()*+,;={}\\\n\r")
  }

  it should "decode exotic/reserved characters in the query" in {
    val uri = Uri.parse("?weird%3D%26key=strange%25value&arrow=%E2%87%94")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq.empty)
    uri.queryParameters should equal(Seq(Parameter("weird=&key", Some("strange%value")), Parameter("arrow", Some("⇔"))))
    uri.fragment should equal(None)
    uri.toStringRaw should equal("?weird=&key=strange%value&arrow=⇔")
  }

  it should "decode reserved characters in the query parameter value" in {
    val uri = Uri.parse("?reserved=%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D%7B%7D%5C%0A%0D")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq.empty)
    uri.queryParameters should equal(Seq(Parameter("reserved", Some(":/?#[]@!$&'()*+,;={}\\\n\r"))))
    uri.fragment should equal(None)
    uri.toStringRaw should equal("?reserved=:/?#[]@!$&'()*+,;={}\\\n\r")
  }

  it should "decode reserved characters in the fragment" in {
    val uri = Uri.parse("#%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D%7B%7D%5C%0A%0D")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq.empty)
    uri.query should equal(None)
    uri.query should equal(None)
    uri.fragment.value.fragment should equal(":/?#[]@!$&'()*+,;={}\\\n\r")
    uri.toStringRaw should equal("#:/?#[]@!$&'()*+,;={}\\\n\r")
  }

  it should "decode 2-byte groups" in {
    val uri = Uri.parse("/%C2%A2?cents_sign=%C2%A2")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("¢")))
    uri.queryParameters should equal(Seq(Parameter("cents_sign", Some("¢"))))
    uri.fragment should equal(None)
    uri.toStringRaw should equal("/¢?cents_sign=¢")
  }

  it should "decode 3-byte groups" in {
    val uri = Uri.parse("/%E2%82%AC?euro_sign=%E2%82%AC")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("€")))
    uri.queryParameters should equal(Seq(Parameter("euro_sign", Some("€"))))
    uri.fragment should equal(None)
    uri.toStringRaw should equal("/€?euro_sign=€")
  }

  it should "decode 4-byte groups" in {
    val uri = Uri.parse("/%F0%9F%82%A0?ace_of_spades=%F0%9F%82%A1")
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("\uD83C\uDCA0")))
    uri.queryParameters should equal(Seq(Parameter("ace_of_spades", Some("\uD83C\uDCA1"))))
    uri.fragment should equal(None)
    uri.toStringRaw should equal("/\uD83C\uDCA0?ace_of_spades=\uD83C\uDCA1")
  }

  it should "fail with a non-percent encoded user containing '%'" in {
    intercept[java.net.URISyntaxException] {
      Uri.parse("//us%er:password@test.com")
    }
  }

  it should "fail with a non-percent encoded password containing '%'" in {
    intercept[java.net.URISyntaxException] {
      Uri.parse("//user:pass%word@test.com")
    }
  }

  it should "fail with a non-percent encoded absolute path ending with '%'" in {
    intercept[java.net.URISyntaxException] {
      Uri.parse("/path%")
    }
  }

  it should "fail with a non-percent encoded relative path starting with '%'" in {
    intercept[java.net.URISyntaxException] {
      Uri.parse("%path")
    }
  }

  it should "fail with a non-percent encoded query containing '%'" in {
    intercept[java.net.URISyntaxException] {
      Uri.parse("?query=abc%yum&john=hello")
    }
  }

  it should "fail with a non-percent encoded fragment containing '%'" in {
    intercept[java.net.URISyntaxException] {
      Uri.parse("#frag%ment")
    }
  }

  "`NoopDecoder` in `Uri.parse(...)`" should "parse with a non-percent encoded user containing '%'" in {
    implicit val c = UriConfig(decoder = NoopDecoder)
    val uriString = "//%user:password@test.com"
    val uri = Uri.parse(uriString)
    uri.scheme should equal(None)
    uri.user.value should equal("%user")
    uri.password.value should equal("password")
    uri.host.value should equal("test.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq.empty)
    uri.fragment should equal(None)
    uri.toStringRaw should equal(uriString)
  }

  it should "parse with a non-percent encoded password containing '%'" in {
    implicit val c = UriConfig(decoder = NoopDecoder)
    val uriString = "//user:pass%word@test.com"
    val uri = Uri.parse(uriString)
    uri.scheme should equal(None)
    uri.user.value should equal("user")
    uri.password.value should equal("pass%word")
    uri.host.value should equal("test.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq.empty)
    uri.fragment should equal(None)
    uri.toStringRaw should equal(uriString)
  }

  it should "parse with a non-percent encoded path containing '%'" in {
    implicit val c = UriConfig(decoder = NoopDecoder)
    val uriString = "/path%"
    val uri = Uri.parse(uriString)
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("path%")))
    uri.queryParameters should equal(Seq.empty)
    uri.fragment should equal(None)
    uri.toStringRaw should equal(uriString)
  }

  it should "parse with a non-percent encoded query containing '%'" in {
    implicit val c = UriConfig(decoder = NoopDecoder)
    val uriString = "?query=abc%yum&john=hello"
    val uri = Uri.parse(uriString)
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters should equal(Seq(Parameter("query", Some("abc%yum")), Parameter("john", Some("hello"))))
    uri.fragment should equal(None)
    uri.toStringRaw should equal(uriString)
  }

  it should "parse with a non-percent encoded fragment containing '%'" in {
    implicit val c = UriConfig(decoder = NoopDecoder)
    val uriString = "#frag%ment"
    val uri = Uri.parse(uriString)
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value.fragment should equal("frag%ment")
    uri.toStringRaw should equal(uriString)
  }

  "`PermissiveDecoder` in `Uri.parse(...)`" should "parse with a non-percent encoded user containing '%' and encoded password" in {
    implicit val c = UriConfig(decoder = PermissivePercentDecoder)
    val uri = Uri.parse("//%user:p%40ssword@test.com")
    uri.scheme should equal(None)
    uri.user.value should equal("%user")
    uri.password.value should equal("p@ssword")
    uri.host.value should equal("test.com")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
    uri.toStringRaw should equal("//%user:p@ssword@test.com")
  }
}
