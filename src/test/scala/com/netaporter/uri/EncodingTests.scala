package com.netaporter.uri

import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.encoding._

class EncodingTests extends TestSpec {

  "Default `PercentEncoder` in `Uri.toString(...)`" should "encode user" in {
    val uri = Uri(None, Authority.option("üser", registeredName = "test.com"), None, None, None)
    uri.toString should equal("//%C3%BCser@test.com")
  }

  it should "encode password" in {
    val uri = Uri(None, Authority.option("user", "p@ssword", registeredName = "test.com"), None, None, None)
    uri.toString should equal("//user:p%40ssword@test.com")
  }

  it should "encode absolute path" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("üris-in-scàla.html")), None, None)
    uri.toString should equal("/%C3%BCris-in-sc%C3%A0la.html")
  }

  it should "encode relative path" in {
    val uri = Uri(None, None, RootlessPath.option(StringSegment("uri with space")), None, None)
    uri.toString should equal("uri%20with%20space")
  }

  it should "encode query" in {
    val uri = Uri(None, None, None, Query.option(Parameter("càsh", Some("+£50")), Parameter("©opyright", Some("false"))), None)
    uri.toString should equal("?c%C3%A0sh=%2B%C2%A350&%C2%A9opyright=false")
  }

  it should "encode fragment" in {
    val uri = Uri(None, None, None, None, Fragment.option("fràgment"))
    uri.toString should equal("#fr%C3%A0gment")
  }

  it should "NOT encode path pchars" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "example.com"), AbsolutePath.option(StringSegment("-._~!$&'()*+,;=:@"), StringSegment("test")), None, None)
    uri.toString should equal("http://example.com/-._~!$&'()*+,;=:@/test")
  }

  it should "encode query with control characters" in {
    val uri = Uri(None, None, None, Query.option(Parameter("control", Some("\u0019\u007F"))), None)
    uri.toString should equal("?control=%19%7F")
  }

  it should "encode query with Russian characters" in {
    val uri = Uri(None, None, None, Query.option(Parameter("russian", Some("Скала"))), None)
    uri.toString should equal("?russian=%D0%A1%D0%BA%D0%B0%D0%BB%D0%B0")
  }

  it should "encode query with Chinese characters" in {
    val uri = Uri(None, None, None, Query.option(Parameter("chinese", Some("网址"))), None)
    uri.toString should equal("?chinese=%E7%BD%91%E5%9D%80")
  }

  "Default `PercentEncoder` in `Uri.toString(...)` with non-UTF8 encoding" should "encode query with Chinese characters" in {
    val uri = Uri(None, None, None, Query.option(Parameter("chinese", Some("网址"))), None)
    uri.toString(UriConfig(charset = "GB2312")) should equal("?chinese=%CD%F8%D6%B7")
  }

  "`Uri.toString(...)` with custom `PercentEncoder`" should "encode path with extra encoded characters easily" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("abcde")), None, None)
    uri.toString(UriConfig(encoder = PercentEncoder.default ++('a', 'b'))) should equal("/%61%62cde")
  }

  it should "encode query with less encoded characters easily" in {
    val uri = Uri(None, None, None, Query.option(Parameter("reserved", Some(":/?#[]@!$&'()*+,;={}\\\n\r"))), None)
    uri.toString(UriConfig(encoder = PercentEncoder.default -- '+')) should equal("?reserved=%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A+%2C%3B%3D%7B%7D%5C%0A%0D")
  }

  it should "encode query with custom encoded characters easily" in {
    val uri = Uri(None, None, None, Query.option(Parameter("reserved", Some(":/?#[]@!$&'()*+,;={}\\"))), None)
    uri.toString(UriConfig(encoder = PercentEncoder('#'))) should equal("?reserved=:/?%23[]@!$&'()*+,;={}\\")
  }

  "`UriConfig.conservative` in `Uri.toString(...)`" should "encode query with reserved characters" in {
    val uri = Uri(None, None, None, Query.option(Parameter("reserved", Some(":/?#[]@!$&'()*+,;={}\\\n\r"))), None)
    uri.toString(UriConfig.conservative) should equal("?reserved=%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D%7B%7D%5C%0A%0D")
  }

  it should "encode absolute path with '\"'" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment(""""that"""")), None, None)
    uri.toString(UriConfig.conservative) should equal("/%22that%22")
  }

  "`NoopEncoder` in `Uri.toString(...)`" should "process query with reserved characters wihtout encoding" in {
    val uri = Uri(None, None, None, Query.option(Parameter("reserved", Some(":/?#[]@!$&'()*+,;={}\\\n\r"))), None)
    uri.toString(UriConfig(encoder = NoopEncoder)) should equal("?reserved=:/?#[]@!$&'()*+,;={}\\\n\r")
  }

  "`Uri.toString(...)` with `EncodeCharAs`" should "encode path ' ' as '+'" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("uri with space")), None, None)
    uri.toString(UriConfig(encoder = EncodeCharAs.spaceAsPlus)) should equal("/uri+with+space")
  }

  it should "encode path ' ' as '_'" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("uri with space")), None, None)
    uri.toString(UriConfig(encoder = EncodeCharAs(' ', "_"))) should equal("/uri_with_space")
  }

  "`Uri.toString(...)` with `ChainedUriEncoder`" should "encode path" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("uri without space")), None, None)
    uri.toString(UriConfig(encoder = EncodeCharAs(' ', "") + EncodeCharAs('w', "W") + EncodeCharAs('s', "S"))) should equal("/uriWithoutSpace")
  }

  // NOTE: This is covered elsewhere, but is not detected as such:
  "`NoopEncoder`" should "encode a `Char` by returning it as a `String`" in {
    NoopEncoder.encodeChar('e') should equal("e")
  }
}
