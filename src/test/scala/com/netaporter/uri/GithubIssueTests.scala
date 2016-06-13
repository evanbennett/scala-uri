package com.netaporter.uri

import org.scalatest.{Matchers, OptionValues, FlatSpec}
import Uri._
import com.netaporter.uri.decoding.PermissivePercentDecoder
import com.netaporter.uri.config.UriConfig
import com.netaporter.uri

/**
 * Test Suite to ensure that bugs raised by awesome github peeps NEVER come back
 */
class GithubIssueTests extends FlatSpec with Matchers with OptionValues {

  import uri.dsl._

  "Github Issue #2" should "now be fixed. Pluses in querystrings should be encoded when using the conservative encoder" in {
    val uri = "http://theon.github.com/" ? ("+" -> "+")
    uri.toString(UriConfig.conservative) should equal("http://theon.github.com/?%2B=%2B")
  }

  "Github Issue #4" should "now be fixed. Port numbers should be rendered by toString" in {
    val uri = "http://theon.github.com:8080/test" ? ("p" -> "1")
    uri.toString should equal("http://theon.github.com:8080/test?p=1")
  }

  "Github Issue #5" should "now be fixed. The characters {} should now be percent encoded" in {
    val uri = ("http://theon.github.com" / "{}") ? ("{}" -> "{}")
    uri.toString should equal("http://theon.github.com/%7B%7D?%7B%7D=%7B%7D")
  }

  "Github Issue #6" should "now be fixed. No implicit Encoder val required for implicit Uri -> String conversion " in {
    val uri = "/blah" ? ("blah" -> "blah")
    val uriString: String = uri
    uriString should equal("/blah?blah=blah")
  }

  "Github Issue #7" should "now be fixed. Calling uri.toString() (with parentheses) should now behave the same as uri.toString " in {
    val uri = "/blah" ? ("blah" -> "blah")
    uri.toString() should equal("/blah?blah=blah")
  }

  "Github Issue #8" should "now be fixed. Parsed relative uris should have no scheme" in {
    val uri = parse("abc")

    uri.scheme should equal(None)
    uri.host should equal(None)
    uri.pathToString should equal("abc")
  }

  "Github Issue #15" should "now be fixed. Empty Query String values are parsed" in {
    val uri = parse("http://localhost:8080/ping?oi=TscV16GUGtlU&ppc=&bpc=")

    uri.scheme.value should equal("http")
    uri.host.value should equal("localhost")
    uri.port.value should equal(8080)
    uri.pathToString should equal("/ping")
    uri.query.params("oi") should equal(Vector(Some("TscV16GUGtlU")))
    uri.query.params("ppc") should equal(Vector(Some("")))
    uri.query.params("bpc") should equal(Vector(Some("")))
  }

  "Github Issue #12" should "now be fixed. Parsing URIs parse percent escapes" in {
    val source = new Uri(
      Some("http"),
      None,
      None,
      Some("xn--ls8h.example.net"),
      None,
      Some(AbsolutePath(Seq(PathPart(""), PathPart("path with spaces")))),
      QueryString(Vector("a b" -> Some("c d"))),
      None
    )
    val parsed = parse(source.toString)
    parsed should equal(source)
  }

  "Github Issue #19" should "now be fixed" in {
    val uri: Uri = "/coldplay.com?singer=chris%26will"
    uri.toString should equal("/coldplay.com?singer=chris%26will")
  }

  "Github Issue #26" should "now be fixed" in {
    implicit val c = UriConfig(decoder = PermissivePercentDecoder)
    val uri = "http://lesswrong.com/index.php?query=abc%yum&john=hello"
    val u = parse(uri)
    u.query.param("query") should equal(Some("abc%yum"))
  }

  "Github Issue #37" should "now be fixed" in {
    val uri = "http://test.com:8080" / "something"
    uri.toString should equal("http://test.com:8080/something")
  }

  "Github Issue #51" should "now be fixed. Long URI should not throw StackOverflowException in scala 2.11" in {
    if (util.Properties.versionString contains "2.11") {
      Uri.parse("http://www.counselingeducation.com/preroll/d/?id=44022&pid=2289033&subid=&size=300x250&e=xKS%2BPDcZDew%2BLDcx7z7hjoWHrPUQRWmjqoNs30jyvuRTh8XP3dCxgaPcjsF5DGAMVAaTJNGqdCoSU2PV6eax0GqflO%2BEDnrj44T6m72%2B%2F0Tv%2B4NCKEMbshFTAv27Dr%2FXp18roLvPT6rwfRcD46jr2qrGL8ru5C%2Ba3QQVpioQ92QPYaU39vFSwrVPU8GWWZcLMuN1s%2BKtqcOx7KXSFBiOGpNNxryydPRzMjI%2BhwmaT1z4ijVnpDjYnicAcX%2BuKtJRqM%2F%2BN9DscOxSZr2x4oSzpkptlTeHvJrj5kJVcrqTzC16tRsuYUulPd7uQblmMxcTsHRkQoGtcB2oOkqRHbW5I6NNWYKQtAnr69gYGAcEXf2a46wi9s7CH8%2FI6uMRvjwvuO0XLFxGVr3hF1V7qfNEIpNo0Lt94LOwefZqnp8fae14taQ1Rb7TGdA9BLIhsbQBmC1WWK2mIUd%2FjERFPhGdoyjcQqGfpGPetYE1yyAdukcubhPcDTNfvPM5DzLENqkefmBNb808irgpulr%2BkUYj2jm6iLRxcGim0EsntciBOPbxVTm7LxfU7D34Z5APCbiyopazfj05Ks5EiD%2FiwsEAJmMXJjKFQlHcpgNLTju3Ct8%2B3TcRstIzoThYphqY8HfuoAtZWCOy9HsSLwXFCqQyZVGO5im%2Fn1BPrF%2Fx%2BnCUfG7yH0laqdQdxaPnz32Ax9H3dscs5E2MfyRTOJVhe0cJLP7T%2B0JuFvhy3NU9Jk2Jx%2BPnWN8wn47O9XJDfnM%2FScIjXpNtiiXxu8b72l14HaM56P2GoIOvaLq2M1Gfullar0dmSkXFmPoI2NMViA7EHBk1zer7AAh7pBq7y10uEh%2BbaDBowavM5c7HF6%2BJW1NBrcFIKBpNr2SvotpFlfqh629ROGLkL54AhzkFHfJzJev3UrXXlpYPP9XdDzTpxdzt1sVHf1Za9JkVPXzyxTVxT2tUDHpW1BJwduCytmxgu8OmVqTvlkbBObhHzLR4sLbOeRUeVNYxndkXn197%2Fxg5%2FsgBchZikNfaAnZhphGTQHiL4DhHjEG2vUaQAZkM8mQBRGkKAFsHn2i%2B%2BGb%2B13ZMKJwbzlmqXhvTBp3e6vmwLY7Ouw0PGR2Q3%2FNU6tqJbZhbUcE6x0rBCszsVwu4qzkUTVU0PyzXYaws%2BKSvWPD1AbLPfcb1NujrAkZKoDiXS%2BoHon9JMDucWOT1IiO01EshpfwkW4oUrgO9GcBtZUpFprmNm24zCzi975RzMzZb9JbilXy3McoBUHJccUPDcyJcNoRNXFL%2BWpt3kQk8sw%2Bf3QNHIWP%2FM5b3uxhgYqKjEql%2FH130ft6dhMcLAozVhEPBeaJiP2NAuMSRGm4HWJpSbIxZz2TcvcsUOYRyS3AXMjN6JEBZ8st29XSS2Gd1jD6SypKB3S9okfnwQApEm4gRLwssSozyyP%2B%2Bu%2FhYd6F%2Btd7vEz5WPaxgwPHLEORaayotzguPJoe2M1ilxUnz%2FruESHMXMHnypw9Tnoz3%2FEd1Lp%2FN1x%2B0Ngrk9IJ8Kzsxc6gtdHYsNablFG0T8Ls%2FfiH1kcM06yMmIEpoMTQc%2BUx4l2qxd5s%2FoUgWMuAcu7jbGesjWJxvCbMfdWZSI%2Fk%2FSXw3WUoIjOZxWT1I8gQzvJo7AsTjXAtvnwRwJIF8KqVFn2IxFT1EUQrhlMdL9l0cAjWdX7iETnCG0HHXCahFlatsZ6Z3OIA8%2BE3PgsiIvpe0Sijo0Bhj5fBs2tbdoHSegcOz%2FCv2j52vz%2Bh9Qm8pXI2mlHK%2B1H4GwFsq6jZ3EHlSsdmFzDb75yaUqhAqkXR%2BIdTTe9NFgoTJNOjdGZYzKjboxD7W2kYQByyM%2Foapfi%2B%2BZ1K6mH2q4Jc%2B2r0MzyGhwwrjxlVQXrJlpSfDodVIVvtETP7PGQO0%2FcOaNwIvksFKkOnOfZu5PsTB2uMdGGwpVevfeqO4EbUKgdYiURznwAZ7M1jLp%2B1IFIr6xgYCHYitfMpMlvu2mjsy1k89VQyOmLmtY9hx2HqI18GZCS%2FivyDsYysEHFR3xxDGu1K53YhSFVVEaKyAQO9lX3wfJLKnDpWnhZ%2Bru0yuXb1s1lUvJ9E1jJC3U%2B9z%2FfnizvH86TzLRLCbyYUWPOosYRGSeLJcXHskmD0hJSnEtivmJT%2FXcbQ3OL%2BuD97ni1NeQnOo3blVtc546ezMOWr5KScgNih%2FZ8zGaC6i2%2BasVMmLVJNVo9sKypQ0U%2B4pFL7QzR0teTnkMpxf3FRvM4ZPIxhsQrt1C%2FUbQCyRqCt0r6mNjzbw%2FV5NB3HccohOpEowGluk8NV40FwRGXuWgm%2BgIZT9jwMyRaZBR80q8JLdbjB6JXiROs5Fq%2BZtuBzMZ%2BV8QDon%2Bbzq7hcDi1kXVjLBxWxmpb%2FdgqfMkKPJ6Qop723HX4kVhNcGa03AaRwontrm2R%2FU9Dp1V9PtEYFKtaiy8csoTFwVYnNo6pcRnA%2BnEjgLyAYS287FlpF4eTHex97U3Rt%2FJCURSRVXfIF3WglXFB8aLDcvg%2BMRt2niW9Js34BVrapopCWZ%2BC8L3MMpzBlm6SVGlgiqzwt%2B1vVIkdMqu9v34Mc0qIay8ZlhtasYQDR5TXoX%2FBS8x3mZVXk36TEulqs%2Bys9XMJhz6QZ9L15LieRDwzCwyI1RHFu1IioKHwxXOOESlwtws5LotYme2UMQSjU2dEGvb3XrOqaAWODJT%2FwujSRoRVq0e8pzbcqrNPM1E0xnWKHnRCn44L0CJ4BXUM4J3BiE1es2RsxOh2e%2FNy%2FsfyFw%2FdYmt%2FUBVrx4rRfXalkbZFtzcNpoQNJo9blxAn5yvMIw8Bgo5aVRYwucWJaCH56jCCOsQkMarAb7Ob41gClaVJpY1ldVEACrnSsd4szk54E7I3R6PYQYp9tu8WbcN8hV%2FvSRvdY7FGZ249bFPbQx4LbjwTXd%2Bx%2FQmyIVbp2B85KeffWM71N8xLAIr1KaLAbX5Wm%2Bju4yuiyQv1ZJK8DpD9Unro5cWrcW1AOI4kXraeQvuWCgLZ%2FrZDhEo282hS6CR%2Fw5CjmOsEyMemjTHBIFacIo2i4t1ifbX5exKwCtME7CWg%2BdD4bBfvhFejqhxC%2BwpMTjejtr15Tekpup1gX1CmsFX4BHQ0fNb64k9Zr9DdaCna5hhVFoVhOEEMl67A2i9SvClr1uMIq7mfwYdn49esiWueotngWLpUBuMQsYBLKhXq72dur6BeA%2B4nv7L8OS5yQq1Q2UdyR573ONsHlKDQZAa34d1v1K%2BcnoUAq9t9VsI8HF6H8xvsH1Xlgv33S6IgRW4t%2FtQOjhmAhEDkkCaNHdVWj%2F72%2FXc%2BXTfu5E%2Ft1Z4DDBUXyylWrheUpGYDIzaWCDSnIudTw4YtLrT3URz5R49N85aAIJHvrOMFJgZcDoOgRqRMh%2FH2FiawltY6BW379Gx8ypM%2FSnJZY6h7pLhPRIJhTbU2eH8mfAF8kKJFVrCsmdWjqxxe4M1mJFSoYof%2FrZVvBbpcVHaf0KkBA2H4LQJz9aW1ZQiMRpt91agU7a6Ki9iSOnXPYu9jROCPMjrrm8udxqs9BBQtfwfFCuahvK6SDMIm81qE3vVYQHeDnQXqgCGQlHAtnobObxR6R0IEiDBCFnQCX2ZTicZ2wjbSq8Bm2GWnuA3IIV%2Fs9J3XLtA5sjcYQ%2FHoMcbPrCoFSd5AixaKlEIBQgtmIKtG3EHXTtlcudRoDKFTN0mdLd98DeSDHs71iXutTnfcppPYlduQFtfcG54GmAZbX7%2BxW4g%2FkZ1fBK%2BY0qDtwt1cnj0Okq3uezonM5GRa97MOvIN8B3k4PtAjUjyHGMo3TE%2FKWCb8NvF3VharBChKo7AL%2BpFzBw68GAsBKgEcyNzbavAMNfSwMXEIqKhQe9muUd0SPGrNvkWm0Rg7fltgvbsQbdpbpFFXLJetqgdZIE4Ux8959yIBfO3f2IJoTsx01kiFo0prRgYYBKo5Fl6FSyj92DpdA%2B4%2BJIynn%2FHggHxJFZvw1dnvvTcgpBR31j2mhLqg7%2F7mLkjYjh3ujVuXs6DynZSgIx2owCnU3peEEcTBvzFPj%2FJYY72QUI5EyrURwGKkkVG1fqrgrz%2F047e0957KicIxKMqm9F6pEsdskWSR3HVpp6jdRF%2F48GlVdcEuG%2FghloHKnQciUOLsZ6MlmOKS22rQ9yYKU0DuyWZOdtHGHB2O0FO4pjjmjf3IZX6lqk%2F909pEQve42JwS3u06nkL4LE8v%2BKLUDR%2B9cffor1PRKcw77BrT8TKM2ihSm1Ixx1RKjGG95matE6UDsq%2FzUVRrRrK5MsoS3eEAX5Mcgnp6AO2gts%2F7N6wzTVdPvyoiQVxknQcYHaZ9saAB4ZDmKPAyVFWopEi1XJ58Ah7EGruOc8NTAOynf8OlGXr2E9lFniz2oi05T4XrDKRaHcCmldQWn4NsiCbfcCEfwA99LMRCN0M7qMQVBpDvnwdkS04ih0AhiKzZuAWgoEVjsjYLrg9YBHuVjdvfaFejOJokXo3JllL2mkkqJP1Wxf02480xbB9xlVK7Ma4wGRH2hOfIbn%2FZExULl59HKnJFh5mBE4vjOg2lpDhJJ0OEMxrZ9xW2Vpi5OwpRP%2B66MOMmGeRv99pcFQ3sbLf0Utr4Z4IhQN2TyFYHPlmLZtdm6nJDTmZ1k4MqkL1WcXGChP3O64yhYQfN%2FgRL1B6fBxDPzuv8d%2F%2FG5wegQV%2Bdu75XXe7BWFzbPAjtvdhAtLaOwzJqCRBHigmDDgfKkvBUUvvivbYMuvzyHYN9czCwjKiCvukiyOvScpsreZ8yrVMs7S5ZETIfvQu95%2BzOXwVOa%2BL8f2vRKl7Z6Cs0YdjXBa4ZZ%2FFL2kiZ%2FSwsbZkYVeTYGO0K%2F8jzXHObqIf%2BwbeJnKf1k%2BZWYkkKQc6HP97hQbDRy34letmFX6IwC5YYgykLqUBk0Vwwc%2FAswtS5CNlEfZ%2FjOhjNcl%2FMc8FPqqKPtdbfSUX8pJr7U70eNKDToH888%2FBwTBn92MJnJQ%2Fj8AtRZcJRVwspbWHHeS56%2BCJ3%2FyvQdagxlirxC2x3nCOmY0FZkXufdoZh2OLaZfOWL6l3FYuyYSetM76mq1II4K%2B8dg3b2xIgCF0%2FP8eZj1uea9oE%2BqnaAiFKQ5EOygrZOjmsbL47USKWjjqYXrpFrFCQ3IN1bA0ivPiv5LWgS7f2GWS0A2qW4pEnR7OMEEMjwV0s2jC6ecaCExvcY7YGTBJ0xKqsIj%2F6TNjFgEo1Hj8DOKgyhFDv3Xl%2FYBme71CeEusAXEe0hNH%2BB%2B1JSDq8NKaz06J%2BCKH1p4n5F%2Bs1vTDuUGFg5W4LM6I7Jc1jvUJ%2FRj%2BL0g7nlrfSA1M1ESt3u%2BPb2w1C7%2BFgttSbf8GVo1JuybI8mG3r6diOzygBD5bsL%2F7E1UlIozRoILPz3eBq40Cny5JeoyrW4a0GO7E6wZZCkK%2BbQlvjuITDFqijTCvN6ElmaCZVLatgEH9sskRwzn4LvosWDk4sqj7j6WSxwRTFX12SVYabAhsq9he40711O7tLTMBxgi4xM5i6GEn7kOyPIU%2BRjLMUCTPyVny26fTTWlB%2F1WRhYuyD9H49Bs8xtTHkajxrn%2BMkkPLLnAuGKnwaLrtHU6EacS6AL2Y%2F%2BWvye6xHxfhV99Rk%2FMM1XCIOoV8O4i4Hlt2E4OkcM8tt7HEulI%2F7fzv%2BMjuHjpVnCiUIJY6jpeMOF2co62cIESfkePXeDLykAdGW88rxCiBHRV6CZMmvuiE6MIR6C3mIIOvBNteY7nl8wRbzla00qs7lNBWhsDPVIeyKt5Y6EPVKGIKhkzRUaugbK6rocl9HvyYJJ2Z2b7P%2BtQ3zFbNI2V4wc7oYe%2Ba1atiTQwflqUKezDILr20s5w%2FBaghY%2BjcjVeBkSXDue6r1FxvuSa22DDflc388g49a1QfC%2BOR0OPQCmKIB1xYSpY5Niw%2F7EkyMRDOEim0cOHxmsDDU3kokSA4DCWIVI0XdeMue1EojvycfKEeBBTw4Fuga6qx6XRZawiFY%2BSWT2Frb0C69qMm6xLvp1lZZkd1SaXzBGQ%2FcEQAl8lirNDJA1ZojtcZEQYqkpzaR7p47gg8IA2gQwxRNuYShzWoxEfn7q39CGqFxeuC6obeDPoLjf07VNnv9jGt%2FrMqHTmIu0SZOF1QKs1m7J5dsC6ABd3e0x5eI4eEHcjMzk0r6TidyIzD%2BkEBjOrkyjQ%2FbMvkAoL9ZqScdRS02oSPyOyDhtP8TN7HE3kWkOPOZeIyuH39pAYW%2FOv%2FRy7w3iKaSKWkqMPNhsNm8142VBspXtc3%2FoOWXCgM4G%2BCYrTBdDOiuhlY757YaPGjo1htNdau5yq89yN%2F89XfRQ%2Fc4pMh%2FNcPaZifxR1C2t69gV1qAwudCJ9BLinlvrUwQ1aUhLlW2W7uRrWrJ966kLVuIo2Nr%2BpE%2F9NINcw02QHbaY1hRbc7iBcCMNkUio7xF8xM9e%2FhYuD%2F2g%2BXF0NkxJOfQPOuClDTrw9cHhkKORhAgpujUiB%2Fri1yDEPuD7o%2FqdF713f7%2BqXn32iMD2fsEiEPBHOjQ8lG4OdhPJyyumzkv020jbNRGCCUa0KvgRZB6YZgGpCeJRvsRffLue8uzo59LNt9cSmEQyapdaHgzO2vCtN14OM6CnK5COET%2Bxkvch%2FQNnvTCcKYaMTv61lVlf4cy0SSmWnj8HGVCs3%2FJEcIkcECenFp4z4T%2F96uJQThSFITbARMePzOv%2Bj5dRnWPNf5o%2Fo35PFeQ3IwoO%2Bhf808s0%2FOEb1XCZxoAoUtcN3jSEX6FOt091jz4GDU1pM1VGLALMTVZVD997pdMA4udTpmQd%2BWz8E8qAlOES4HZ2Syi49g9MsO8LCSq0MffQZhVGyCa42TPympTcJN1OKVj4B5M%2FcDngK6194ksT2O0HHHNVRrHVXOiT3Jt1P20Hr7g6Ie7Mk%2BtSivxRx455BcuITjhafdTEW%2BgN570RNkdjX79nyQpHz%2BirIjiwNJi07pQ069MqHglf7P%2BIPiKzea%2BIprmHPZ96%2ByZfOFChzAnSvI86GuyCGiKmLNcI1zu6%2FHUC%2Fg11yVjIs5DOE4jN")
    }
  }

  "Github Issue #55" should "now be fixed" in {
    val uri: Uri = "http://localhost:9002/iefjiefjief-efefeffe-fefefee/toto?access_token=ijifjijef-fekieifj-fefoejfoef&gquery=filter(time_before_closing%3C=45)"
    uri.query.param("gquery") should equal(Some("filter(time_before_closing<=45)"))
    uri.toString should equal("http://localhost:9002/iefjiefjief-efefeffe-fefefee/toto?access_token=ijifjijef-fekieifj-fefoejfoef&gquery=filter(time_before_closing%3C%3D45)")
  }

  "Github Issue #56" should "now be fixed" in {
    val ex = the [java.net.URISyntaxException] thrownBy Uri.parse("http://test.net/##")
    ex.getMessage should startWith("Invalid URI could not be parsed.")
  }

  "Github Issue #65 example 1" should "now be fixed" in {
    val uri = Uri.parse("http://localhost:9000/?foo=test&&bar=test")
    uri.toString should equal("http://localhost:9000/?foo=test&&bar=test")
  }

  "Github Issue #65 example 2" should "now be fixed" in {
    val uri = Uri.parse("http://localhost:9000/mlb/2014/06/15/david-wrights-slump-continues-why-new-york-mets-franchise-third-baseman-must-be-gone-before-seasons-end/?utm_source=RantSports&utm_medium=HUBRecirculation&utm_term=MLBNew York MetsGrid")
    uri.toString should equal("http://localhost:9000/mlb/2014/06/15/david-wrights-slump-continues-why-new-york-mets-franchise-third-baseman-must-be-gone-before-seasons-end/?utm_source=RantSports&utm_medium=HUBRecirculation&utm_term=MLBNew%20York%20MetsGrid")
  }

  "Github Issue #65 example 3" should "now be fixed" in {
    val uri = Uri.parse("http://localhost:9000/t?x=y%26")
    uri.query.param("x") should equal(Some("y&"))
    uri.toString should equal("http://localhost:9000/t?x=y%26")
  }

  "Github Issue #65 example 4" should "now be fixed" in {
    val uri = Uri.parse("http://localhost/offers.xml?&id=10748337&np=1")
    uri.toString should equal("http://localhost/offers.xml?&id=10748337&np=1")
  }

  "Github Issue #65 example 5" should "now be fixed" in {
    val uri = Uri.parse("http://localhost/offers.xml?id=10748337&np=1&")
    uri.toString should equal("http://localhost/offers.xml?id=10748337&np=1&")
  }

  "Github Issue #65 example 6" should "now be fixed" in {
    val uri = Uri.parse("http://localhost/offers.xml?id=10748337&np=1&#anchor")
    uri.toString should equal("http://localhost/offers.xml?id=10748337&np=1&#anchor")
  }

  "Github Issue #68" should "now be fixed" in {
    val uri = ("http://example.com/path" ? ("param" -> "something==")).toString
    uri.toString should equal("http://example.com/path?param=something%3D%3D")
  }

  "Github Issue #72" should "now be fixed" in {
    val uri = Uri.parse("http://hello.world?email=abc@xyz")
    uri.host should equal(Some("hello.world"))
    uri.query.param("email") should equal(Some("abc@xyz"))
  }

  "Github Issue #73" should "now be fixed" in {
    val uri = "http://somewhere.something".withUser("user:1@domain").withPassword("abc xyz")
    uri.toString should equal("http://user%3A1%40domain:abc%20xyz@somewhere.something")
  }

  "Github Issue #99" should "now be fixed" in {
    val uri = Uri.parse("https://www.foo.com/#/myPage?token=bar")
    uri.toString should equal("https://www.foo.com/#/myPage?token=bar")
  }

  "Github Issue #104" should "now be fixed" in {
    val uri = Uri.parse("a1+-.://localhost")
    uri.scheme should equal(Some("a1+-."))
    uri.host should equal(Some("localhost"))
  }

  "Github Issue #106" should "now be fixed" in {
    val p = "http://localhost:1234"

    val withPath = p / "some/path/segments"
    withPath.toString should equal("http://localhost:1234/some/path/segments")

    val withPathAndQuery = p / "some/path/segments" ? ("returnUrl" -> "http://localhost:1234/some/path/segments")
    withPathAndQuery.toString should equal("http://localhost:1234/some/path/segments?returnUrl=http://localhost:1234/some/path/segments")
  }
}
