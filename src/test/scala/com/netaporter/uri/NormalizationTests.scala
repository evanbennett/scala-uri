package com.netaporter.uri

class NormalizationTests extends TestSpec {

  // NOTE: Decoding tested in DecodingTests.
  // NOTE: Encoding tested in EncodingTests.

  "Case normalization" should "should work" in {
    val uri = Uri(Scheme.option("htTP"), Authority.option(registeredName = "www.examPLe.com"), None, None, None)
    uri.toString should equal("http://www.example.com")
  }

  it should "should work with no effect" in {
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.toString should equal("http://www.example.com")
  }

  it should "should work when disabled" in {
    implicit val config = UriConfig(caseNormalization = false)
    val uri = Uri(Scheme.option("htTP"), Authority.option(registeredName = "www.examPLe.com"), None, None, None)
    uri.toString should equal("htTP://www.examPLe.com")
  }

  it should "should work when disabled with no effect" in {
    implicit val config = UriConfig(caseNormalization = false)
    val uri = Uri(Scheme.option("http"), Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.toString should equal("http://www.example.com")
  }

  val segmentsWithDotSegments = Seq(StringSegment("."), StringSegment("shouldBeRemoved"), StringSegment(".."), StringSegment("segment"))
  val segmentsAfterPathSegmentNormalization = Seq(StringSegment("segment"))
  val uriConfigWithDisabledPathSegmentNormalization = UriConfig.DEFAULT.copy(pathSegmentNormalization = false)

  "Path segment normalization" should "should by default remove dot segments in a scheme with authority uri" in {
    val uri = SchemeWithAuthorityUri(Scheme("scheme"), Authority(registeredName = "host"), AbsolutePath.option(segmentsWithDotSegments), None, None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(segmentsAfterPathSegmentNormalization)
  }

  it should "should when specified, not remove dot segments in a scheme with authority uri" in {
    val uri = SchemeWithAuthorityUri(Scheme("scheme"), Authority(registeredName = "host"), AbsolutePath.option(segmentsWithDotSegments), None, None)(uriConfigWithDisabledPathSegmentNormalization)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(segmentsWithDotSegments)
  }

  it should "should by default remove dot segments in a scheme with absolute path uri" in {
    val uri = SchemeWithAbsolutePathUri(Scheme("scheme"), AbsolutePath(segmentsWithDotSegments), None, None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(segmentsAfterPathSegmentNormalization)
  }

  it should "should when specified, not remove dot segments in a scheme with absolute path uri" in {
    val uri = SchemeWithAbsolutePathUri(Scheme("scheme"), AbsolutePath(segmentsWithDotSegments), None, None)(uriConfigWithDisabledPathSegmentNormalization)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(segmentsWithDotSegments)
  }

  it should "should by default remove dot segments in a scheme with rootless path uri" in {
    val uri = SchemeWithRootlessPathUri(Scheme("scheme"), RootlessPath(segmentsWithDotSegments), None, None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(segmentsAfterPathSegmentNormalization)
  }

  it should "should when specified, not remove dot segments in a scheme with rootless path uri" in {
    val uri = SchemeWithRootlessPathUri(Scheme("scheme"), RootlessPath(segmentsWithDotSegments), None, None)(uriConfigWithDisabledPathSegmentNormalization)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(segmentsWithDotSegments)
  }

  it should "should by default remove dot segments in a network path reference" in {
    val uri = NetworkPathReference(Authority(registeredName = "host"), AbsolutePath.option(segmentsWithDotSegments), None, None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(segmentsAfterPathSegmentNormalization)
  }

  it should "should when specified, not remove dot segments in a network path reference" in {
    val uri = NetworkPathReference(Authority(registeredName = "host"), AbsolutePath.option(segmentsWithDotSegments), None, None)(uriConfigWithDisabledPathSegmentNormalization)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(segmentsWithDotSegments)
  }

  it should "should by default remove dot segments in an absolute path reference" in {
    val uri = AbsolutePathReference(AbsolutePath(segmentsWithDotSegments), None, None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(segmentsAfterPathSegmentNormalization)
  }

  it should "should when specified, not remove dot segments in an absolute path reference" in {
    val uri = AbsolutePathReference(AbsolutePath(segmentsWithDotSegments), None, None)(uriConfigWithDisabledPathSegmentNormalization)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(segmentsWithDotSegments)
  }

  it should "should do nothing for a relative path reference" in {
    val uri = RelativePathReference(RootlessPath(segmentsWithDotSegments), None, None)
    uri.path.value shouldBe a[RootlessPath]
    uri.pathSegments should equal(segmentsWithDotSegments)
  }
}
