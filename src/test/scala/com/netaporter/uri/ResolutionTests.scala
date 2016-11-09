package com.netaporter.uri

/**
 * Reference resolution tests, base on RFC 3986 section 5.4.
 */
class ResolutionTests extends TestSpec {

  val baseUri = AbsoluteUri(Scheme("http"), Authority.option(registeredName = "a"), AbsolutePath.option(StringSegment("b"), StringSegment("c"), StringSegment("d;p")), Query.option(Parameter("q")))

  "The base URI" should "successfully parse" in {
    baseUri shouldBe a[SchemeWithAuthorityUri]
    baseUri.scheme.value.scheme should equal("http")
    baseUri.userInfo should equal(None)
    baseUri.hostString.value should equal("a")
    baseUri.port should equal(None)
    baseUri.path.value shouldBe an[AbsolutePath]
    baseUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("d;p")))
    baseUri.queryParameters.value should equal(Seq(Parameter("q", None)))
    baseUri.fragment should equal(None)
  }

  "Normal examples from RFC 3986 section 5.4.1" should "successfully resolve with a scheme and rootless path" in {
    val referenceUri = Uri("g:h")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithRootlessPathUri]
    targetUri.scheme.value.scheme should equal("g")
    targetUri.authority should equal(None)
    targetUri.path.value shouldBe a[RootlessPath]
    targetUri.pathSegments should equal(Seq(StringSegment("h")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("g:h")
  }

  it should "successfully resolve with a rootless path" in {
    val referenceUri = Uri("g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/g")
  }

  it should "successfully resolve with a rootless path starting with a \".\" segment" in {
    val referenceUri = Uri("./g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/g")
  }

  it should "successfully resolve with a rootless path ending with an empty segment" in {
    val referenceUri = Uri("g/")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g"), EmptySegment))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/g/")
  }

  it should "successfully resolve with an absolute path" in {
    val referenceUri = Uri("/g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/g")
  }

  it should "successfully resolve with a host" in {
    val referenceUri = Uri("//g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("g")
    targetUri.port should equal(None)
    targetUri.path should equal(None)
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://g")
  }

  it should "successfully resolve with a query" in {
    val referenceUri = Uri("?y")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("d;p")))
    targetUri.queryParameters.value should equal(Seq(Parameter("y", None)))
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/d;p?y")
  }

  it should "successfully resolve with a rootless path and query" in {
    val referenceUri = Uri("g?y")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g")))
    targetUri.queryParameters.value should equal(Seq(Parameter("y", None)))
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/g?y")
  }

  it should "successfully resolve with a fragment" in {
    val referenceUri = Uri("#s")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("d;p")))
    targetUri.queryParameters.value should equal(Seq(Parameter("q", None)))
    targetUri.fragment.value.fragment should equal("s")
    targetUri.toString should equal("http://a/b/c/d;p?q#s")
  }

  it should "successfully resolve with a rootless path and fragment" in {
    val referenceUri = Uri("g#s")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment.value.fragment should equal("s")
    targetUri.toString should equal("http://a/b/c/g#s")
  }

  it should "successfully resolve with a rootless path, query and fragment" in {
    val referenceUri = Uri("g?y#s")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g")))
    targetUri.queryParameters.value should equal(Seq(Parameter("y", None)))
    targetUri.fragment.value.fragment should equal("s")
    targetUri.toString should equal("http://a/b/c/g?y#s")
  }

  it should "successfully resolve with a rootless path starting with a ';'" in {
    val referenceUri = Uri(";x")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment(";x")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/;x")
  }

  it should "successfully resolve with a rootless path containing a ';'" in {
    val referenceUri = Uri("g;x")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g;x")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/g;x")
  }

  it should "successfully resolve with a rootless path containing a ';', query and fragment" in {
    val referenceUri = Uri("g;x?y#s")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g;x")))
    targetUri.queryParameters.value should equal(Seq(Parameter("y", None)))
    targetUri.fragment.value.fragment should equal("s")
    targetUri.toString should equal("http://a/b/c/g;x?y#s")
  }

  it should "successfully resolve with an empty reference" in {
    val referenceUri = Uri("")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("d;p")))
    targetUri.queryParameters.value should equal(Seq(Parameter("q", None)))
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/d;p?q")
  }

  it should "successfully resolve with a rootless path containing only a \".\" segment" in {
    val referenceUri = Uri(".")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), EmptySegment))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/")
  }

  it should "successfully resolve with a rootless path containing a \".\" segment and an empty segment" in {
    val referenceUri = Uri("./")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), EmptySegment))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/")
  }

  it should "successfully resolve with a rootless path containing only a \"..\" segment" in {
    val referenceUri = Uri("..")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), EmptySegment))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/")
  }

  it should "successfully resolve with a rootless path containing a \"..\" segment and an empty segment" in {
    val referenceUri = Uri("../")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), EmptySegment))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/")
  }

  it should "successfully resolve with a rootless path containing a \"..\" segment and another segment" in {
    val referenceUri = Uri("../g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/g")
  }

  it should "successfully resolve with a rootless path containing only 2 \"..\" segments" in {
    val referenceUri = Uri("../..")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(EmptySegment))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/")
  }

  it should "successfully resolve with a rootless path containing 2 \"..\" segments and an empty segment" in {
    val referenceUri = Uri("../../")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(EmptySegment))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/")
  }

  it should "successfully resolve with a rootless path containing 2 \"..\" segments and another segment" in {
    val referenceUri = Uri("../../g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/g")
  }

  "Abnormal examples from RFC 3986 section 5.4.2" should "successfully resolve with a rootless path containing 3 \"..\" segments and another segment" in {
    val referenceUri = Uri("../../../g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/g")
  }

  it should "successfully resolve with a rootless path containing 4 \"..\" segments and another segment" in {
    val referenceUri = Uri("../../../../g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/g")
  }

  it should "successfully resolve with an absolute path containing a \".\" segment and another segment" in {
    val referenceUri = Uri("/./g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/g")
  }

  it should "successfully resolve with an absolute path containing a \"..\" segment and another segment" in {
    val referenceUri = Uri("/../g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/g")
  }

  it should "successfully resolve with a rootless path ending with \".\"" in {
    val referenceUri = Uri("g.")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g.")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/g.")
  }

  it should "successfully resolve with a rootless path starting with \".\"" in {
    val referenceUri = Uri(".g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment(".g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/.g")
  }

  it should "successfully resolve with a rootless path ending with \"..\"" in {
    val referenceUri = Uri("g..")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g..")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/g..")
  }

  it should "successfully resolve with a rootless path starting with \"..\"" in {
    val referenceUri = Uri("..g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("..g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/..g")
  }

  it should "successfully resolve with a rootless path containing a \".\" segment, a \"..\" segment and another segment" in {
    val referenceUri = Uri("./../g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/g")
  }

  it should "successfully resolve with a rootless path containing a \".\" segment, another segment and a \".\" segment" in {
    val referenceUri = Uri("./g/.")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g"), EmptySegment))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/g/")
  }

  it should "successfully resolve with a rootless path containing a segment, a \".\" segment and another segment" in {
    val referenceUri = Uri("g/./h")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g"), StringSegment("h")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/g/h")
  }

  it should "successfully resolve with a rootless path containing a segment, a \"..\" segment and another segment" in {
    val referenceUri = Uri("g/../h")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("h")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/h")
  }

  it should "successfully resolve with a rootless path containing a matrix parameter segment, a \".\" segment and another segment" in {
    implicit val config = UriConfig(matrixParameterParsing = true)
    val referenceUri = Uri("g;x=1/./y")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), MatrixParametersSegment("g", Parameter("x", Option("1"))), StringSegment("y")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/g;x=1/y")
  }

  it should "successfully resolve with a rootless path containing a matrix parameter segment, a \"..\" segment and another segment" in {
    implicit val config = UriConfig(matrixParameterParsing = true)
    val referenceUri = Uri("g;x=1/../y")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("y")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/y")
  }

  it should "successfully resolve with a rootless path and query containing \".\"" in {
    val referenceUri = Uri("g?y/./x")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g")))
    targetUri.queryParameters.value should equal(Seq(Parameter("y/./x", None)))
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/g?y/./x")
  }

  it should "successfully resolve with a rootless path and query containing \"..\"" in {
    val referenceUri = Uri("g?y/../x")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g")))
    targetUri.queryParameters.value should equal(Seq(Parameter("y/../x", None)))
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/g?y/../x")
  }

  it should "successfully resolve with a rootless path and fragment containing \".\"" in {
    val referenceUri = Uri("g#s/./x")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment.value.fragment should equal("s/./x")
    targetUri.toString should equal("http://a/b/c/g#s/./x")
  }

  it should "successfully resolve with a rootless path and fragment containing \"..\"" in {
    val referenceUri = Uri("g#s/../x")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment.value.fragment should equal("s/../x")
    targetUri.toString should equal("http://a/b/c/g#s/../x")
  }

  it should "successfully resolve with a scheme and rootless path" in {
    val referenceUri = Uri("http:g")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithRootlessPathUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.authority should equal(None)
    targetUri.path.value shouldBe a[RootlessPath]
    targetUri.pathSegments should equal(Seq(StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http:g")
  }

  it should "successfully resolve with a scheme and rootless path, ignoring identical scheme" in {
    val referenceUri = Uri("http:g")
    val targetUri = baseUri.resolve(referenceUri, true)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("a")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("b"), StringSegment("c"), StringSegment("g")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://a/b/c/g")
  }

  "Other reference resolution tests" should "successfully resolve with a scheme and NO path" in {
    val referenceUri = Uri("scheme:#fragment")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithFragmentUri]
    targetUri.scheme.value.scheme should equal("scheme")
    targetUri.authority should equal(None)
    targetUri.path should equal(None)
    targetUri.query should equal(None)
    targetUri.fragment.value.fragment should equal("fragment")
    targetUri.toString should equal("scheme:#fragment")
  }

  it should "successfully resolve with an authority and path" in {
    val referenceUri = Uri("//host:234/path")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("http")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("host")
    targetUri.port.value should equal(234)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("path")))
    targetUri.query should equal(None)
    targetUri.fragment should equal(None)
    targetUri.toString should equal("http://host:234/path")
  }

  it should "successfully resolve with a rootless path, when the base URI has an authority but NO path" in {
    val baseUri = Uri("scheme://host").asInstanceOf[AbsoluteUri]
    val referenceUri = Uri("path?key=value")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithAuthorityUri]
    targetUri.scheme.value.scheme should equal("scheme")
    targetUri.userInfo should equal(None)
    targetUri.hostString.value should equal("host")
    targetUri.port should equal(None)
    targetUri.path.value shouldBe an[AbsolutePath]
    targetUri.pathSegments should equal(Seq(StringSegment("path")))
    targetUri.queryParameters.value should equal(Seq(Parameter("key", Option("value"))))
    targetUri.fragment should equal(None)
    targetUri.toString should equal("scheme://host/path?key=value")
  }

  it should "successfully resolve with a rootless path, when the base URI has NO authority and NO path" in {
    val baseUri = Uri("scheme:?key=value").asInstanceOf[AbsoluteUri]
    val referenceUri = Uri("path#fragment")
    val targetUri = baseUri.resolve(referenceUri)
    targetUri shouldBe a[SchemeWithRootlessPathUri]
    targetUri.scheme.value.scheme should equal("scheme")
    targetUri.authority should equal(None)
    targetUri.path.value shouldBe a[RootlessPath]
    targetUri.pathSegments should equal(Seq(StringSegment("path")))
    targetUri.query should equal(None)
    targetUri.fragment.value.fragment should equal("fragment")
    targetUri.toString should equal("scheme:path#fragment")
  }
}
