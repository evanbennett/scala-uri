package com.netaporter.uri

import com.netaporter.uri.parsing.ParseUri

class ParsingConfigurationTests extends TestSpec {

  val uriStringSimple = "scheme://registeredName/path?queryKey#fragment"
  val uriStringNotDns = "//registered..name/?queryKey#fragment"
  val uriStringEmptyQuery = "/path/?"
  val uriStringFragmentWithHash = "path?queryKey#fragment#"
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryString.value should equal("queryKey")
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    uriNotDns shouldBe a[NetworkPathReference]
    uriNotDns.scheme should equal(None)
    uriNotDns.userInfo should equal(None)
    uriNotDns.hostString.value should equal("registered..name")
    uriNotDns.port should equal(None)
    uriNotDns.path.value shouldBe an[AbsolutePath]
    uriNotDns.pathSegments should equal(Seq(EmptySegment))
    uriNotDns.queryParameters.value should equal(Seq(Parameter("queryKey")))
    uriNotDns.fragment.value.fragment should equal("fragment")
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
    val uriEmptyQuery = Uri.option(uriStringEmptyQuery).value
    uriEmptyQuery shouldBe a[AbsolutePathReference]
    uriEmptyQuery.scheme should equal(None)
    uriEmptyQuery.authority should equal(None)
    uriEmptyQuery.path.value shouldBe an[AbsolutePath]
    uriEmptyQuery.pathSegments should equal(Seq(StringSegment("path"), EmptySegment))
    uriEmptyQuery.query.value should equal(Query())
    uriEmptyQuery.fragment should equal(None)
    val uriFragmentWithHash = Uri.option(uriStringFragmentWithHash).value
    uriFragmentWithHash shouldBe a[RelativePathReference]
    uriFragmentWithHash.scheme should equal(None)
    uriFragmentWithHash.authority should equal(None)
    uriFragmentWithHash.path.value shouldBe an[RootlessPath]
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
