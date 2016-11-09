package com.netaporter.uri

import scala.language.postfixOps
import com.netaporter.uri.dsl.{ uriToUriDsl => _, stringToUri => _, stringToUriDsl => _, queryParamToUriDsl => _, uriToString => _, _ }

class DslTests extends TestSpec {
  /*
   * Used infix operator precedence (by first character) listed in increasing order of precedence:
   *   =                               Only used in parameters which MUST be surrounded by parentheses.
   *   &                               Only used to separate query parameters. The only issue, is potentially `#` for a fragment.
   *   :                               Used: after a scheme; to separate host and port. (Password is not supported.)
   *   /                               Only used to separate path segments. The only issue, is potentially `#` for a fragment.
   *   @ ; ? `#` (all other specials)
   *
   * See: http://www.scala-lang.org/files/archive/spec/2.11/06-expressions.html#infix-operations
   */

  "The DSL" should "create a `Parameter`" in {
    val parameter = "queryKey" `=` "queryValue"
    parameter shouldBe a[Parameter]
    parameter.key should equal("queryKey")
    parameter.value.value should equal("queryValue")
  }

  it should "create a `StringSegment`" in {
    val pathSegment: StringSegment = "pathSegment"
    pathSegment shouldBe a[StringSegment]
    pathSegment.segment should equal("pathSegment")
  }

  it should "create a `MatrixParametersSegment` with 1 matrix parameter" in {
    val pathSegment = "pathSegment" `;` ("matrixKey" `=` "matrixValue")
    pathSegment shouldBe a[MatrixParametersSegment]
    pathSegment.segment should equal("pathSegment")
    pathSegment.parameters should equal(Seq(Parameter("matrixKey", Some("matrixValue"))))
  }

  it should "create a `MatrixParametersSegment` with multiple matrix parameters" in {
    val pathSegment = "pathSegment" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "") `;` ("matrixKey3" `=` "matrixValue3")
    pathSegment shouldBe a[MatrixParametersSegment]
    pathSegment.segment should equal("pathSegment")
    pathSegment.parameters should equal(Seq(Parameter("matrixKey1", Some("matrixValue1")), Parameter("matrixKey2", Some("")), Parameter("matrixKey3", Some("matrixValue3"))))
  }

  it should "create a `MatrixParametersSegment` with multiple matrix parameters with key only parameter first" in {
    val pathSegment = "pathSegment" `;` "matrixKey1" `;` ("matrixKey2" `=` "matrixValue2") `;` ("matrixKey3" `=` "matrixValue3")
    pathSegment shouldBe a[MatrixParametersSegment]
    pathSegment.segment should equal("pathSegment")
    pathSegment.parameters should equal(Seq(Parameter("matrixKey1"), Parameter("matrixKey2", Some("matrixValue2")), Parameter("matrixKey3", Some("matrixValue3"))))
  }

  it should "create a `MatrixParametersSegment` with multiple matrix parameters with key only parameter last" in {
    val pathSegment = "pathSegment" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2") `;` "matrixKey3"
    pathSegment shouldBe a[MatrixParametersSegment]
    pathSegment.segment should equal("pathSegment")
    pathSegment.parameters should equal(Seq(Parameter("matrixKey1", Some("matrixValue1")), Parameter("matrixKey2", Some("matrixValue2")), Parameter("matrixKey3")))
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  it should "create a `Uri` with scheme, userInfo, registeredName, port, multiple segements, multiple query parameters and fragment" in {
    val uri: Uri = "scheme".`://`("userInfo" `@` "registeredName" `:` 8080) / "pathSegment1" / "pathSegment2" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2") ? ("queryKey1" `=` "queryValue1") & ("queryKey2" `=` "queryValue2") `#` "fragment"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.userInfoString.value should equal("userInfo")
    uri.password should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port.value should equal(8080)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment1"), MatrixParametersSegment("pathSegment2", Seq(Parameter("matrixKey1", Some("matrixValue1")), Parameter("matrixKey2", Some("matrixValue2"))))))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey1", Some("queryValue1")), Parameter("queryKey2", Some("queryValue2"))))
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme, registeredName, multiple segements, no query and fragment" in {
    val uri: Uri = "scheme".`://`("registeredName") / "pathSegment1" / "pathSegment2" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2") `#` "fragment"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment1"), MatrixParametersSegment("pathSegment2", Seq(Parameter("matrixKey1", Some("matrixValue1")), Parameter("matrixKey2", Some("matrixValue2"))))))
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme, registeredName, a single string segement, a query parameter and fragment" in {
    val uri: Uri = "scheme".`://`("registeredName") / "pathSegment" ? ("queryKey" `=` "queryValue") `#` "fragment"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme, registeredName and empty path" in {
    val uri: Uri = "scheme".`://`("registeredName") /

    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, registeredName and emptyfragment" in {
    val uri: Uri = "scheme".`://`("registeredName") `#`

    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with scheme, registeredName and fragment" in {
    val uri: Uri = ("scheme" `://` "registeredName") `#` "fragment"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  it should "create a `Uri` with scheme only" in {
    val uri: Uri = "scheme" `:`

    uri shouldBe a[SchemeUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme and empty authority" in {
    val uri: Uri = "scheme" `://`

    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme and registeredName" in {
    val uri: Uri = "scheme" `://` "registeredName"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, userInfo and registeredName" in {
    val uri: Uri = "scheme" `://` "userInfo" `@` "registeredName"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.userInfoString.value should equal("userInfo")
    uri.password should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, registeredName and port" in {
    val uri: Uri = "scheme".`://`("registeredName" `:` 8080)
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port.value should equal(8080)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty authority and empty path" in {
    val uri: Uri = "scheme" `:///`

    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty authority and single path segment" in {
    val uri: Uri = "scheme" `:///` "pathSegment"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty authority, single path segment and query" in {
    val uri: Uri = "scheme".`:///`("pathSegment") ? ("queryKey" `=` "queryValue")
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty authority, single path segment, query and fragment" in {
    val uri: Uri = "scheme".`:///`("pathSegment") ? ("queryKey" `=` "queryValue") `#` "fragment"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme, empty authority, single path segment and fragment" in {
    val uri: Uri = "scheme".`:///`("pathSegment") `#` "fragment"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme, empty authority, empty path and empty query" in {
    val uri: Uri = "scheme" `:///?`

    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query.value should equal(EmptyQuery)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty authority, empty path and query" in {
    val uri: Uri = "scheme" `:///?` ("queryKey" `=` "queryValue")
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty authority, empty path and single query key (without a value)" in {
    val uri: Uri = "scheme" `:///?` "query"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.queryString.value should equal("query")
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty authority, empty path, empty query and empty fragment" in {
    val uri: Uri = "scheme" `:///?#`

    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with scheme, empty authority, empty path, empty query and fragment" in {
    val uri: Uri = "scheme" `:///?#` "fragment"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query.value should equal(EmptyQuery)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme, empty authority, empty path and empty fragment" in {
    val uri: Uri = "scheme" `:///#`

    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with scheme, empty authority, empty path and fragment" in {
    val uri: Uri = "scheme" `:///#` "fragment"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme, empty authority and empty query" in {
    val uri: Uri = "scheme" `://?`

    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty authority and query" in {
    val uri: Uri = "scheme" `://?` ("queryKey" `=` "queryValue")
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty authority and single query key (without a value)" in {
    val uri: Uri = "scheme" `://?` "query"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path should equal(None)
    uri.queryString.value should equal("query")
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty authority, empty query and empty fragment" in {
    val uri: Uri = "scheme" `://?#`

    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with scheme, empty authority, empty query and fragment" in {
    val uri: Uri = "scheme" `://?#` "fragment"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme, empty authority and empty fragment" in {
    val uri: Uri = "scheme" `://#`

    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with scheme, empty authority and fragment" in {
    val uri: Uri = "scheme" `://#` "fragment"
    uri shouldBe a[SchemeWithAuthorityUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority.value should equal(EmptyAuthority)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme and empty path" in {
    val uri: Uri = "scheme" :/

    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme and single path segment" in {
    val uri: Uri = "scheme" :/ "pathSegment"
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, single path segment and query" in {
    val uri: Uri = "scheme".:/("pathSegment") ? ("queryKey" `=` "queryValue")
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, single path segment, query and fragment" in {
    val uri: Uri = "scheme".:/("pathSegment") ? ("queryKey" `=` "queryValue") `#` "fragment"
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme, single path segment and fragment" in {
    val uri: Uri = "scheme".:/("pathSegment") `#` "fragment"
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme, empty path and empty query" in {
    val uri: Uri = "scheme" :/?

    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query.value should equal(EmptyQuery)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty path and query" in {
    val uri: Uri = "scheme" :/? ("queryKey" `=` "queryValue")
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty path and single query key (without a value)" in {
    val uri: Uri = "scheme" :/? "query"
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.queryString.value should equal("query")
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty path, empty query and empty fragment" in {
    val uri: Uri = "scheme" :/?#

    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with scheme, empty path, empty query and fragment" in {
    val uri: Uri = "scheme" :/?# "fragment"
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query.value should equal(EmptyQuery)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme, empty path and empty fragment" in {
    val uri: Uri = "scheme" :/#

    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with scheme, empty path and fragment" in {
    val uri: Uri = "scheme" :/# "fragment"
    uri shouldBe a[SchemeWithAbsolutePathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme and rootless single path segment" in {
    val uri: Uri = "scheme".`:`("pathSegment")
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme and rootless single matrix parameter segment" in {
    val uri: Uri = "scheme".`:`("pathSegment" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2"))
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq(MatrixParametersSegment("pathSegment", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2", "matrixValue2")))))
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, rootless single path segment and query" in {
    val uri: Uri = "scheme".`:`("pathSegment") ? ("queryKey" `=` "queryValue")
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, rootless single path segment, query and fragment" in {
    val uri: Uri = "scheme".`:`("pathSegment") ? ("queryKey" `=` "queryValue") `#` "fragment"
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme, rootless single path segment and fragment" in {
    val uri: Uri = "scheme".`:`("pathSegment") `#` "fragment"
    uri shouldBe a[SchemeWithRootlessPathUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme and empty query" in {
    val uri: Uri = "scheme" :?

    uri shouldBe a[SchemeWithQueryUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme and query" in {
    val uri: Uri = "scheme" :? ("queryKey" `=` "queryValue")
    uri shouldBe a[SchemeWithQueryUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme and single query key (without a value)" in {
    val uri: Uri = "scheme" :? "query"
    uri shouldBe a[SchemeWithQueryUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryString.value should equal("query")
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with scheme, empty query and empty fragment" in {
    val uri: Uri = "scheme" :?#

    uri shouldBe a[SchemeWithQueryUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with scheme, empty query and fragment" in {
    val uri: Uri = "scheme" :?# "fragment"
    uri shouldBe a[SchemeWithQueryUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with scheme and empty fragment" in {
    val uri: Uri = "scheme" :#

    uri shouldBe a[SchemeWithFragmentUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with scheme and fragment" in {
    val uri: Uri = "scheme" :# "fragment"
    uri shouldBe a[SchemeWithFragmentUri]
    uri.scheme.value.scheme should equal("scheme")
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  it should "create a `Uri` with an empty authority only" in {
    val uri: Uri = `//`

    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.authority.value should equal(EmptyAuthority)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with registeredName only" in {
    val uri: Uri = `//`("registeredName")
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with ipv4Address only" in {
    val uri: Uri = `//`("192.168.205.1")
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipv4Address.value should equal("192.168.205.1")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with ipLiteral (as IPv6 address) only" in {
    val uri: Uri = `//`("[e98f:2d9::384]")
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[e98f:2d9::384]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with ipLiteral (as IPvFuture address) only" in {
    val uri: Uri = `//`("[v9.something]")
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.ipLiteral.value should equal("[v9.something]")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with registeredName and empty path" in {
    val uri: Uri = `//`("registeredName") /

    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with registeredName and single path segment" in {
    val uri: Uri = `//`("registeredName") / "pathSegment"
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with registeredName, single path segment and query" in {
    val uri: Uri = `//`("registeredName") / "pathSegment" ? ("queryKey" `=` "queryValue")
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with registeredName, single path segment, query and fragment" in {
    val uri: Uri = `//`("registeredName") / "pathSegment" ? ("queryKey" `=` "queryValue") `#` "fragment"
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with registeredName, single path segment and fragment" in {
    val uri: Uri = `//`("registeredName") / "pathSegment" `#` "fragment"
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with registeredName, empty path and empty query" in {
    val uri: Uri = `//`("registeredName") /?

    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query.value should equal(EmptyQuery)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with registeredName, empty path and single query parameter" in {
    val uri: Uri = `//`("registeredName") /? ("queryKey" `=` "queryValue")
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with registeredName, empty path and single query key (without a value)" in {
    val uri: Uri = `//`("registeredName") /? "query"
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.queryString.value should equal("query")
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with registeredName, empty path, empty query and empty fragment" in {
    val uri: Uri = `//`("registeredName") /?#

    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with registeredName, empty path, empty query and fragment" in {
    val uri: Uri = `//`("registeredName") /?# "fragment"
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query.value should equal(EmptyQuery)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with registeredName, empty path and empty fragment" in {
    val uri: Uri = `//`("registeredName") /#

    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with registeredName, empty path and fragment" in {
    val uri: Uri = `//`("registeredName") /# "fragment"
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with registeredName and empty query" in {
    val uri: Uri = `//`("registeredName") ?

    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with registeredName and single query parameter" in {
    val uri: Uri = `//`("registeredName") ? ("queryKey" `=` "queryValue")
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with registeredName and single query key (without a value)" in {
    val uri: Uri = `//`("registeredName") ? "query"
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.queryString.value should equal("query")
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with registeredName, empty query and empty fragment" in {
    val uri: Uri = `//`("registeredName") ?#

    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with registeredName, empty query and fragment" in {
    val uri: Uri = `//`("registeredName") ?# "fragment"
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with registeredName and empty fragment" in {
    val uri: Uri = `//`("registeredName") `#`

    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with registeredName and fragment" in {
    val uri: Uri = `//`("registeredName") `#` "fragment"
    uri shouldBe a[NetworkPathReference]
    uri.scheme should equal(None)
    uri.userInfo should equal(None)
    uri.authority.value.host.value.registeredName.value should equal("registeredName")
    uri.port should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  it should "create a `Uri` with an empty absolute path only" in {
    val uri: Uri = /

    uri shouldBe a[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value should equal(EmptyAbsolutePath)
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with a single string segment only" in {
    val uri: Uri = /("pathSegment")
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment")))
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with a string segment ending with '/'" in {
    val uri: Uri = /("pathSegment") /

    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment"), EmptySegment))
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with multiple string segments" in {
    val uri: Uri = /("pathSegment1") / "pathSegment2" / "pathSegment3"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment1"), StringSegment("pathSegment2"), StringSegment("pathSegment3")))
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with multiple matrix parameter segments with key only parameter second followed by a string segment" in {
    val uri: Uri = /("pathSegment1" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2")) / "pathSegment2" `;` ("matrixKey1" `=` "matrixValue1") `;` "matrixKey2" / "pathSegment3"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("pathSegment1", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2", "matrixValue2"))), MatrixParametersSegment("pathSegment2", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2"))), StringSegment("pathSegment3")))
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with multiple matrix parameter segments with key only parameter second followed by another matrix parameter segment" in {
    val uri: Uri = /("pathSegment1" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2")) / "pathSegment2" `;` ("matrixKey1" `=` "matrixValue1") `;` "matrixKey2" / "pathSegment3" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2")
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("pathSegment1", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2", "matrixValue2"))), MatrixParametersSegment("pathSegment2", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2"))), MatrixParametersSegment("pathSegment3", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2", "matrixValue2")))))
    uri.query should equal(None)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with multiple string segments and query with key only" in {
    val uri: Uri = /("pathSegment1") / "pathSegment2" / "pathSegment3" ? "query"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment1"), StringSegment("pathSegment2"), StringSegment("pathSegment3")))
    uri.queryString.value should equal("query")
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with multiple matrix parameter segments with key only parameter last followed by empty query" in {
    val uri: Uri = /("pathSegment1" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2")) / "pathSegment2" `;` ("matrixKey1" `=` "matrixValue1") `;` "matrixKey2" ?

    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("pathSegment1", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2", "matrixValue2"))), MatrixParametersSegment("pathSegment2", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2")))))
    uri.query.value should equal(EmptyQuery)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with multiple matrix parameter segments with key only parameter last followed by query" in {
    val uri: Uri = /("pathSegment1" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2")) / "pathSegment2" `;` ("matrixKey1" `=` "matrixValue1") `;` "matrixKey2" ? ("queryKey" `=` "queryValue")
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("pathSegment1", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2", "matrixValue2"))), MatrixParametersSegment("pathSegment2", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2")))))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with multiple matrix parameter segments with key only parameter last followed by query with key only" in {
    val uri: Uri = /("pathSegment1" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2")) / "pathSegment2" `;` ("matrixKey1" `=` "matrixValue1") `;` "matrixKey2" ? "query"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("pathSegment1", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2", "matrixValue2"))), MatrixParametersSegment("pathSegment2", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2")))))
    uri.queryString.value should equal("query")
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with multiple matrix parameter segments with key only parameter last followed by empty query and empty fragment" in {
    val uri: Uri = /("pathSegment1" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2")) / "pathSegment2" `;` ("matrixKey1" `=` "matrixValue1") `;` "matrixKey2" ?#

    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("pathSegment1", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2", "matrixValue2"))), MatrixParametersSegment("pathSegment2", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2")))))
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with absolute path with multiple matrix parameter segments with key only parameter last followed by empty query and a fragment" in {
    val uri: Uri = /("pathSegment1" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2")) / "pathSegment2" `;` ("matrixKey1" `=` "matrixValue1") `;` "matrixKey2" ?# "fragment"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("pathSegment1", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2", "matrixValue2"))), MatrixParametersSegment("pathSegment2", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2")))))
    uri.query.value should equal(EmptyQuery)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with absolute path with multiple matrix parameter segments with key only parameter first followed by query and empty fragment" in {
    val uri: Uri = /("pathSegment1" `;` "matrixKey1" `;` ("matrixKey2" `=` "matrixValue2")) / "pathSegment2" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2") ? ("queryKey" `=` "queryValue") `#`

    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("pathSegment1", Seq(Parameter("matrixKey1"), Parameter("matrixKey2", "matrixValue2"))), MatrixParametersSegment("pathSegment2", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2", "matrixValue2")))))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with absolute path with multiple matrix parameter segments with key only parameter last followed by query and fragment" in {
    val uri: Uri = /("pathSegment1" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2")) / "pathSegment2" `;` ("matrixKey1" `=` "matrixValue1") `;` "matrixKey2" ? ("queryKey" `=` "queryValue") `#` "fragment"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("pathSegment1", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2", "matrixValue2"))), MatrixParametersSegment("pathSegment2", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2")))))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with absolute path with multiple matrix parameter segments with key only parameter first followed by empty fragment" in {
    val uri: Uri = /("pathSegment1" `;` ("matrixKey1" `=` "matrixValue1") `;` "matrixKey2") / "pathSegment2" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2") `#`

    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("pathSegment1", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2"))), MatrixParametersSegment("pathSegment2", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2", "matrixValue2")))))
    uri.query should equal(None)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with absolute path with multiple matrix parameter segments with key only parameter last followed by fragment" in {
    val uri: Uri = /("pathSegment1" `;` ("matrixKey1" `=` "matrixValue1") `;` ("matrixKey2" `=` "matrixValue2")) / "pathSegment2" `;` ("matrixKey1" `=` "matrixValue1") `;` "matrixKey2" `#` "fragment"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(MatrixParametersSegment("pathSegment1", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2", "matrixValue2"))), MatrixParametersSegment("pathSegment2", Seq(Parameter("matrixKey1", "matrixValue1"), Parameter("matrixKey2")))))
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with absolute path with a string segment ending with '/', and empty query" in {
    val uri: Uri = /("pathSegment") /?

    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment"), EmptySegment))
    uri.query.value should equal(EmptyQuery)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with a string segment ending with '/', and query" in {
    val uri: Uri = /("pathSegment") /? ("queryKey" `=` "queryValue")
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment"), EmptySegment))
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with a string segment ending with '/', and query with key only" in {
    val uri: Uri = /("pathSegment") /? "query"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment"), EmptySegment))
    uri.queryString.value should equal("query")
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with a string segment ending with '/', empty query and empty fragment" in {
    val uri: Uri = /("pathSegment") /?#

    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment"), EmptySegment))
    uri.query.value should equal(EmptyQuery)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with absolute path with a string segment ending with '/', empty query and fragment" in {
    val uri: Uri = /("pathSegment") /?# "fragment"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment"), EmptySegment))
    uri.query.value should equal(EmptyQuery)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with absolute path with a string segment ending with '/', and empty fragment" in {
    val uri: Uri = /("pathSegment") /#

    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment"), EmptySegment))
    uri.query should equal(None)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with absolute path with a string segment ending with '/', and fragment" in {
    val uri: Uri = /("pathSegment") /# "fragment"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment"), EmptySegment))
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with absolute path with multiple string segments and query with key only with parentheses surrounding the path" in {
    val uri: Uri = (/("pathSegment1") / "pathSegment2" / "pathSegment3") ? "query"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment1"), StringSegment("pathSegment2"), StringSegment("pathSegment3")))
    uri.queryString.value should equal("query")
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with absolute path with multiple string segments, empty query and fragment" in {
    val uri: Uri = /("pathSegment1") / "pathSegment2" / "pathSegment3" `?#` "fragment"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment1"), StringSegment("pathSegment2"), StringSegment("pathSegment3")))
    uri.query.value should equal(EmptyQuery)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with absolute path with multiple string segments, empty query and fragment with parentheses surrounding the path" in {
    val uri: Uri = (/("pathSegment1") / "pathSegment2" / "pathSegment3") `?#` "fragment"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment1"), StringSegment("pathSegment2"), StringSegment("pathSegment3")))
    uri.query.value should equal(EmptyQuery)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with absolute path with multiple string segments and fragment" in {
    val uri: Uri = /("pathSegment1") / "pathSegment2" / "pathSegment3" `#` "fragment"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment1"), StringSegment("pathSegment2"), StringSegment("pathSegment3")))
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with absolute path with multiple string segments and fragment with parentheses surrounding the path" in {
    val uri: Uri = (/("pathSegment1") / "pathSegment2" / "pathSegment3") `#` "fragment"
    uri shouldBe an[AbsolutePathReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path.value shouldBe an[AbsolutePath]
    uri.pathSegments should equal(Seq(StringSegment("pathSegment1"), StringSegment("pathSegment2"), StringSegment("pathSegment3")))
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  it should "create a `Uri` with an empty query only" in {
    val uri: Uri = ?

    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query.value should equal(EmptyQuery)
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with single query parameter only" in {
    val uri: Uri = ?("queryKey" `=` "queryValue")
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with single query parameter and fragment" in {
    val uri: Uri = ?("queryKey" `=` "queryValue") `#` "fragment"
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey", "queryValue")))
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with multiple query parameters" in {
    val uri: Uri = ?("queryKey1" `=` "queryValue1") & ("queryKey2" `=` "queryValue2") & ("queryKey3" `=` "queryValue3")
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey1", Some("queryValue1")), Parameter("queryKey2", Some("queryValue2")), Parameter("queryKey3", Some("queryValue3"))))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with multiple query parameters with key only parameter first" in {
    val uri: Uri = ?("queryKey1") & ("queryKey2" `=` "queryValue2") & ("queryKey3" `=` "queryValue3")
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey1"), Parameter("queryKey2", Some("queryValue2")), Parameter("queryKey3", Some("queryValue3"))))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with multiple query parameters with key only parameter last" in {
    val uri: Uri = ?("queryKey1" `=` "queryValue1") & ("queryKey2" `=` "queryValue2") & "queryKey3"
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey1", Some("queryValue1")), Parameter("queryKey2", Some("queryValue2")), Parameter("queryKey3")))
    uri.fragment should equal(None)
  }

  it should "create a `Uri` with multiple query parameters and fragment" in {
    val uri: Uri = ?("queryKey1" `=` "queryValue1") & ("queryKey2" `=` "queryValue2") & ("queryKey3" `=` "queryValue3") `#` "fragment"
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey1", Some("queryValue1")), Parameter("queryKey2", Some("queryValue2")), Parameter("queryKey3", Some("queryValue3"))))
    uri.fragmentString.value should equal("fragment")
  }

  it should "create a `Uri` with multiple query parameters and empty fragment" in {
    val uri: Uri = ?("queryKey1" `=` "queryValue1") & ("queryKey2" `=` "queryValue2") & ("queryKey3" `=` "queryValue3") `#`

    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey1", Some("queryValue1")), Parameter("queryKey2", Some("queryValue2")), Parameter("queryKey3", Some("queryValue3"))))
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with multiple query parameters with key only parameter last and fragment" in {
    val uri: Uri = ?("queryKey1" `=` "queryValue1") & ("queryKey2" `=` "queryValue2") & "queryKey3" `#` "fragment"
    uri shouldBe a[QueryReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.queryParameters.value should equal(Seq(Parameter("queryKey1", Some("queryValue1")), Parameter("queryKey2", Some("queryValue2")), Parameter("queryKey3")))
    uri.fragmentString.value should equal("fragment")
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  it should "create a `Uri` with an empty fragment only" in {
    val uri: Uri = `#`

    uri shouldBe a[FragmentReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragment.value should equal(EmptyFragment)
  }

  it should "create a `Uri` with fragment only" in {
    val uri: Uri = `#`("fragment")
    uri shouldBe a[FragmentReference]
    uri.scheme should equal(None)
    uri.authority should equal(None)
    uri.path should equal(None)
    uri.query should equal(None)
    uri.fragmentString.value should equal("fragment")
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  it should "convert a SchemeDsl `toString`" in {
    val dsl: SchemeDsl = "scheme"
    dsl.toString should equal("scheme:")
  }

  it should "convert an AuthorityDsl `toString`" in {
    val dsl: AuthorityDsl = "scheme".`://`("registeredName")
    dsl.toString should equal("scheme://registeredname")
  }

  it should "convert a PathDsl `toString`" in {
    val dsl: PathDsl = "scheme".`://`("registeredName") / "path"
    dsl.toString should equal("scheme://registeredname/path")
  }

  it should "convert a QueryDsl `toString`" in {
    val dsl: QueryDsl = "scheme".`://`("registeredName") / "path" `?` "queryKey"
    dsl.toString should equal("scheme://registeredname/path?queryKey")
  }

  it should "convert a FragmentDsl `toString`" in {
    val dsl: FragmentDsl = "scheme".`://`("registeredName") / "path" `?` "queryKey" `#` "fragment"
    dsl.toString should equal("scheme://registeredname/path?queryKey#fragment")
  }
}
