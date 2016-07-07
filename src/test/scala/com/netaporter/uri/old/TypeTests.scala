package com.netaporter.uri

import org.scalatest.{Matchers, FlatSpec}

class OldTypeTests extends FlatSpec with Matchers {

  import dsl.{ uriToUriDsl, stringToUri, stringToUriDsl, queryParamToUriDsl, uriToString }

  "String" should "render correctly" in {
    val uri = "/uris-in-scala.html" ? ("param" -> "hey")
    uri.toString should equal ("/uris-in-scala.html?param=hey")
  }

  "Booleans" should "render correctly" in {
    val uri = "/uris-in-scala.html" ? ("param" -> true)
    uri.toString should equal ("/uris-in-scala.html?param=true")
  }

  "Integers" should "render correctly" in {
    val uri = "/uris-in-scala.html" ? ("param" -> 1)
    uri.toString should equal ("/uris-in-scala.html?param=1")
  }

  "Floats" should "render correctly" in {
    val uri = "/uris-in-scala.html" ? ("param" -> 0.5f)
    uri.toString should equal ("/uris-in-scala.html?param=0.5")
  }

  "Options" should "render correctly" in {
    val uri = "/uris-in-scala.html" ? ("param" -> Some("some")) & ("param2" -> None)
    uri.toString should equal ("/uris-in-scala.html?param=some&param2") // TODO: Appended "&param2" due to functional changes.
  }
}
