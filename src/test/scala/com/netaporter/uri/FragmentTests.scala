package com.netaporter.uri

class FragmentTests extends TestSpec {

  "`Uri.withFragment`" should "change the fragment when provided a `Uri`" in {
    val uri = Uri(None, None, None, None, Fragment.option("fragment"))
    uri.fragmentString.value should equal("fragment")
    val uri2 = Uri(None, None, None, None, Fragment.option("fragment2"))
    uri.withFragment(uri2).fragmentString.value should equal("fragment2")
  }

  it should "change the fragment when provided a `Fragment`" in {
    val uri = Uri(None, None, None, None, Fragment.option("fragment"))
    uri.fragmentString.value should equal("fragment")
    uri.withFragment(Fragment("fragment2")).fragmentString.value should equal("fragment2")
  }

  it should "change the fragment when provided a `String`" in {
    val uri = Uri(None, None, None, None, Fragment.option("fragment"))
    uri.fragmentString.value should equal("fragment")
    uri.withFragment("fragment2").fragmentString.value should equal("fragment2")
  }

  it should "remove the fragment when provided nothing" in {
    val uri = Uri(None, None, None, None, Fragment.option("fragment"))
    uri.fragmentString.value should equal("fragment")
    uri.withFragment().fragment should equal(None)
  }

  "`Uri.fragmentToString` and therefore `Fragment.toString`" should "work with a fragment" in {
    val uri = Uri(None, None, None, None, Fragment.option("fragment"))
    uri.fragmentToString should equal("#fragment")
  }

  it should "work without a fragment" in {
    EmptyReference.fragmentToString should equal("")
  }

  "`Fragment.copy`" should "succeed" in {
    Fragment("http").copy("https").fragment should equal("https")
  }

  it should "succeed when passed an empty string" in {
    Fragment("http").copy("") should equal(EmptyFragment)
  }

  it should "fail when passed `null`" in {
    an [IllegalArgumentException] should be thrownBy {
      Fragment("http").copy(null)
    }
  }

  "`Fragment.apply`" should "succeed" in {
    Fragment("https").fragment should equal("https")
  }

  it should "succeed when passed an empty string" in {
    Fragment("") should equal(EmptyFragment)
  }

  it should "fail when passed `null`" in {
    an [IllegalArgumentException] should be thrownBy {
      Fragment(null)
    }
  }

  "`Fragment.option`" should "return Some" in {
    Fragment.option("http").value.fragment should equal("http")
  }

  it should "return Some(EmptyFragment) when passed an empty string" in {
    Fragment.option("").value should equal(EmptyFragment)
  }

  it should "return None when passed `null`" in {
    Fragment.option(null) should equal(None)
  }
}
