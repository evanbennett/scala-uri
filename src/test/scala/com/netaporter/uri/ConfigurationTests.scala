package com.netaporter.uri

class ConfigurationTests extends TestSpec {

  "`UriConfig`" should "be equal" in {
    UriConfig.DEFAULT.equals(UriConfig()) should equal(true)
  }

  it should "not be equal" in {
    UriConfig.DEFAULT.equals(UriConfig.RFC3986) should equal(false)
  }

  it should "not equal `null" in {
    UriConfig.DEFAULT.equals(null) should equal(false)
  }

  it should "not equal a `String`" in {
    UriConfig.DEFAULT.equals("string") should equal(false)
  }

  "`UriConfig.hashCode`" should "succeed" in {
    UriConfig.DEFAULT.hashCode should equal(945288978)
  }

  "`UriConfig.toString`" should "succeed" in {
    UriConfig.DEFAULT.toString should startWith("UriConfig(")
  }

  "`UriConfig.apply`" should "succeed with no arguments" in {
    UriConfig() should not equal(null)
  }

  it should "fail with `userInfoDecoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(userInfoDecoder = null)
    }
  }

  it should "fail with `userDecoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(userDecoder = null)
    }
  }

  it should "fail with `passwordDecoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(passwordDecoder = null)
    }
  }

  it should "fail with `registeredNameDecoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(registeredNameDecoder = null)
    }
  }

  it should "fail with `pathDecoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(pathDecoder = null)
    }
  }

  it should "fail with `queryDecoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(queryDecoder = null)
    }
  }

  it should "fail with `fragmentDecoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(fragmentDecoder = null)
    }
  }

  it should "fail with `userInfoEncoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(userInfoEncoder = null)
    }
  }

  it should "fail with `userEncoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(userEncoder = null)
    }
  }

  it should "fail with `passwordEncoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(passwordEncoder = null)
    }
  }

  it should "fail with `registeredNameEncoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(registeredNameEncoder = null)
    }
  }

  it should "fail with `pathEncoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(pathEncoder = null)
    }
  }

  it should "fail with `queryEncoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(queryEncoder = null)
    }
  }

  it should "fail with `fragmentEncoder` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(fragmentEncoder = null)
    }
  }

  it should "fail with `charset` `null`" in {
    a [IllegalArgumentException] should be thrownBy {
      UriConfig(charset = null: java.nio.charset.Charset) // TODO: Once the deprecations are removed, `: java.nio.charset.Charset` can be removed.
    }
  }
}
