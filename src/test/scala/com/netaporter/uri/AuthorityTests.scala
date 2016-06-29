package com.netaporter.uri

import com.netaporter.uri.config.UriConfig

class AuthorityTests extends TestSpec {

  "`Uri.userInfo`" should "return the user info" in {
    val uri = Uri(None, Authority.option(UserInfo.option("user", Some("password")), "www.example.com", Some(8080)), None, None, None)
    uri.userInfo.value should equal(UserInfo("user", Some("password")))
  }

  it should "return `None`" in {
    EmptyUri.userInfo should equal(None)
  }

  "`Uri.user` and therefore `Authority.user`" should "return the user" in {
    val uri = Uri(None, Authority.option(UserInfo.option("user", Some("password")), "www.example.com", Some(8080)), None, None, None)
    uri.user.value should equal("user")
  }

  it should "return `None`" in {
    EmptyUri.user should equal(None)
  }

  "`Uri.password` and therefore `Authority.password`" should "return the password" in {
    val uri = Uri(None, Authority.option(UserInfo.option("user", Some("password")), "www.example.com", Some(8080)), None, None, None)
    uri.password.value should equal("password")
  }

  it should "return `None`" in {
    EmptyUri.password should equal(None)
  }

  "`Uri.host`" should "return the host" in {
    val uri = Uri(None, Authority.option(UserInfo.option("user", Some("password")), "www.example.com", Some(8080)), None, None, None)
    uri.host.value should equal("www.example.com")
  }

  it should "return `None`" in {
    EmptyUri.host should equal(None)
  }

  "`Uri.hostParts`" should "return the dot separated host" in {
    val uri = Uri(None, Authority.option(None, "theon.github.com", None), None, None, None)
    uri.hostParts should equal(Seq("theon", "github", "com"))
  }

  it should "return `Seq.empty` when the host is empty" in {
    EmptyUri.hostParts should equal(Seq.empty)
  }

  "`Uri.subdomain`" should "return the first dot separated part of the host" in {
    val uri = Uri(None, Authority.option(None, "theon.github.com", None), None, None, None)
    uri.subdomain.value should equal("theon")
  }

  it should "return None when the host is empty" in {
    EmptyUri.subdomain should equal(None)
  }

  "`Uri.publicSuffix`" should "match the longest public suffix" in {
    val uri = Uri(None, Authority.option(None, "www.google.co.uk", None), None, None, None)
    uri.publicSuffix.value should equal("co.uk")
  }

  it should "return None when the host is empty" in {
    EmptyUri.publicSuffix should equal(None)
  }

  it should "only return public suffixes that match full dot separated host parts" in {
    val uri = Uri(None, Authority.option(None, "www.bar.com", None), None, None, None)
    // Github issue #110: Should not match ar.com
    uri.publicSuffix.value should equal("com")
  }

  "`Uri.publicSuffixes`" should "match all public suffixes" in {
    val uri = Uri(None, Authority.option(None, "www.google.co.uk", None), None, None, None)
    uri.publicSuffixes should equal(Seq("co.uk", "uk"))
  }

  it should "return `Seq.empty` when the host is empty" in {
    EmptyUri.publicSuffixes should equal(Seq.empty)
  }

  "`Uri.port`" should "return the port" in {
    val uri = Uri(None, Authority.option(UserInfo.option("user", Some("password")), "www.example.com", Some(8080)), None, None, None)
    uri.port.value should equal(8080)
  }

  it should "return `None`" in {
    EmptyUri.port should equal(None)
  }

  "`Uri.withAuthority`" should "change the authority when provided a `Uri`" in {
    val authority = Authority.option(host = "www.example.com")
    val uri = Uri(None, authority, None, None, None)
    EmptyUri.withAuthority(uri).authority should equal(authority)
  }

  it should "change the authority when provided an `Authority`" in {
    val authority = Authority.option(host = "www.example.com")
    val uri = Uri(None, authority, None, None, None)
    uri.authority should equal(authority)
    val authority2 = Authority(host = "www.example2.com")
    uri.withAuthority(authority2).authority.value should equal(authority2)
  }

  it should "remove the authority when provided nothing" in {
    val authority = Authority.option(host = "www.example.com")
    val uri = Uri(None, authority, None, None, None)
    uri.authority should equal(authority)
    uri.withAuthority().authority should equal(None)
  }

  "`Uri.withUserInfo`" should "change the userInfo when provided a `Uri`" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.userInfo should equal(None)
    val userInfo = UserInfo.option("user")
    val uri2 = Uri(None, Authority.option(userInfo, "www.example.com", None), None, None, None)
    uri.withUserInfo(uri2).userInfo should equal(userInfo)
  }

  it should "change the userInfo when provided an `Authority`" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.userInfo should equal(None)
    val userInfo = UserInfo.option("user")
    uri.withUserInfo(Authority(userInfo, "www.example.com", None)).userInfo should equal(userInfo)
  }

  it should "change the userInfo when provided an `UserInfo`" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.userInfo should equal(None)
    val userInfo = UserInfo("user")
    uri.withUserInfo(userInfo).userInfo.value should equal(userInfo)
  }

  it should "remove the userInfo when provided nothing" in {
    val userInfo = UserInfo.option("user")
    val uri = Uri(None, Authority.option(userInfo, "www.example.com", None), None, None, None)
    uri.userInfo should equal(userInfo)
    uri.withUserInfo().userInfo should equal(None)
  }

  it should "not change the userInfo when userInfo was empty, and provided empty userInfo" in {
    EmptyUri.withUser() should equal(EmptyUri)
  }

  it should "fail when the host was empty, and userInfo is not empty" in {
    intercept[IllegalArgumentException] {
      EmptyUri.withUserInfo(UserInfo("user"))
    }
  }

  "`Uri.withUser`" should "change the user when provided a `Uri`" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.user should equal(None)
    val uri2 = Uri(None, Authority.option(user = "user", host = "www.example.com"), None, None, None)
    uri.withUser(uri2).user.value should equal("user")
  }

  it should "change the user when provided an `Authority`" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.user should equal(None)
    uri.withUser(Authority(user = "user", host = "www.example.com")).user.value should equal("user")
  }

  it should "change the user when provided an `UserInfo`" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.user should equal(None)
    uri.withUser(UserInfo("user")).user.value should equal("user")
  }

  it should "change the user when provided a `String`" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.user should equal(None)
    uri.withUser("user").user.value should equal("user")
  }

  it should "remove the user when provided nothing" in {
    val uri = Uri(None, Authority.option(user = "user", host = "www.example.com"), None, None, None)
    uri.user.value should equal("user")
    uri.withUser().user should equal(None)
  }

  it should "change the user when password was not empty, and provided a `String`" in {
    val uri = Uri(None, Authority.option("user", "password", "www.example.com"), None, None, None)
    uri.user.value should equal("user")
    uri.withUser("user2").user.value should equal("user2")
  }

  it should "not change the user when user was empty, and provided empty user" in {
    EmptyUri.withUser() should equal(EmptyUri)
  }

  it should "fail when the host was empty, and user is not empty" in {
    intercept[IllegalArgumentException] {
      EmptyUri.withUser("user")
    }
  }

  "`Uri.withPassword`" should "change the password when provided a `Uri`" in {
    val uri = Uri(None, Authority.option(user = "user", host = "www.example.com"), None, None, None)
    uri.password should equal(None)
    val uri2 = Uri(None, Authority.option("user", "password", "www.example2.com"), None, None, None)
    uri.withPassword(uri2).password.value should equal("password")
  }

  it should "change the password when provided an `Authority`" in {
    val uri = Uri(None, Authority.option(user = "user", host = "www.example.com"), None, None, None)
    uri.password should equal(None)
    uri.withPassword(Authority(user = "user", password = "password2", host = "www.example2.com")).password.value should equal("password2")
  }

  it should "change the password when provided an `UserInfo`" in {
    val uri = Uri(None, Authority.option(user = "user", host = "www.example.com"), None, None, None)
    uri.password should equal(None)
    uri.withPassword(UserInfo("user", "password2")).password.value should equal("password2")
  }

  it should "change the password when provided a `String`" in {
    val uri = Uri(None, Authority.option(user = "user", host = "www.example.com"), None, None, None)
    uri.password should equal(None)
    uri.withPassword("password").password.value should equal("password")
  }

  it should "remove the password when provided nothing" in {
    val uri = Uri(None, Authority.option(user = "user", password = "password", host = "www.example.com"), None, None, None)
    uri.password.value should equal("password")
    uri.withPassword().password should equal(None)
  }

  it should "not change the password when authority was empty, and provided empty password" in {
    EmptyUri.withPassword() should equal(EmptyUri)
  }

  it should "fail when the authority was empty, and password is not empty" in {
    intercept[IllegalArgumentException] {
      EmptyUri.withPassword("password")
    }
  }

  it should "not change the password when the authority was not empty, the userInfo was empty, and provided empty" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.withPassword() should equal(uri)
  }

  it should "fail when the authority was not empty, the userInfo was empty, and password is not empty" in {
    intercept[IllegalArgumentException] {
      Uri(None, Authority.option(host = "www.example.com"), None, None, None).withPassword("password")
    }
  }

  "`Uri.withHost`" should "change the host when provided a `Uri`" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.host.value should equal("www.example.com")
    val uri2 = Uri(None, Authority.option(host = "www.example2.com"), None, None, None)
    uri.withHost(uri2).host.value should equal("www.example2.com")
  }

  it should "change the host when provided an `Authority`" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.host.value should equal("www.example.com")
    uri.withHost(Authority(host = "www.example2.com")).host.value should equal("www.example2.com")
  }

  it should "change the host when provided a `String`" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.host.value should equal("www.example.com")
    uri.withHost("www.example2.com").host.value should equal("www.example2.com")
  }

  it should "remove the host when provided nothing" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.host.value should equal("www.example.com")
    uri.withHost().host should equal(None)
  }

  it should "change the host when authority was empty" in {
    EmptyUri.withHost("www.example.com").host.value should equal("www.example.com")
  }

  "`Uri.withPort`" should "change the port when provided a `Uri`" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.port should equal(None)
    val uri2 = Uri(None, Authority.option(host = "www.example.com", port = 8080), None, None, None)
    uri.withPort(uri2).port.value should equal(8080)
  }

  it should "change the port when provided an `Authority`" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.port should equal(None)
    uri.withPort(Authority(host = "www.example.com", port = 8080)).port.value should equal(8080)
  }

  it should "change the port when provided an `Int`" in {
    val uri = Uri(None, Authority.option(host = "www.example.com"), None, None, None)
    uri.port should equal(None)
    uri.withPort(8080).port.value should equal(8080)
  }

  it should "remove the port when provided nothing" in {
    val uri = Uri(None, Authority.option(host = "www.example.com", port = 8080), None, None, None)
    uri.port.value should equal(8080)
    uri.withPort().port should equal(None)
  }

  it should "not change the port when port was empty, and provided 0" in {
    EmptyUri.withPort(0) should equal(EmptyUri)
  }

  it should "fail when the host was empty, and port is not 0" in {
    intercept[IllegalArgumentException] {
      EmptyUri.withPort(8080)
    }
  }

  "`Uri.authorityToString` and therefore `Authority.toString`" should "work with host" in {
    val uri = Uri(None, Authority.option(None, "www.example.com", None), None, None, None)
    uri.authorityToString should equal("//www.example.com")
  }

  it should "work with user and host" in {
    val uri = Uri(None, Authority.option(UserInfo.option("user", None), "www.example.com", None), None, None, None)
    uri.authorityToString should equal("//user@www.example.com")
  }

  it should "work with user, password, and host" in {
    val uri = Uri(None, Authority.option(UserInfo.option("user", Some("password")), "www.example.com", None), None, None, None)
    uri.authorityToString should equal("//user:password@www.example.com")
  }

  it should "work with host and port" in {
    val uri = Uri(None, Authority.option(None, "www.example.com", Some(8080)), None, None, None)
    uri.authorityToString should equal("//www.example.com:8080")
  }

  it should "work with user, host and port" in {
    val uri = Uri(None, Authority.option(UserInfo.option("user", None), "www.example.com", Some(8080)), None, None, None)
    uri.authorityToString should equal("//user@www.example.com:8080")
  }

  it should "work with user, password, host and port" in {
    val uri = Uri(None, Authority.option(UserInfo.option("user", Some("password")), "www.example.com", Some(8080)), None, None, None)
    uri.authorityToString should equal("//user:password@www.example.com:8080")
  }

  it should "work without an authority" in {
    EmptyUri.authorityToString should equal("")
  }

  "`Uri.authorityToStringRaw` and therefore `Authority.toStringRaw`" should "work with host" in {
    val uri = Uri(None, Authority.option(None, "www.example.com", None), None, None, None)
    uri.authorityToStringRaw should equal("//www.example.com")
  }

  it should "work with user and host" in {
    val uri = Uri(None, Authority.option(UserInfo.option("user", None), "www.example.com", None), None, None, None)
    uri.authorityToStringRaw should equal("//user@www.example.com")
  }

  it should "work with user, password, and host" in {
    val uri = Uri(None, Authority.option(UserInfo.option("user", Some("password")), "www.example.com", None), None, None, None)
    uri.authorityToStringRaw should equal("//user:password@www.example.com")
  }

  it should "work with host and port" in {
    val uri = Uri(None, Authority.option(None, "www.example.com", Some(8080)), None, None, None)
    uri.authorityToStringRaw should equal("//www.example.com:8080")
  }

  it should "work with user, host and port" in {
    val uri = Uri(None, Authority.option(UserInfo.option("user", None), "www.example.com", Some(8080)), None, None, None)
    uri.authorityToStringRaw should equal("//user@www.example.com:8080")
  }

  it should "work with user, password, host and port" in {
    val uri = Uri(None, Authority.option(UserInfo.option("user", Some("password")), "www.example.com", Some(8080)), None, None, None)
    uri.authorityToStringRaw should equal("//user:password@www.example.com:8080")
  }

  it should "work without an authority" in {
    EmptyUri.authorityToStringRaw should equal("")
  }

  "`Authority.copy`" should "succeed with userInfo, host and port" in {
    val authority = Authority("user", "password", "www.example.com", 8080).copy(UserInfo.option("user2", Some("password2")), "www.example2.com", Some(8082))
    authority.userInfo.value should equal(UserInfo("user2", Some("password2")))
    authority.host should equal("www.example2.com")
    authority.port.value should equal(8082)
  }

  it should "succeed with host and port, and without userInfo" in {
    val authority = Authority("user", "password", "www.example.com", 8080).copy(host = "www.example2.com", port = Some(8082))
    authority.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.host should equal("www.example2.com")
    authority.port.value should equal(8082)
  }

  it should "succeed with userInfo and port, and without host" in {
    val authority = Authority("user", "password", "www.example.com", 8080).copy(UserInfo.option("user2", Some("password2")), port = Some(8082))
    authority.userInfo.value should equal(UserInfo("user2", Some("password2")))
    authority.host should equal("www.example.com")
    authority.port.value should equal(8082)
  }

  it should "succeed with userInfo and host, and without port" in {
    val authority = Authority("user", "password", "www.example.com", 8080).copy(UserInfo.option("user2", Some("password2")), "www.example2.com")
    authority.userInfo.value should equal(UserInfo("user2", Some("password2")))
    authority.host should equal("www.example2.com")
    authority.port.value should equal(8080)
  }

  it should "succeed with userInfo, and without host and port" in {
    val authority = Authority("user", "password", "www.example.com", 8080).copy(UserInfo.option("user2", Some("password2")))
    authority.userInfo.value should equal(UserInfo("user2", Some("password2")))
    authority.host should equal("www.example.com")
    authority.port.value should equal(8080)
  }

  it should "succeed with host, and without userInfo and port" in {
    val authority = Authority("user", "password", "www.example.com", 8080).copy(host = "www.example2.com")
    authority.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.host should equal("www.example2.com")
    authority.port.value should equal(8080)
  }

  it should "succeed with port, and without userInfo and host" in {
    val authority = Authority("user", "password", "www.example.com", 8080).copy(port = Some(8082))
    authority.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.host should equal("www.example.com")
    authority.port.value should equal(8082)
  }

  it should "fail when passed `null` host" in {
    intercept[IllegalArgumentException] {
      Authority("user", "password", "www.example.com", 8080).copy(host = null)
    }
  }

  it should "fail when passed an empty string host" in {
    intercept[IllegalArgumentException] {
      Authority("user", "password", "www.example.com", 8080).copy(host = "")
    }
  }

  it should "fail when passed a negative port" in {
    intercept[IllegalArgumentException] {
      Authority("user", "password", "www.example.com", 8080).copy(port = Some(-23))
    }
  }

  it should "fail when passed a zero port" in {
    intercept[IllegalArgumentException] {
      Authority("user", "password", "www.example.com", 8080).copy(port = Some(0))
    }
  }

  it should "fail when passed a too large port" in {
    intercept[IllegalArgumentException] {
      Authority("user", "password", "www.example.com", 8080).copy(port = Some(65536))
    }
  }

  "`Authority.apply`" should "succeed with userInfo, host and port" in {
    val authority = Authority(UserInfo.option("user", Some("password")), "www.example.com", Some(8080))
    authority.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.host should equal("www.example.com")
    authority.port.value should equal(8080)
  }

  it should "succeed with host and port, and without userInfo" in {
    val authority = Authority(None, "www.example.com", Some(8080))
    authority.userInfo should equal(None)
    authority.host should equal("www.example.com")
    authority.port.value should equal(8080)
  }

  it should "succeed with userInfo and host, and without port" in {
    val authority = Authority(UserInfo.option("user", Some("password")), "www.example.com", None)
    authority.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.host should equal("www.example.com")
    authority.port should equal(None)
  }

  it should "succeed with host, and without userInfo and port" in {
    val authority = Authority(None, "www.example.com", None)
    authority.userInfo should equal(None)
    authority.host should equal("www.example.com")
    authority.port should equal(None)
  }

  it should "fail when passed `null` host" in {
    intercept[IllegalArgumentException] {
      Authority(None, null, None)
    }
  }

  it should "fail when passed an empty string host" in {
    intercept[IllegalArgumentException] {
      Authority(None, "", None)
    }
  }

  it should "fail when passed a negative port" in {
    intercept[IllegalArgumentException] {
      Authority(None, "www.example.com", Some(-23))
    }
  }

  it should "fail when passed a zero port" in {
    intercept[IllegalArgumentException] {
      Authority(None, "www.example.com", Some(0))
    }
  }

  it should "fail when passed a too large port" in {
    intercept[IllegalArgumentException] {
      Authority(None, "www.example.com", Some(65536))
    }
  }

  it should "succeed with user, password, host and port" in {
    val authority = Authority("user", "password", "www.example.com", 8080)
    authority.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.host should equal("www.example.com")
    authority.port.value should equal(8080)
  }

  it should "fail when passed password without user" in {
    intercept[IllegalArgumentException] {
     Authority(host = null, password = "password")
    }
  }

  it should "fail when passed something other than host without host" in {
    intercept[IllegalArgumentException] {
     Authority(user = "user", host = null)
    }
  }

  "`Authority.option`" should "return Some with userInfo, host and port" in {
    val authority = Authority.option(UserInfo.option("user", Some("password")), "www.example.com", Some(8080))
    authority.value.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.value.host should equal("www.example.com")
    authority.value.port.value should equal(8080)
  }

  it should "return Some with host and port, and without userInfo" in {
    val authority = Authority.option(None, "www.example.com", Some(8080))
    authority.value.userInfo should equal(None)
    authority.value.host should equal("www.example.com")
    authority.value.port.value should equal(8080)
  }

  it should "return Some with userInfo and host, and without port" in {
    val authority = Authority.option(UserInfo.option("user", Some("password")), "www.example.com", None)
    authority.value.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.value.host should equal("www.example.com")
    authority.value.port should equal(None)
  }

  it should "return Some with host, and without userInfo and port" in {
    val authority = Authority.option(None, "www.example.com", None)
    authority.value.userInfo should equal(None)
    authority.value.host should equal("www.example.com")
    authority.value.port should equal(None)
  }

  it should "return None when passed `null` host" in {
   Authority.option(None, null, None) should equal(None)
  }

  it should "return None when passed an empty string host" in {
   Authority.option(None, "", None) should equal(None)
  }

  it should "fail when passed a negative port" in {
    intercept[IllegalArgumentException] {
     Authority.option(None, "www.example.com", Some(-23))
    }
  }

  it should "fail when passed a zero port" in {
    intercept[IllegalArgumentException] {
     Authority.option(None, "www.example.com", Some(0))
    }
  }

  it should "fail when passed a too large port" in {
    intercept[IllegalArgumentException] {
     Authority.option(None, "www.example.com", Some(65536))
    }
  }

  it should "return Some with user, password, host and port" in {
    val authority = Authority.option("user", "password", "www.example.com", 8080)
    authority.value.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.value.host should equal("www.example.com")
    authority.value.port.value should equal(8080)
  }

  it should "fail when passed password without user" in {
    intercept[IllegalArgumentException] {
     Authority.option(host = null, password = "password")
    }
  }

  it should "fail when passed something other than host without host" in {
    intercept[IllegalArgumentException] {
     Authority.option(user = "user", host = null)
    }
  }

  "`UserInfo.copy`" should "succeed with user and password" in {
    val userInfo = UserInfo("user").copy("user2", Some("password2"))
    userInfo.user should equal("user2")
    userInfo.password.value should equal("password2")
  }

  it should "succeed with user, and without password" in {
    val userInfo = UserInfo("user").copy("user2")
    userInfo.user should equal("user2")
    userInfo.password should equal(None)
  }

  it should "succeed with user, and without password (with existing password)" in {
    val userInfo = UserInfo("user", "password").copy("user2")
    userInfo.user should equal("user2")
    userInfo.password.value should equal("password")
  }

  it should "succeed with password, and without user" in {
    val userInfo = UserInfo("user").copy(password = Some("password2"))
    userInfo.user should equal("user")
    userInfo.password.value should equal("password2")
  }

  it should "fail when passed `null` user" in {
    intercept[IllegalArgumentException] {
      UserInfo("user").copy(null)
    }
  }

  "`UserInfo.toStringRaw`" should "succeed" in {
    UserInfo("user", Some("password")).toStringRaw(UriConfig.default) should equal("user:password@")
  }
}
