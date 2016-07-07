package com.netaporter.uri

import com.netaporter.uri.config.UriConfig

class AuthorityTests extends TestSpec {

  "`Uri.userInfo`" should "return the user info" in {
    val uri = Uri(None, Authority.option("user", "password", "www.example.com", port = 8080), None, None, None)
    uri.userInfo.value should equal(UserInfo("user", Some("password")))
  }

  it should "return `None`" in {
    EmptyRelativeReference.userInfo should equal(None)
  }

  "`Uri.user` and therefore `Authority.user`" should "return the user" in {
    val uri = Uri(None, Authority.option("user", "password", "www.example.com", port = 8080), None, None, None)
    uri.user.value should equal("user")
  }

  it should "return `None`" in {
    EmptyRelativeReference.user should equal(None)
  }

  "`Uri.password` and therefore `Authority.password`" should "return the password" in {
    val uri = Uri(None, Authority.option("user", "password", "www.example.com", port = 8080), None, None, None)
    uri.password.value should equal("password")
  }

  it should "return `None`" in {
    EmptyRelativeReference.password should equal(None)
  }

  "`Uri.host`" should "return the host" in {
    val uri = Uri(None, Authority.option("user", "password", "www.example.com", port = 8080), None, None, None)
    uri.host.value should equal("www.example.com")
  }

  it should "return `None`" in {
    EmptyRelativeReference.host should equal(None)
  }

  "`Uri.hostParts`" should "return the dot separated host" in {
    val uri = Uri(None, Authority.option(registeredName = "theon.github.com"), None, None, None)
    uri.hostParts should equal(Seq("theon", "github", "com"))
  }

  it should "return `Seq.empty` when the host is empty" in {
    EmptyRelativeReference.hostParts should equal(Seq.empty)
  }

  "`Uri.subdomain`" should "return the first dot separated part of the host" in {
    val uri = Uri(None, Authority.option(registeredName = "theon.github.com"), None, None, None)
    uri.subdomain.value should equal("theon")
  }

  it should "return None when the host is empty" in {
    EmptyRelativeReference.subdomain should equal(None)
  }

  "`Uri.publicSuffix`" should "match the longest public suffix" in {
    val uri = Uri(None, Authority.option(registeredName = "www.google.co.uk"), None, None, None)
    uri.publicSuffix.value should equal("co.uk")
  }

  it should "return None when the host is empty" in {
    EmptyRelativeReference.publicSuffix should equal(None)
  }

  it should "only return public suffixes that match full dot separated host parts" in {
    val uri = Uri(None, Authority.option(registeredName = "www.bar.com"), None, None, None)
    // Github issue #110: Should not match ar.com
    uri.publicSuffix.value should equal("com")
  }

  "`Uri.publicSuffixes`" should "match all public suffixes" in {
    val uri = Uri(None, Authority.option(registeredName = "www.google.co.uk"), None, None, None)
    uri.publicSuffixes should equal(Seq("co.uk", "uk"))
  }

  it should "return `Seq.empty` when the host is empty" in {
    EmptyRelativeReference.publicSuffixes should equal(Seq.empty)
  }

  "`Uri.port`" should "return the port" in {
    val uri = Uri(None, Authority.option("user", "password", "www.example.com", port = 8080), None, None, None)
    uri.port.value should equal(8080)
  }

  it should "return `None`" in {
    EmptyRelativeReference.port should equal(None)
  }

  "`Uri.withAuthority`" should "change the authority when provided a `Uri`" in {
    val authority = Authority.option(registeredName = "www.example.com")
    val uri = Uri(None, authority, None, None, None)
    EmptyRelativeReference.withAuthority(uri).authority should equal(authority)
  }

  it should "change the authority when provided an `Authority`" in {
    val authority = Authority.option(registeredName = "www.example.com")
    val uri = Uri(None, authority, None, None, None)
    uri.authority should equal(authority)
    val authority2 = Authority(registeredName = "www.example2.com")
    uri.withAuthority(authority2).authority.value should equal(authority2)
  }

  it should "remove the authority when provided nothing" in {
    val authority = Authority.option(registeredName = "www.example.com")
    val uri = Uri(None, authority, None, None, None)
    uri.authority should equal(authority)
    uri.withAuthority().authority should equal(None)
  }

  "`Uri.withUserInfo`" should "change the userInfo when provided a `Uri`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.userInfo should equal(None)
    val userInfo = UserInfo.option("user")
    val uri2 = Uri(None, Authority.option("user", registeredName = "www.example.com"), None, None, None)
    uri.withUserInfo(uri2).userInfo should equal(userInfo)
  }

  it should "change the userInfo when provided an `Authority`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.userInfo should equal(None)
    val userInfo = UserInfo.option("user")
    uri.withUserInfo(Authority("user", registeredName = "www.example.com")).userInfo should equal(userInfo)
  }

  it should "change the userInfo when provided an `UserInfo`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.userInfo should equal(None)
    val userInfo = UserInfo("user")
    uri.withUserInfo(userInfo).userInfo.value should equal(userInfo)
  }

  it should "remove the userInfo when provided nothing" in {
    val userInfo = UserInfo.option("user")
    val uri = Uri(None, Authority.option("user", registeredName = "www.example.com"), None, None, None)
    uri.userInfo should equal(userInfo)
    uri.withUserInfo().userInfo should equal(None)
  }

  it should "not change the userInfo when userInfo was empty, and provided empty userInfo" in {
    EmptyRelativeReference.withUser() should equal(EmptyRelativeReference)
  }

  it should "fail when the host was empty, and userInfo is not empty" in {
    intercept[IllegalArgumentException] {
      EmptyRelativeReference.withUserInfo(UserInfo("user"))
    }
  }

  "`Uri.withUser`" should "change the user when provided a `Uri`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.user should equal(None)
    val uri2 = Uri(None, Authority.option(user = "user", registeredName = "www.example.com"), None, None, None)
    uri.withUser(uri2).user.value should equal("user")
  }

  it should "change the user when provided an `Authority`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.user should equal(None)
    uri.withUser(Authority(user = "user", registeredName = "www.example.com")).user.value should equal("user")
  }

  it should "change the user when provided an `UserInfo`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.user should equal(None)
    uri.withUser(UserInfo("user")).user.value should equal("user")
  }

  it should "change the user when provided a `String`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.user should equal(None)
    uri.withUser("user").user.value should equal("user")
  }

  it should "remove the user when provided nothing" in {
    val uri = Uri(None, Authority.option(user = "user", registeredName = "www.example.com"), None, None, None)
    uri.user.value should equal("user")
    uri.withUser().user should equal(None)
  }

  it should "change the user when password was not empty, and provided a `String`" in {
    val uri = Uri(None, Authority.option("user", "password", "www.example.com"), None, None, None)
    uri.user.value should equal("user")
    uri.withUser("user2").user.value should equal("user2")
  }

  it should "not change the user when user was empty, and provided empty user" in {
    EmptyRelativeReference.withUser() should equal(EmptyRelativeReference)
  }

  it should "fail when the host was empty, and user is not empty" in {
    intercept[IllegalArgumentException] {
      EmptyRelativeReference.withUser("user")
    }
  }

  "`Uri.withPassword`" should "change the password when provided a `Uri`" in {
    val uri = Uri(None, Authority.option(user = "user", registeredName = "www.example.com"), None, None, None)
    uri.password should equal(None)
    val uri2 = uri.withPassword(Uri(None, Authority.option("user2", "password", "www.example2.com"), None, None, None))
    uri2.user.value should equal("user")
    uri2.password.value should equal("password")
    uri2.host.value should equal("www.example.com")
  }

  it should "change the password when provided an `Authority`" in {
    val uri = Uri(None, Authority.option(user = "user", registeredName = "www.example.com"), None, None, None)
    uri.password should equal(None)
    val uri2 = uri.withPassword(Authority(user = "user2", password = "password2", registeredName = "www.example2.com"))
    uri2.user.value should equal("user")
    uri2.password.value should equal("password2")
    uri2.host.value should equal("www.example.com")
  }

  it should "change the password when provided an `UserInfo`" in {
    val uri = Uri(None, Authority.option(user = "user", registeredName = "www.example.com"), None, None, None)
    uri.password should equal(None)
    val uri2 = uri.withPassword(UserInfo("user2", "password2"))
    uri2.user.value should equal("user")
    uri2.password.value should equal("password2")
    uri2.host.value should equal("www.example.com")
  }

  it should "change the password when provided a `String`" in {
    val uri = Uri(None, Authority.option(user = "user", registeredName = "www.example.com"), None, None, None)
    uri.password should equal(None)
    uri.withPassword("password").password.value should equal("password")
  }

  it should "remove the password when provided nothing" in {
    val uri = Uri(None, Authority.option(user = "user", password = "password", registeredName = "www.example.com"), None, None, None)
    uri.password.value should equal("password")
    uri.withPassword().password should equal(None)
  }

  it should "not change the password when authority was empty, and provided empty password" in {
    EmptyRelativeReference.withPassword() should equal(EmptyRelativeReference)
  }

  it should "fail when the authority was empty, and password is not empty" in {
    intercept[IllegalArgumentException] {
      EmptyRelativeReference.withPassword("password")
    }
  }

  it should "not change the password when the authority was not empty, the userInfo was empty, and provided empty" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.withPassword() should equal(uri)
  }

  it should "fail when the authority was not empty, the userInfo was empty, and password is not empty" in {
    intercept[IllegalArgumentException] {
      Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None).withPassword("password")
    }
  }

  "`Uri.withHost`" should "change the host when provided a `Uri`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.host.value should equal("www.example.com")
    val uri2 = Uri(None, Authority.option(registeredName = "www.example2.com"), None, None, None)
    uri.withHost(uri2).host.value should equal("www.example2.com")
  }

  it should "change the host when provided an `Authority`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.host.value should equal("www.example.com")
    uri.withHost(Authority(registeredName = "www.example2.com")).host.value should equal("www.example2.com")
  }

  it should "change the host when provided an `Option[Host]`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.host.value should equal("www.example.com")
    uri.withHost(Host.option("www.example2.com")).host.value should equal("www.example2.com")
  }

  it should "change the host when provided an `Host`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.host.value should equal("www.example.com")
    uri.withHost(Host("www.example2.com")).host.value should equal("www.example2.com")
  }

  it should "change the host when provided a `String`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.host.value should equal("www.example.com")
    uri.withHost("www.example2.com").authority.value.host.value.registeredName.value should equal("www.example2.com")
  }

  it should "change the host when provided a `String` as `ipv4Address`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.host.value should equal("www.example.com")
    uri.withHost(ipv4Address = "10.0.0.5").authority.value.host.value.ipv4Address.value should equal("10.0.0.5")
  }

  it should "change the host when provided a `String` as `ipLiteralAddress` (IPv6)" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.host.value should equal("www.example.com")
    uri.withHost(ipLiteralAddress = "[d45::2351]").authority.value.host.value.ipLiteralAddress.value should equal("[d45::2351]")
  }

  it should "change the host when provided a `String` as `ipLiteralAddress` (IPvFuture)" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.host.value should equal("www.example.com")
    uri.withHost(ipLiteralAddress = "[vff.whoknows]").authority.value.host.value.ipLiteralAddress.value should equal("[vff.whoknows]")
  }

  it should "remove the host when provided nothing" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.host.value should equal("www.example.com")
    uri.withHost().host should equal(None)
  }

  it should "change the host when authority was empty" in {
    EmptyRelativeReference.withHost("www.example.com").host.value should equal("www.example.com")
  }

  "`Uri.withPort`" should "change the port when provided a `Uri`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.port should equal(None)
    val uri2 = Uri(None, Authority.option(registeredName = "www.example.com", port = 8080), None, None, None)
    uri.withPort(uri2).port.value should equal(8080)
  }

  it should "change the port when provided an `Authority`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.port should equal(None)
    uri.withPort(Authority(registeredName = "www.example.com", port = 8080)).port.value should equal(8080)
  }

  it should "change the port when provided an `Int`" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.port should equal(None)
    uri.withPort(8080).port.value should equal(8080)
  }

  it should "remove the port when provided nothing" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com", port = 8080), None, None, None)
    uri.port.value should equal(8080)
    uri.withPort().port should equal(None)
  }

  it should "not change the port when port was empty, and provided 0" in {
    EmptyRelativeReference.withPort(0) should equal(EmptyRelativeReference)
  }

  it should "fail when the host was empty, and port is not 0" in {
    intercept[IllegalArgumentException] {
      EmptyRelativeReference.withPort(8080)
    }
  }

  "`Uri.authorityToString` and therefore `Authority.toString`" should "work with host" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.authorityToString should equal("//www.example.com")
  }

  it should "work with user and host" in {
    val uri = Uri(None, Authority.option("user", registeredName = "www.example.com"), None, None, None)
    uri.authorityToString should equal("//user@www.example.com")
  }

  it should "work with user, password, and host" in {
    val uri = Uri(None, Authority.option("user", "password", "www.example.com"), None, None, None)
    uri.authorityToString should equal("//user:password@www.example.com")
  }

  it should "work with host and port" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com", port = 8080), None, None, None)
    uri.authorityToString should equal("//www.example.com:8080")
  }

  it should "work with user, host and port" in {
    val uri = Uri(None, Authority.option("user", registeredName = "www.example.com", port = 8080), None, None, None)
    uri.authorityToString should equal("//user@www.example.com:8080")
  }

  it should "work with user, password, host and port" in {
    val uri = Uri(None, Authority.option("user", "password", "www.example.com", port = 8080), None, None, None)
    uri.authorityToString should equal("//user:password@www.example.com:8080")
  }

  it should "work without an authority" in {
    EmptyRelativeReference.authorityToString should equal("")
  }

  "`Uri.authorityToStringRaw` and therefore `Authority.toStringRaw`" should "work with host" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com"), None, None, None)
    uri.authorityToStringRaw should equal("//www.example.com")
  }

  it should "work with user and host" in {
    val uri = Uri(None, Authority.option("user", registeredName = "www.example.com"), None, None, None)
    uri.authorityToStringRaw should equal("//user@www.example.com")
  }

  it should "work with user, password, and host" in {
    val uri = Uri(None, Authority.option("user", "password", "www.example.com"), None, None, None)
    uri.authorityToStringRaw should equal("//user:password@www.example.com")
  }

  it should "work with host and port" in {
    val uri = Uri(None, Authority.option(registeredName = "www.example.com", port = 8080), None, None, None)
    uri.authorityToStringRaw should equal("//www.example.com:8080")
  }

  it should "work with user, host and port" in {
    val uri = Uri(None, Authority.option("user", registeredName = "www.example.com", port = 8080), None, None, None)
    uri.authorityToStringRaw should equal("//user@www.example.com:8080")
  }

  it should "work with user, password, host and port" in {
    val uri = Uri(None, Authority.option("user", "password", "www.example.com", port = 8080), None, None, None)
    uri.authorityToStringRaw should equal("//user:password@www.example.com:8080")
  }

  it should "work without an authority" in {
    EmptyRelativeReference.authorityToStringRaw should equal("")
  }

  "`Authority.copy`" should "succeed with userInfo, host and port" in {
    val authority = Authority("user", "password", "www.example.com", port = 8080).copy(UserInfo.option("user2", Some("password2")), Host.option("www.example2.com"), Some(8082))
    authority.userInfo.value should equal(UserInfo("user2", Some("password2")))
    authority.hostString.value should equal("www.example2.com")
    authority.port.value should equal(8082)
  }

  it should "succeed with host and port, and without userInfo" in {
    val authority = Authority("user", "password", "www.example.com", port = 8080).copy(host = Host.option("www.example2.com"), port = Some(8082))
    authority.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.hostString.value should equal("www.example2.com")
    authority.port.value should equal(8082)
  }

  it should "succeed with userInfo and port, and without host" in {
    val authority = Authority("user", "password", "www.example.com", port = 8080).copy(UserInfo.option("user2", Some("password2")), port = Some(8082))
    authority.userInfo.value should equal(UserInfo("user2", Some("password2")))
    authority.hostString.value should equal("www.example.com")
    authority.port.value should equal(8082)
  }

  it should "succeed with userInfo and host, and without port" in {
    val authority = Authority("user", "password", "www.example.com", port = 8080).copy(UserInfo.option("user2", Some("password2")), Host.option("www.example2.com"))
    authority.userInfo.value should equal(UserInfo("user2", Some("password2")))
    authority.hostString.value should equal("www.example2.com")
    authority.port.value should equal(8080)
  }

  it should "succeed with userInfo, and without host and port" in {
    val authority = Authority("user", "password", "www.example.com", port = 8080).copy(UserInfo.option("user2", Some("password2")))
    authority.userInfo.value should equal(UserInfo("user2", Some("password2")))
    authority.hostString.value should equal("www.example.com")
    authority.port.value should equal(8080)
  }

  it should "succeed with host, and without userInfo and port" in {
    val authority = Authority("user", "password", "www.example.com", port = 8080).copy(host = Host.option("www.example2.com"))
    authority.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.hostString.value should equal("www.example2.com")
    authority.port.value should equal(8080)
  }

  it should "succeed with port, and without userInfo and host" in {
    val authority = Authority("user", "password", "www.example.com", port = 8080).copy(port = Some(8082))
    authority.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.hostString.value should equal("www.example.com")
    authority.port.value should equal(8082)
  }

  it should "fail when passed a negative port" in {
    intercept[IllegalArgumentException] {
      Authority("user", "password", "www.example.com", port = 8080).copy(port = Some(-23))
    }
  }

  it should "fail when passed a zero port" in {
    intercept[IllegalArgumentException] {
      Authority("user", "password", "www.example.com", port = 8080).copy(port = Some(0))
    }
  }

  it should "fail when passed a too large port" in {
    intercept[IllegalArgumentException] {
      Authority("user", "password", "www.example.com", port = 8080).copy(port = Some(65536))
    }
  }

  "`Authority.apply`" should "succeed with userInfo, host and port" in {
    val authority = Authority(UserInfo.option("user", Some("password")), Host.option("www.example.com"), Some(8080))
    authority.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.hostString.value should equal("www.example.com")
    authority.port.value should equal(8080)
  }

  it should "succeed with host and port, and without userInfo" in {
    val authority = Authority(None, Host.option("www.example.com"), Some(8080))
    authority.userInfo should equal(None)
    authority.hostString.value should equal("www.example.com")
    authority.port.value should equal(8080)
  }

  it should "succeed with userInfo and host, and without port" in {
    val authority = Authority(UserInfo.option("user", Some("password")), Host.option("www.example.com"), None)
    authority.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.hostString.value should equal("www.example.com")
    authority.port should equal(None)
  }

  it should "succeed with host, and without userInfo and port" in {
    val authority = Authority(None, Host.option("www.example.com"), None)
    authority.userInfo should equal(None)
    authority.hostString.value should equal("www.example.com")
    authority.port should equal(None)
  }

  it should "succeed without userInfo, host and port" in {
    Authority(None, None, None) should equal(EmptyAuthority)
  }

  it should "fail when passed a negative port" in {
    intercept[IllegalArgumentException] {
      Authority(None, Host.option("www.example.com"), Some(-23))
    }
  }

  it should "fail when passed a zero port" in {
    intercept[IllegalArgumentException] {
      Authority(None, Host.option("www.example.com"), Some(0))
    }
  }

  it should "fail when passed a too large port" in {
    intercept[IllegalArgumentException] {
      Authority(None, Host.option("www.example.com"), Some(65536))
    }
  }

  it should "succeed with user, password, host and port" in {
    val authority = Authority("user", "password", "www.example.com", port = 8080)
    authority.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.hostString.value should equal("www.example.com")
    authority.port.value should equal(8080)
  }

  it should "fail when passed password without user" in {
    intercept[IllegalArgumentException] {
     Authority(password = "password", registeredName = "www.example.com")
    }
  }

  it should "fail when passed something other than host without host" in {
    intercept[IllegalArgumentException] {
     Authority(user = "user")
    }
  }

  it should "fail when passed port without host" in {
    intercept[IllegalArgumentException] {
     Authority(port = 8080)
    }
  }

  it should "fail when passed userInfo (`Option[UserInfo]`) as `null`" in {
    intercept[IllegalArgumentException] {
     Authority(null: Option[UserInfo], None, None)
    }
  }

  it should "fail when passed host (`Option[Host]`) as `null`" in {
    intercept[IllegalArgumentException] {
     Authority(None, null: Option[Host], None)
    }
  }

  it should "fail when passed port (`Option[Int]`) as `null`" in {
    intercept[IllegalArgumentException] {
     Authority(None, None, null: Option[Int])
    }
  }

  "`Authority.option`" should "return Some with userInfo, host and port" in {
    val authority = Authority.option("user", "password", "www.example.com", port = 8080)
    authority.value.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.value.hostString.value should equal("www.example.com")
    authority.value.port.value should equal(8080)
  }

  it should "return Some with host and port, and without userInfo" in {
    val authority = Authority.option(None, Host.option("www.example.com"), Some(8080))
    authority.value.userInfo should equal(None)
    authority.value.hostString.value should equal("www.example.com")
    authority.value.port.value should equal(8080)
  }

  it should "return Some with userInfo and host, and without port" in {
    val authority = Authority.option(UserInfo.option("user", Some("password")), Host.option("www.example.com"), None)
    authority.value.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.value.hostString.value should equal("www.example.com")
    authority.value.port should equal(None)
  }

  it should "return Some with host, and without userInfo and port" in {
    val authority = Authority.option(registeredName = "www.example.com")
    authority.value.userInfo should equal(None)
    authority.value.hostString.value should equal("www.example.com")
    authority.value.port should equal(None)
  }

  it should "return Some without userInfo, host and port" in {
    Authority.option(None, None, None).value should equal(EmptyAuthority)
  }

  it should "fail when passed a negative port" in {
    intercept[IllegalArgumentException] {
     Authority.option(None, Host.option("www.example.com"), Some(-23))
    }
  }

  it should "fail when passed a zero port" in {
    intercept[IllegalArgumentException] {
     Authority.option(None, Host.option("www.example.com"), Some(0))
    }
  }

  it should "fail when passed a too large port" in {
    intercept[IllegalArgumentException] {
     Authority.option(None, Host.option("www.example.com"), Some(65536))
    }
  }

  it should "return Some with user, password, host and port" in {
    val authority = Authority.option("user", "password", "www.example.com", port = 8080)
    authority.value.userInfo.value should equal(UserInfo("user", Some("password")))
    authority.value.hostString.value should equal("www.example.com")
    authority.value.port.value should equal(8080)
  }

  it should "fail when passed password without user" in {
    intercept[IllegalArgumentException] {
     Authority.option(password = "password", registeredName = "www.example.com")
    }
  }

  it should "fail when passed something other than host without host" in {
    intercept[IllegalArgumentException] {
     Authority.option(user = "user")
    }
  }

  it should "fail when passed port without host" in {
    intercept[IllegalArgumentException] {
     Authority.option(port = 8080)
    }
  }

  "`UserInfo.apply`" should "fail when passed a `null` passwoird" in {
    intercept[IllegalArgumentException] {
      UserInfo("user", null: Option[String])
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
    UserInfo("user", Some("password")).toStringRaw(UriConfig.DEFAULT) should equal("user:password@")
  }

  "`Host.host`" should "succeed when it is a `registeredName`" in {
    Host("www.test.com").host should equal("www.test.com")
  }

  it should "succeed when it is an `ipv4Address`" in {
    Host(ipv4Address = "192.168.203.46").host should equal("192.168.203.46")
  }

  it should "succeed when it is an `ipLiteralAddress`" in {
    Host(ipLiteralAddress = "[54d8::2586]").host should equal("[54d8::2586]")
  }

  "`Host.copy`" should "fail as it is not supported" in {
    intercept[UnsupportedOperationException] {
      Host("www.example.com").copy(Some("www.example2.com"))
    }
  }

  "`Host.toString`" should "succeed when it is a `registeredName`" in {
    Host("www.Test.com").toString(UriConfig.DEFAULT) should equal("www.test.com")
  }

  it should "succeed when it is an `ipv4Address`" in {
    Host(ipv4Address = "192.168.203.46").toString(UriConfig.DEFAULT) should equal("192.168.203.46")
  }

  it should "succeed when it is an `ipLiteralAddress`" in {
    Host(ipLiteralAddress = "[54D8::2586]").toString(UriConfig.DEFAULT) should equal("[54d8::2586]")
  }

  "`Host.toStringRaw`" should "succeed when it is a `registeredName`" in {
    Host("www.Test.com").toStringRaw(UriConfig.DEFAULT) should equal("www.Test.com")
  }

  it should "succeed when it is an `ipv4Address`" in {
    Host(ipv4Address = "192.168.203.46").toStringRaw(UriConfig.DEFAULT) should equal("192.168.203.46")
  }

  it should "succeed when it is an `ipLiteralAddress`" in {
    Host(ipLiteralAddress = "[54D8::2586]").toStringRaw(UriConfig.DEFAULT) should equal("[54D8::2586]")
  }

  "`Host.apply` with `Option` arguments" should "fail when all arguments are `None`" in {
    intercept[IllegalArgumentException] {
      Host(None, None, None)
    }
  }

  it should "fail when passed registeredName and ipv4Address" in {
    intercept[IllegalArgumentException] {
      Host(Some("www.example.com"), Some("192.168.203.46"), None)
    }
  }

  it should "fail when passed registeredName and ipLiteralAddress" in {
    intercept[IllegalArgumentException] {
      Host(Some("www.example.com"), None, Some("[54D8::2586]"))
    }
  }

  it should "fail when passed ipv4Address and ipLiteralAddress" in {
    intercept[IllegalArgumentException] {
      Host(None, Some("192.168.203.46"), Some("[54D8::2586]"))
    }
  }

  it should "fail when passed ipv4Address as registeredName" in {
    intercept[IllegalArgumentException] {
      Host(Some("192.168.203.46"), None, None)
    }
  }

  it should "fail when passed an invalid registeredName" in {
    intercept[IllegalArgumentException] {
      Host(Some("[54D8::2586]"), None, None)
    }
  }

  it should "fail when passed an invalid ipv4Address" in {
    intercept[IllegalArgumentException] {
      Host(None, Some("www.example.com"), None)
    }
  }

  it should "fail when passed an invalid ipLiteralAddress" in {
    intercept[IllegalArgumentException] {
      Host(None, None, Some("192.168.203.46"))
    }
  }
}
