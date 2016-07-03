package com.netaporter.uri

import com.netaporter.uri.config.UriConfig

sealed abstract case class Authority(userInfo: Option[UserInfo], host: Option[Host], port: Option[Int]) {

  def user: Option[String] = userInfo.map(_.user)

  def password: Option[String] = userInfo.flatMap(_.password)

  def hostString: Option[String] = host.map(_.host)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def copy(userInfo: Option[UserInfo] = userInfo, host: Option[Host] = host, port: Option[Int] = port): Authority = Authority(userInfo, host, port)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit c: UriConfig): String =
    "//" + userInfo.map(_.toString).getOrElse("") + host.map(_.toString).getOrElse("") + port.map(":" + _).getOrElse("")

  def toStringRaw(implicit c: UriConfig): String = toString(c.withNoEncoding)
}

object Authority {

  def apply(userInfo: Option[UserInfo], host: Option[Host], port: Option[Int]): Authority = {
    if (userInfo == null) throw new IllegalArgumentException("`userInfo` cannot be `null`.")
    if (host == null) throw new IllegalArgumentException("`host` cannot be `null`.")
    if (port == null) throw new IllegalArgumentException("`port` cannot be `null`.")
    if (host.isEmpty) {
      if (userInfo.nonEmpty) throw new IllegalArgumentException("Cannot have a `userInfo` without a `host`.")
      if (port.nonEmpty) throw new IllegalArgumentException("Cannot have a `port` without a `host`.")
      EmptyAuthority
    } else {
      if (port.nonEmpty && port.exists(port => port < 1 || port > 65535)) throw new IllegalArgumentException("Invalid `port`. [" + port + "]")
      new Authority(userInfo, host, port) {}
    }
  }

  def apply(user: String = null, password: String = null, registeredName: String = null, ipv4Address: String = null, ipLiteralAddress: String = null, port: Int = 0): Authority =
    apply(UserInfo.option(user, password), Host.option(registeredName, ipv4Address, ipLiteralAddress), if (port == 0) None else Option(port))

  def option(userInfo: Option[UserInfo], host: Option[Host], port: Option[Int]): Option[Authority] = {
    if (host == null || host.isEmpty) {
      if (userInfo != null && userInfo.nonEmpty) throw new IllegalArgumentException("Cannot have a `userInfo` without a `host`.")
      if (port != null && port.nonEmpty) throw new IllegalArgumentException("Cannot have a `port` without a `host`.")
      Some(EmptyAuthority)
    } else Option(apply(userInfo, host, port))
  }

  def option(user: String = null, password: String = null, registeredName: String = null, ipv4Address: String = null, ipLiteralAddress: String = null, port: Int = 0): Option[Authority] =
    option(UserInfo.option(user, password), Host.option(registeredName, ipv4Address, ipLiteralAddress), if (port == 0) None else Option(port))
}

object EmptyAuthority extends Authority(None, None, None)
