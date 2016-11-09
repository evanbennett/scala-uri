package com.netaporter.uri

/**
 * URI Authority, based on RFC 3986 section 3.2.
 *
 * @param userInfo the optional user info, which cannot exist without a host
 * @param host the optional host
 * @param port the optional port, which cannot exist without a host, and where `-1` represents an empty port
 */
sealed abstract case class Authority(userInfo: Option[UserInfo], host: Option[Host], port: Option[Int]) {

  def userInfoString: Option[String] = userInfo match {
    case Some(userInfo: StringUserInfo) => Option(userInfo.userInfoString)
    case _ => None
  }

  def user: Option[String] = userInfo match {
    case Some(userInfo: UserPasswordUserInfo) => Option(userInfo.user)
    case _ => None
  }

  def password: Option[String] = userInfo match {
    case Some(userInfo: UserPasswordUserInfo) => userInfo.password
    case _ => None
  }

  def hostString: Option[String] = host.map(_.hostString)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def copy(userInfo: Option[UserInfo] = userInfo, host: Option[Host] = host, port: Option[Int] = port): Authority = Authority(userInfo, host, port)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit config: UriConfig): String =
    "//" + userInfo.map(_.toString).getOrElse("") + host.map(_.toString).getOrElse("") + port.map(port => if (port == -1 && config.emptyComponentNormalization) "" else if (port == -1) ":" else ":" + port).getOrElse("")
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
      if (port.nonEmpty && port.exists(port => port < -1 || port == 0 || port > 65535)) throw new IllegalArgumentException("Invalid `port`. [" + port + "]")
      new Authority(userInfo, host, port) {}
    }
  }

  /** NOTE: Does not support `StringUserInfo`. */
  def apply(user: String = null, password: String = null, registeredName: String = null, ipv4Address: String = null, ipLiteral: String = null, port: Int = 0)(implicit config: UriConfig): Authority =
    apply(UserInfo.option(user, password), Host.option(registeredName, ipv4Address, ipLiteral), if (port == 0) None else Option(port))

  def option(userInfo: Option[UserInfo], host: Option[Host], port: Option[Int]): Option[Authority] = Option(apply(userInfo, host, port))

  /** NOTE: Does not support `StringUserInfo`. */
  def option(user: String = null, password: String = null, registeredName: String = null, ipv4Address: String = null, ipLiteral: String = null, port: Int = 0)(implicit config: UriConfig): Option[Authority] =
    Option(apply(user, password, registeredName, ipv4Address, ipLiteral, port))
}

object EmptyAuthority extends Authority(None, None, None)
