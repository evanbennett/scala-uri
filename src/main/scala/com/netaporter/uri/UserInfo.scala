package com.netaporter.uri

/**
 * URI User Info, based on RFC 3986 section 3.2.1.
 * `UserInfo` can have `userInfo` OR (`user` and optionally `password`), but not both.
 */
sealed abstract class UserInfo {

  def toString(implicit config: UriConfig): String
}

object UserInfo {

  def apply(userInfo: String): UserInfo =
    if (userInfo != null && userInfo.isEmpty) EmptyUserInfo else StringUserInfo(userInfo)

  def apply(user: String, password: Option[String]): UserInfo =
    if (user != null && user.isEmpty && password != null && password.isEmpty) EmptyUserInfo else UserPasswordUserInfo(user, password)

  def apply(user: String, password: String): UserInfo = apply(if (user == null) "" else user, Option(password))

  def option(userInfo: String): Option[UserInfo] =
    if (userInfo != null && userInfo.isEmpty) Option(EmptyUserInfo) else StringUserInfo.option(userInfo)

  def option(user: String, password: Option[String]): Option[UserInfo] =
    if (user != null && user.isEmpty && password != null && password.isEmpty) Option(EmptyUserInfo) else UserPasswordUserInfo.option(user, password)

  def option(user: String, password: String): Option[UserInfo] = option(user, Option(password))
}

sealed abstract case class StringUserInfo(userInfoString: String) extends UserInfo {

  def toString(implicit config: UriConfig): String =
    config.userInfoEncoder.encode(userInfoString) + "@"
}

object StringUserInfo {

  def apply(userInfo: String): StringUserInfo = {
    if (userInfo == null) throw new IllegalArgumentException("`userInfo` cannot be `null`.")
    if (userInfo.isEmpty) throw new IllegalArgumentException("`userInfo` must not be empty.")
    new StringUserInfo(userInfo) {}
  }

  def option(userInfo: String): Option[StringUserInfo] =
    if (userInfo == null || userInfo.isEmpty) None else Option(apply(userInfo))
}

sealed abstract case class UserPasswordUserInfo(user: String, password: Option[String]) extends UserInfo {

  def copy(user: String = user, password: Option[String] = password): UserPasswordUserInfo = UserPasswordUserInfo(user, password)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit config: UriConfig): String = {
    if (!config.emptyComponentNormalization) config.userEncoder.encode(user) + password.map(":" + config.passwordEncoder.encode(_)).getOrElse("") + "@"
    else {
      (user, password) match {
        case ("", None) | ("", Some("")) => ""
        case (user, None) => config.userEncoder.encode(user) + "@"
        case (user, Some("")) => config.userEncoder.encode(user) + "@"
        case (user, Some(password)) => config.userEncoder.encode(user) + ":" + config.passwordEncoder.encode(password) + "@"
      }
    }
  }
}

object UserPasswordUserInfo {

  def apply(user: String, password: Option[String] = None): UserPasswordUserInfo = {
    if (user == null) throw new IllegalArgumentException("`user` cannot be `null`.")
    if (password == null) throw new IllegalArgumentException("`password` cannot be `null`.")
    if (user.isEmpty && password.isEmpty) throw new IllegalArgumentException("`user` or `password` must not be empty.")
    new UserPasswordUserInfo(user, password) {}
  }

  def apply(user: String, password: String): UserPasswordUserInfo = apply(if (user == null) "" else user, Option(password))

  def option(user: String, password: Option[String] = None): Option[UserPasswordUserInfo] =
    if (user == null && (password == null || password.isEmpty)) None else Option(apply(if (user == null) "" else user, password))

  def option(user: String, password: String): Option[UserPasswordUserInfo] = option(user, Option(password))
}

object EmptyUserInfo extends UserInfo {

  def toString(implicit config: UriConfig): String =
    if (config.emptyComponentNormalization) "" else "@"
}
