package com.netaporter.uri

import com.netaporter.uri.config.UriConfig

sealed abstract case class UserInfo(user: String, password: Option[String]) {

  def copy(user: String = user, password: Option[String] = password): UserInfo = UserInfo(user, password)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit c: UriConfig): String =
    c.userInfoEncoder.encode(user, c.charset) + password.map(":" + c.userInfoEncoder.encode(_, c.charset)).getOrElse("") + "@"

  def toStringRaw(implicit c: UriConfig): String = toString(c.withNoEncoding)
}

object UserInfo {

  def apply(user: String, password: Option[String]): UserInfo = {
    if (user == null || user.isEmpty) throw new IllegalArgumentException("`user` cannot be `null` and cannot be empty.")
    new UserInfo(user, password) {}
  }

  def apply(user: String, password: String = null): UserInfo = apply(user, Option(password))

  def option(user: String, password: Option[String]): Option[UserInfo] = {
    if (user == null || user.isEmpty) {
      if (password == null || password.nonEmpty) throw new IllegalArgumentException("Cannot have a `password` without a `user`.")
      None
    } else Option(apply(user, password))
  }

  def option(user: String = null, password: String = null): Option[UserInfo] = option(user, Option(password))
}
