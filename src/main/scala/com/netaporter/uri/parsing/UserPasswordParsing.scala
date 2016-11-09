package com.netaporter.uri.parsing

import com.netaporter.uri._
import fastparse.all._

trait UserPasswordParsing extends BaseParser {

  protected val extractUserPasswordUserInfo: ((String, Option[String])) => UserInfo = { case (user, password) =>
    UserInfo(config.userDecoder.decode(user, input), password.map(config.passwordDecoder.decode(_, input)))
  }

  protected override val userInfo: P[UserInfo] =
    P(USER.rep.! ~ (":" ~ USER_INFO.rep.!).? ~ "@" map extractUserPasswordUserInfo)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  protected val USER: P[Unit] = UserPasswordParsing.USER
}

object UserPasswordParsing {

  /** User characters from RFC 3986 section 3.2.1. */
  private val USER = CharIn(encoding.PercentEncoder.RfcCharsets.USER.toSeq)
}
