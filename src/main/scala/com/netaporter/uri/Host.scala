package com.netaporter.uri

import com.netaporter.uri.config.UriConfig

// TODO: At one point I was storing the `ipLiteralAddress` without the '[' and ']'. I am not sure which way to go.
sealed abstract case class Host(registeredName: Option[String], ipv4Address: Option[String], ipLiteralAddress: Option[String]) {

  def host: String = registeredName.getOrElse(ipv4Address.getOrElse(ipLiteralAddress.get))

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def copy(registeredName: Option[String] = None, ipv4Address: Option[String] = None, ipLiteralAddress: Option[String] = None): Host = throw new UnsupportedOperationException

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit c: UriConfig): String =
    // TODO: According to the RFC section 3.2.2 paragraph 2, the `host` should use lower-case:
    // TODO: According to the RFC section 3.2.2 last paragraph, the `registeredName` should be percent encoded:
    registeredName.fold(ipv4Address.fold(ipLiteralAddress.get.toLowerCase)(ipv4Address => ipv4Address))(registeredName => c.hostEncoder.encode(registeredName.toLowerCase, c.charset))

  def toStringRaw(implicit c: UriConfig): String = host
}

object Host {

  private val REGISTERED_NAME_REGEX = """(?!\.\.)[^\.\[\]:/?#][^\[\]:/?#]{0,254}""".r // Cannot contain [\[\]:/?#]; For DNS host (but maybe not for everything else) cannot: start with '.'; contain "..";
  private val DECIMAL_OCTET = """(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)"""
  private val IPV4_ADDRESS = s"""$DECIMAL_OCTET(\\.$DECIMAL_OCTET){3}"""
  private val IPV4_ADDRESS_REGEX = IPV4_ADDRESS.r
  private val HEX_DIGIT = """[a-fA-F0-9]"""
  private val HEXTET = s"""$HEX_DIGIT{1,4}"""
  private val IPV6_ADDRESS_REGEX = (s"""\\[(($HEXTET:){7}$HEXTET|""" +
                                s"""($HEXTET:){6}$IPV4_ADDRESS|""" +
                              s"""::($HEXTET:){6}$HEXTET|""" +
                              s"""::($HEXTET:){5}$IPV4_ADDRESS|""" +
                    s"""($HEXTET)?::($HEXTET:){5}$HEXTET|""" +
                    s"""($HEXTET)?::($HEXTET:){4}$IPV4_ADDRESS|""" +
         s"""($HEXTET(:$HEXTET)?)?::($HEXTET:){4}$HEXTET|""" +
         s"""($HEXTET(:$HEXTET)?)?::($HEXTET:){3}$IPV4_ADDRESS|""" +
     s"""($HEXTET(:$HEXTET){0,2})?::($HEXTET:){3}$HEXTET|""" +
     s"""($HEXTET(:$HEXTET){0,2})?::($HEXTET:){2}$IPV4_ADDRESS|""" +
     s"""($HEXTET(:$HEXTET){0,3})?::($HEXTET:){2}$HEXTET|""" +
     s"""($HEXTET(:$HEXTET){0,3})?::$HEXTET:$IPV4_ADDRESS|""" +
     s"""($HEXTET(:$HEXTET){0,4})?::$HEXTET:$HEXTET|""" +
     s"""($HEXTET(:$HEXTET){0,4})?::$IPV4_ADDRESS|""" +
     s"""($HEXTET(:$HEXTET){0,5})?::$HEXTET|""" +
     s"""($HEXTET(:$HEXTET){0,6})?::)\\]""").r
  private val SUB_DELIMS = """!$&'()*+,;="""
  private val UNRESERVED = """a-zA-Z0-9\-\._~"""
  val IPVFUTURE_ADDRESS_REGEX = s"""\\[[vV]$HEX_DIGIT+\\.[$UNRESERVED$SUB_DELIMS:]+\\]""".r

  def apply(registeredName: Option[String], ipv4Address: Option[String], ipLiteralAddress: Option[String]): Host = {
    if ((registeredName == null || registeredName.isEmpty) && (ipv4Address == null || ipv4Address.isEmpty) && (ipLiteralAddress == null || ipLiteralAddress.isEmpty)) throw new IllegalArgumentException("`registeredName`, `ipv4Address` or `ipLiteralAddress` must be provided.")
    registeredName match {
      case None =>
      case Some(registeredName) =>
        if (ipv4Address.nonEmpty || ipLiteralAddress.nonEmpty) throw new IllegalArgumentException("Only 1 of `registeredName`, `ipv4Address` and `ipLiteralAddress` are permitted.")
        registeredName match {
          case IPV4_ADDRESS_REGEX(_*) => throw new IllegalArgumentException("`registeredName` is an `ipv4Address`.") // TODO: Do you think this should be here? What about blocking `ipFuture`s?
          case REGISTERED_NAME_REGEX(_*) =>
          case _ => throw new IllegalArgumentException("`registeredName` is not valid.")
        }
    }
    ipv4Address match {
      case None =>
      case Some(ipv4Address) =>
        if (ipLiteralAddress.nonEmpty) throw new IllegalArgumentException("Only 1 of `registeredName`, `ipv4Address` and `ipLiteralAddress` are permitted.")
        ipv4Address match {
          case IPV4_ADDRESS_REGEX(_*) =>
          case _ => throw new IllegalArgumentException("`ipv4Address` is not valid.")
        }
    }
    ipLiteralAddress match {
      case None =>
      case Some(ipLiteralAddress) =>
        ipLiteralAddress match {
          case IPV6_ADDRESS_REGEX(_*) =>
          case IPVFUTURE_ADDRESS_REGEX(_*) =>
          case _ => throw new IllegalArgumentException("`ipLiteralAddress` is not valid.")
        }
    }
    new Host(registeredName, ipv4Address, ipLiteralAddress) {}
  }

  def apply(registeredName: String = null, ipv4Address: String = null, ipLiteralAddress: String = null): Host = apply(Option(registeredName), Option(ipv4Address), Option(ipLiteralAddress))

  def option(registeredName: Option[String], ipv4Address: Option[String], ipLiteralAddress: Option[String]): Option[Host] = {
    if ((registeredName == null || registeredName.isEmpty) && (ipv4Address == null || ipv4Address.isEmpty) && (ipLiteralAddress == null || ipLiteralAddress.isEmpty)) None
    else Option(apply(registeredName, ipv4Address, ipLiteralAddress))
  }

  def option(registeredName: String = null, ipv4Address: String = null, ipLiteralAddress: String = null): Option[Host] = option(Option(registeredName), Option(ipv4Address), Option(ipLiteralAddress))

  def parse(host: String): Option[Host] = {
    host match {
      case null => None
      case "" => None
      case IPVFUTURE_ADDRESS_REGEX(_*) => Some(new Host(None, None, Some(host)) {})
      case IPV6_ADDRESS_REGEX(_*) => Some(new Host(None, None, Some(host)) {})
      case IPV4_ADDRESS_REGEX(_*) => Some(new Host(None, Some(host), None) {})
      case REGISTERED_NAME_REGEX(_*) => Some(new Host(Some(host), None, None) {})
      case _ => throw new IllegalArgumentException("`host` is not valid.")
    }
  }
}
