package com.netaporter.uri

import com.netaporter.uri.parsing.BaseParser.{IPV4_ADDRESS_HOST, IPV6_ADDRESS_HOST, IPVFUTURE_HOST}
import com.netaporter.uri.parsing.DomainNameParsing.DOMAIN_NAME_HOST

/**
 * URI Host, based on RFC 3986 section 3.2.2.
 *
 * @param registeredName the optional registered name
 * @param ipv4Address the optional IPv4 address
 * @param ipLiteral the optional IP literal
 */
sealed abstract case class Host(registeredName: Option[String], ipv4Address: Option[String], ipLiteral: Option[String]) {

  def hostString: String = registeredName.getOrElse(ipv4Address.getOrElse(ipLiteral.get))

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  def toString(implicit config: UriConfig): String = {
    (registeredName, ipv4Address, ipLiteral) match {
      case (Some(registeredName), _, _) if config.caseNormalization => config.registeredNameEncoder.encode(registeredName.toLowerCase)
      case (Some(registeredName), _, _) => config.registeredNameEncoder.encode(registeredName)
      case (_, Some(ipv4Address), _) => ipv4Address
      case (_, _, Some(ipLiteral)) if config.caseNormalization => ipLiteral.toLowerCase
      case (_, _, Some(ipLiteral)) => ipLiteral
    }
  }
}

object Host {

  def apply(registeredName: Option[String], ipv4Address: Option[String], ipLiteral: Option[String])(implicit config: UriConfig): Host = {
    // NOTE: `parse` bypasses this method as the checks are redundant.
    registeredName match {
      case null => throw new IllegalArgumentException("`registeredName` cannot be `null`.")
      case None =>
        ipv4Address match {
          case null => throw new IllegalArgumentException("`ipv4Address` cannot be `null`.")
          case None =>
            ipLiteral match {
              case null => throw new IllegalArgumentException("`ipLiteral` cannot be `null`.")
              case None => throw new IllegalArgumentException("`registeredName`, `ipv4Address` or `ipLiteral` must be provided.")
              case Some(IPV6_ADDRESS_HOST(_)) | Some(IPVFUTURE_HOST(_)) => // Valid IP literal
              case _ => throw new IllegalArgumentException("`ipLiteral` is not valid.")
            }
          case _ if ipLiteral.nonEmpty => throw new IllegalArgumentException("Only 1 of `registeredName`, `ipv4Address` and `ipLiteral` are permitted.")
          case Some(IPV4_ADDRESS_HOST(_)) => // Valid IPv4 address
          case _ => throw new IllegalArgumentException("`ipv4Address` is not valid.")
        }
      case _ if ipv4Address.nonEmpty || ipLiteral.nonEmpty => throw new IllegalArgumentException("Only 1 of `registeredName`, `ipv4Address` and `ipLiteral` are permitted.")
      case Some("") => throw new IllegalArgumentException("`registeredName` cannot be \"\".")
      case Some(IPV4_ADDRESS_HOST(_)) => throw new IllegalArgumentException("`registeredName` cannot be an `ipv4Address`.")
      case _ if !config.registeredNameMustBeDomainName => // Valid registered name
      case Some(DOMAIN_NAME_HOST(_)) => // Valid domain name
      case _ => throw new IllegalArgumentException("`registeredName` is not a valid domain name.")
    }
    new Host(registeredName, ipv4Address, ipLiteral) {}
  }

  def apply(registeredName: String = null, ipv4Address: String = null, ipLiteral: String = null)(implicit config: UriConfig): Host = apply(Option(registeredName), Option(ipv4Address), Option(ipLiteral))

  /** This is implemented ONLY so that the parser can bypass redundant validation. */
  private[uri] def fromParser(registeredName: String = null, ipv4Address: String = null, ipLiteral: String = null): Host = new Host(Option(registeredName), Option(ipv4Address), Option(ipLiteral)) {}

  def option(registeredName: Option[String], ipv4Address: Option[String], ipLiteral: Option[String])(implicit config: UriConfig): Option[Host] = {
    if (registeredName != null && registeredName.isEmpty && ipv4Address != null && ipv4Address.isEmpty && ipLiteral != null && ipLiteral.isEmpty) None
    else Option(apply(registeredName, ipv4Address, ipLiteral))
  }

  def option(registeredName: String = null, ipv4Address: String = null, ipLiteral: String = null)(implicit config: UriConfig): Option[Host] = option(Option(registeredName), Option(ipv4Address), Option(ipLiteral))

  def parse(hostString: String)(implicit config: UriConfig): Option[Host] = {
    // NOTE: Bypasses default `apply` as all cases are covered here.
    hostString match {
      case null | "" => None
      case IPVFUTURE_HOST(_) | IPV6_ADDRESS_HOST(_) => Some(new Host(None, None, Some(hostString)) {})
      case IPV4_ADDRESS_HOST(_) => Some(new Host(None, Some(hostString), None) {})
      case _ if !config.registeredNameMustBeDomainName => Some(new Host(Some(hostString), None, None) {})
      case DOMAIN_NAME_HOST(_) => Some(new Host(Some(hostString), None, None) {})
      case _ => throw new IllegalArgumentException("`hostString` is not valid.")
    }
  }
}
