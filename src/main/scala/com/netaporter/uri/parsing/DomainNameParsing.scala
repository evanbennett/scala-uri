package com.netaporter.uri.parsing

import fastparse.all._

/**
 * Modify `BaseParser` to only allow registered names that are domain names.
 *
 * RFC 3986 section 3.2.2 states "The most common name registry mechanism is the Domain Name System (DNS)."
 * Therefore, this has been implemented.
 */
trait DomainNameParsing extends BaseParser {

  protected override val registeredName: P[String] = DomainNameParsing.DOMAIN_NAME
}

object DomainNameParsing {

  private val ALPHA_NUMERIC = CharIn('a' to 'z', 'A' to 'Z', '0' to '9')
  private val ALPHA_NUMERIC_HYPHEN = CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "-")

  private val DOMAIN_NAME: P[String] = P(((ALPHA_NUMERIC ~ ALPHA_NUMERIC_HYPHEN.rep.?).!.filter(domainLabel =>  ALPHA_NUMERIC.unapply(domainLabel.last.toString).nonEmpty)).rep(1, ".") ~ ".".?.! map { case (labels, lastDot) => labels.mkString(".") + lastDot })

  // The following is provided ONLY for `Host` validation:
  private[uri] val DOMAIN_NAME_HOST = P(DOMAIN_NAME ~ End)
}
