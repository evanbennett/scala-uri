package com.netaporter.uri

class DomainNameTests extends TestSpec {

  implicit val config = UriConfig(registeredNameMustBeDomainName = true)

  "Parsing a domaind name with `UriConfig(registeredNameMustBeDomainName = true)`" should "succeed with a single label with no '-'s" in {
    val uri = Uri("//label")
    uri.authority.value.host.value.registeredName.value should equal("label")
  }

  it should "succeed with a full domain name with no '-'s" in {
    val uri = Uri("//subdomain.domain.com.au")
    uri.authority.value.host.value.registeredName.value should equal("subdomain.domain.com.au")
  }

  it should "succeed with a local domain name with one label and no '-'s" in {
    val uri = Uri("//localsubdomain.")
    uri.authority.value.host.value.registeredName.value should equal("localsubdomain.")
  }

  it should "succeed with a local domain name with multiple labels and no '-'s" in {
    val uri = Uri("//localsubdomain.localsubdomain.")
    uri.authority.value.host.value.registeredName.value should equal("localsubdomain.localsubdomain.")
  }

  it should "fail with only a label separator" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//.")
    }
  }

  it should "fail starting with a label separator" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//.label")
    }
  }

  it should "fail starting with a double label separator" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//..label")
    }
  }

  it should "fail with an empty label in the middle" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//label..label")
    }
  }

  it should "fail with an empty label at the end of a local domain name" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//label..")
    }
  }

  it should "succeed with a single label with a '-'" in {
    val uri = Uri("//la-bel")
    uri.authority.value.host.value.registeredName.value should equal("la-bel")
  }

  it should "succeed with a single label with multiple '-'s" in {
    val uri = Uri("//label-with-multiple-hyphens")
    uri.authority.value.host.value.registeredName.value should equal("label-with-multiple-hyphens")
  }

  it should "succeed with a full domain name with a '-' in the domain" in {
    val uri = Uri("//subdomain.do-main.com.au")
    uri.authority.value.host.value.registeredName.value should equal("subdomain.do-main.com.au")
  }

  it should "succeed with a full domain name with a '-' in the first label" in {
    val uri = Uri("//sub-domain.domain.com.au")
    uri.authority.value.host.value.registeredName.value should equal("sub-domain.domain.com.au")
  }

  it should "succeed with a full domain name with a '-' in the first label and domain" in {
    val uri = Uri("//sub-domain.do-main.com.au")
    uri.authority.value.host.value.registeredName.value should equal("sub-domain.do-main.com.au")
  }

  it should "succeed with a full domain name with multiple '-'s in the subdomain and domain (including a double '-')" in {
    val uri = Uri("//sub-do-main.do--main.com.au")
    uri.authority.value.host.value.registeredName.value should equal("sub-do-main.do--main.com.au")
  }

  it should "succeed with a local domain name with one label and a '-'" in {
    val uri = Uri("//local-subdomain.")
    uri.authority.value.host.value.registeredName.value should equal("local-subdomain.")
  }

  it should "succeed with a local domain name with multiple labels and a '-' in the first label" in {
    val uri = Uri("//local-subdomain.localsubdomain.")
    uri.authority.value.host.value.registeredName.value should equal("local-subdomain.localsubdomain.")
  }

  it should "succeed with a local domain name with multiple labels and a '-' in the last label" in {
    val uri = Uri("//localsubdomain.local-subdomain.")
    uri.authority.value.host.value.registeredName.value should equal("localsubdomain.local-subdomain.")
  }

  it should "succeed with a local domain name with multiple labels and a '-' in each label" in {
    val uri = Uri("//local-subdomain.local-subdomain.")
    uri.authority.value.host.value.registeredName.value should equal("local-subdomain.local-subdomain.")
  }

  it should "succeed with a local domain name with multiple labels and multiple '-'s in all labels" in {
    val uri = Uri("//local--sub-domain.local-sub-domain.")
    uri.authority.value.host.value.registeredName.value should equal("local--sub-domain.local-sub-domain.")
  }

  it should "fail with only a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//-")
    }
  }

  it should "fail with only 2 '-'s" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//--")
    }
  }

  it should "fail with only more than 2 '-'s" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//----")
    }
  }

  it should "fail with a single label starting with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//-label")
    }
  }

  it should "fail with a single label starting with 2 '-'s" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//--label")
    }
  }

  it should "fail with a single label ending with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//label-")
    }
  }

  it should "fail with a single label ending with 2 '-'s" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//label--")
    }
  }
//**********************************************************************************************
  it should "fail with a full domain name with the first label starting with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//-subdomain.domain.com.au")
    }
  }

  it should "fail with a full domain name with the first label starting with 2 '-'s" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//--subdomain.domain.com.au")
    }
  }

  it should "fail with a full domain name with the first label ending with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//subdomain-.domain.com.au")
    }
  }

  it should "fail with a full domain name with the first label ending with 2 '-'s" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//subdomain--.domain.com.au")
    }
  }

  it should "fail with a full domain name with the first label starting, containing and ending with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//-sub-domain-.domain.com.au")
    }
  }

  it should "fail with a full domain name with the second label starting with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//subdomain.-domain.com.au")
    }
  }

  it should "fail with a full domain name with the second label starting with 2 '-'s" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//subdomain.--domain.com.au")
    }
  }

  it should "fail with a full domain name with the second label ending with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//subdomain.domain-.com.au")
    }
  }

  it should "fail with a full domain name with the second label starting, containing and ending with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//subdomain.-do-main-.com.au")
    }
  }

  it should "fail with a full domain name with the first and second labels starting and ending with '-'s" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//-subdomain-.-domain-.com.au")
    }
  }

  it should "fail with a full domain name with the first and second labels starting, containing  and ending with '-'s" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//-sub-domain-.-do-main-.com.au")
    }
  }

  it should "fail with a full domain name with the last label ending with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//subdomain.domain.com.au-")
    }
  }

  it should "fail with a local domain name with one label starting with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//-localsubdomain.")
    }
  }

  it should "fail with a local domain name with one label starting with 2 '-'s" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//--localsubdomain.")
    }
  }

  it should "fail with a local domain name with one label ending with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//localsubdomain-.")
    }
  }

  it should "fail with a local domain name with one label ending with 2 '-'s" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//localsubdomain--.")
    }
  }

  it should "fail with a local domain name with one label starting, containing and ending with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//-local-sub-domain-.")
    }
  }

//**********************************************************************************************
  it should "fail with a local domain name with multiple labels and the first label starting with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//-localsubdomain.localsubdomain.")
    }
  }

  it should "fail with a local domain name with multiple labels and the first label ending with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//localsubdomain-.localsubdomain.")
    }
  }

  it should "fail with a local domain name with multiple labels and the first label starting and ending with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//-localsubdomain-.localsubdomain.")
    }
  }

  it should "fail with a local domain name with multiple labels and the second label starting with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//localsubdomain.-localsubdomain.")
    }
  }

  it should "fail with a local domain name with multiple labels and the second label ending with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//localsubdomain.localsubdomain-.")
    }
  }

  it should "fail with a local domain name with multiple labels and the second label starting and ending with a '-'" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//localsubdomain.-localsubdomain-.")
    }
  }

  it should "fail with a local domain name with multiple labels and all labels starting and ending with '-'s" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//-localsubdomain-.-localsubdomain-.")
    }
  }

  it should "fail with a local domain name with multiple labels and all labels starting, containing and ending with '-'s" in {
    a [java.net.URISyntaxException] should be thrownBy {
      Uri("//-local--sub-domain-.-local-sub-domain-.")
    }
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  "`Host.apply` with `Option` arguments" should "succeed when passed a registered name that is not a domain name when not required to (start with \".\")" in {
    implicit val config = UriConfig(registeredNameMustBeDomainName = false)
    Host(Some(".domain.co.uk"), None, None).registeredName.value should equal(".domain.co.uk")
  }

  it should "succeed when passed a registered name that is not a domain name when not required to (contain \"..\")" in {
    implicit val config = UriConfig(registeredNameMustBeDomainName = false)
    Host(Some("subdomain..domain.com.au"), None, None).registeredName.value should equal("subdomain..domain.com.au")
  }

  it should "fail when passed a registered name that is not a domain name when required to (start with \".\")" in {
    an [IllegalArgumentException] should be thrownBy {
      Host(Some(".domain.co.uk"), None, None)
    }
  }

  it should "fail when passed a registered name that is not a domain name when required to (contain \"..\")" in {
    an [IllegalArgumentException] should be thrownBy {
      Host(Some("subdomain..domain.com.au"), None, None)
    }
  }

  "`Host.parse`" should "succeed when passed a registered name that is not a domain name when not required to (start with \".\")" in {
    implicit val config = UriConfig(registeredNameMustBeDomainName = false)
    Host.parse(".domain.co.uk").value.registeredName.value should equal(".domain.co.uk")
  }

  it should "succeed when passed a registered name that is not a domain name when not required to (contain \"..\")" in {
    implicit val config = UriConfig(registeredNameMustBeDomainName = false)
    Host.parse("subdomain..domain.com.au").value.registeredName.value should equal("subdomain..domain.com.au")
  }

  it should "fail when passed a registered name that is not a domain name when required to (start with \".\")" in {
    an [IllegalArgumentException] should be thrownBy {
      Host.parse(".domain.co.uk")
    }
  }

  it should "fail when passed a registered name that is not a domain name when required to (contain \"..\")" in {
    an [IllegalArgumentException] should be thrownBy {
      Host.parse("subdomain..domain.com.au")
    }
  }
}
