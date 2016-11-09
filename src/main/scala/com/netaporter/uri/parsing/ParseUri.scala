package com.netaporter.uri.parsing

import com.netaporter.uri.{ Uri, UriConfig }

object ParseUri {

  def apply(string: String)(implicit config: UriConfig): Uri = {
    parser(string).parseUri match {
      case fastparse.core.Parsed.Success(uri, _) => uri
      case fastparse.core.Parsed.Failure(_, index, extra) => throw new java.net.URISyntaxException(string, "Invalid URI could not be parsed. " + extra.traced.fullStack, index)
    }
  }

  def option(string: String)(implicit config: UriConfig): Option[Uri] = {
    parser(string).parseUri match {
      case fastparse.core.Parsed.Success(uri, _) => Option(uri)
      case fastparse.core.Parsed.Failure(_, _, _) => None
    }
  }

  private def parser(string: String)(implicit config: UriConfig): BaseParser = {
    (config.delimiterParsing, config.userPasswordParsing, config.registeredNameMustBeDomainName, config.matrixParameterParsing, config.queryParameterParsing, config.fragmentAllowHashParsing) match {
      case (false, false, false, false, false, false) => new BaseParser(string)
      case (false, false, false, false, false, true) => new BaseParser(string) with FragmentAllowHashParsing
      case (false, false, false, false, true, false) => new BaseParser(string) with QueryParameterParsing
      case (false, false, false, false, true, true) => new BaseParser(string) with QueryParameterParsing with FragmentAllowHashParsing
      case (false, false, false, true, false, false) => new BaseParser(string) with MatrixParameterParsing
      case (false, false, false, true, false, true) => new BaseParser(string) with MatrixParameterParsing with FragmentAllowHashParsing
      case (false, false, false, true, true, false) => new BaseParser(string) with MatrixParameterParsing with QueryParameterParsing
      case (false, false, false, true, true, true) => new BaseParser(string) with MatrixParameterParsing with QueryParameterParsing with FragmentAllowHashParsing
      case (false, false, true, false, false, false) => new BaseParser(string) with DomainNameParsing
      case (false, false, true, false, false, true) => new BaseParser(string) with DomainNameParsing with FragmentAllowHashParsing
      case (false, false, true, false, true, false) => new BaseParser(string) with DomainNameParsing with QueryParameterParsing
      case (false, false, true, false, true, true) => new BaseParser(string) with DomainNameParsing with QueryParameterParsing with FragmentAllowHashParsing
      case (false, false, true, true, false, false) => new BaseParser(string) with DomainNameParsing with MatrixParameterParsing
      case (false, false, true, true, false, true) => new BaseParser(string) with DomainNameParsing with MatrixParameterParsing with FragmentAllowHashParsing
      case (false, false, true, true, true, false) => new BaseParser(string) with DomainNameParsing with MatrixParameterParsing with QueryParameterParsing
      case (false, false, true, true, true, true) => new BaseParser(string) with DomainNameParsing with MatrixParameterParsing with QueryParameterParsing with FragmentAllowHashParsing
      case (false, true, false, false, false, false) => new BaseParser(string) with UserPasswordParsing
      case (false, true, false, false, false, true) => new BaseParser(string) with UserPasswordParsing with FragmentAllowHashParsing
      case (false, true, false, false, true, false) => new BaseParser(string) with UserPasswordParsing with QueryParameterParsing
      case (false, true, false, false, true, true) => new BaseParser(string) with UserPasswordParsing with QueryParameterParsing with FragmentAllowHashParsing
      case (false, true, false, true, false, false) => new BaseParser(string) with UserPasswordParsing with MatrixParameterParsing
      case (false, true, false, true, false, true) => new BaseParser(string) with UserPasswordParsing with MatrixParameterParsing with FragmentAllowHashParsing
      case (false, true, false, true, true, false) => new BaseParser(string) with UserPasswordParsing with MatrixParameterParsing with QueryParameterParsing
      case (false, true, false, true, true, true) => new BaseParser(string) with UserPasswordParsing with MatrixParameterParsing with QueryParameterParsing with FragmentAllowHashParsing
      case (false, true, true, false, false, false) => new BaseParser(string) with UserPasswordParsing with DomainNameParsing
      case (false, true, true, false, false, true) => new BaseParser(string) with UserPasswordParsing with DomainNameParsing with FragmentAllowHashParsing
      case (false, true, true, false, true, false) => new BaseParser(string) with UserPasswordParsing with DomainNameParsing with QueryParameterParsing
      case (false, true, true, false, true, true) => new BaseParser(string) with UserPasswordParsing with DomainNameParsing with QueryParameterParsing with FragmentAllowHashParsing
      case (false, true, true, true, false, false) => new BaseParser(string) with UserPasswordParsing with DomainNameParsing with MatrixParameterParsing
      case (false, true, true, true, false, true) => new BaseParser(string) with UserPasswordParsing with DomainNameParsing with MatrixParameterParsing with FragmentAllowHashParsing
      case (false, true, true, true, true, false) => new BaseParser(string) with UserPasswordParsing with DomainNameParsing with MatrixParameterParsing with QueryParameterParsing
      case (false, true, true, true, true, true) => new BaseParser(string) with UserPasswordParsing with DomainNameParsing with MatrixParameterParsing with QueryParameterParsing with FragmentAllowHashParsing
      case (true, false, false, false, false, _) => new BaseParser(string) with DelimiterParsing
      case (true, false, false, false, true, _) => new BaseParser(string) with DelimiterParsing with QueryParameterParsing with QueryParameterWithDelimiterParsing
      case (true, false, false, true, false, _) => new BaseParser(string) with DelimiterParsing with MatrixParameterParsing with MatrixParameterWithDelimiterParsing
      case (true, false, false, true, true, _) => new BaseParser(string) with DelimiterParsing with MatrixParameterParsing with MatrixParameterWithDelimiterParsing with QueryParameterParsing with QueryParameterWithDelimiterParsing
      case (true, false, true, false, false, _) => new BaseParser(string) with DelimiterParsing with DomainNameParsing
      case (true, false, true, false, true, _) => new BaseParser(string) with DelimiterParsing with DomainNameParsing with QueryParameterParsing with QueryParameterWithDelimiterParsing
      case (true, false, true, true, false, _) => new BaseParser(string) with DelimiterParsing with DomainNameParsing with MatrixParameterParsing with MatrixParameterWithDelimiterParsing
      case (true, false, true, true, true, _) => new BaseParser(string) with DelimiterParsing with DomainNameParsing with MatrixParameterParsing with MatrixParameterWithDelimiterParsing with QueryParameterParsing with QueryParameterWithDelimiterParsing
      case (true, true, false, false, false, _) => new BaseParser(string) with DelimiterParsing with UserPasswordParsing with UserPasswordWithDelimiterParsing
      case (true, true, false, false, true, _) => new BaseParser(string) with DelimiterParsing with UserPasswordParsing with UserPasswordWithDelimiterParsing with QueryParameterParsing with QueryParameterWithDelimiterParsing
      case (true, true, false, true, false, _) => new BaseParser(string) with DelimiterParsing with UserPasswordParsing with UserPasswordWithDelimiterParsing with MatrixParameterParsing with MatrixParameterWithDelimiterParsing
      case (true, true, false, true, true, _) => new BaseParser(string) with DelimiterParsing with UserPasswordParsing with UserPasswordWithDelimiterParsing with MatrixParameterParsing with MatrixParameterWithDelimiterParsing with QueryParameterParsing with QueryParameterWithDelimiterParsing
      case (true, true, true, false, false, _) => new BaseParser(string) with DelimiterParsing with UserPasswordParsing with UserPasswordWithDelimiterParsing with DomainNameParsing
      case (true, true, true, false, true, _) => new BaseParser(string) with DelimiterParsing with UserPasswordParsing with UserPasswordWithDelimiterParsing with DomainNameParsing with QueryParameterParsing with QueryParameterWithDelimiterParsing
      case (true, true, true, true, false, _) => new BaseParser(string) with DelimiterParsing with UserPasswordParsing with UserPasswordWithDelimiterParsing with DomainNameParsing with MatrixParameterParsing with MatrixParameterWithDelimiterParsing
      case (true, true, true, true, true, _) => new BaseParser(string) with DelimiterParsing with UserPasswordParsing with UserPasswordWithDelimiterParsing with DomainNameParsing with MatrixParameterParsing with MatrixParameterWithDelimiterParsing with QueryParameterParsing with QueryParameterWithDelimiterParsing
    }
  }
}
