package com.netaporter.uri.parsing

import com.netaporter.uri._
import fastparse.all._

trait QueryParameterParsing extends BaseParser {

  protected val extractParameterQuery: Seq[Parameter] => Query = (parameters: Seq[Parameter]) =>
    Query(parameters.map(config.queryDecoder.decodeParameter(_, input)))

  protected val queryParameter: P[Parameter] =
    P(QUERY_PARAMETER_KEY.rep(1).! ~ ("=" ~ QUERY_PARAMETER_VALUE.rep.!).? map extractParameter)

  // THEON: Implemented empty query parameter removal, consistent with matrix parameters. Updated GithubIssueTests #65 examples 1, 4, 5, and 6.
  protected override val query: P[Query] =
    P("?" ~ "&".rep ~ queryParameter.rep(sep = "&".rep(1)) ~ "&".rep map extractParameterQuery)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  protected val QUERY_PARAMETER_KEY: P[Unit] = QueryParameterParsing.QUERY_PARAMETER_KEY
  protected val QUERY_PARAMETER_VALUE: P[Unit] = QueryParameterParsing.QUERY_PARAMETER_VALUE
}

object QueryParameterParsing {

  private val QUERY_PARAMETER_KEY = CharIn((encoding.PercentEncoder.RfcCharsets.QUERY - ('&', '=')).toSeq)
  private val QUERY_PARAMETER_VALUE = CharIn((encoding.PercentEncoder.RfcCharsets.QUERY - ('&')).toSeq)
}
