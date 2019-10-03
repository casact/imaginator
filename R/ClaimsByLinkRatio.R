#' @title Claims by link ratio
#'
#' @description
#' Given a data frame of claims, this will simulate claim development by applying a (possibly) random link ratio.
#'
#' @param dfClaims A claims data frame
#' @param Links A vector of the same length as `Lags` of factors, or their
#'   distributions, determining how severities change from one evaluation date
#'   to the next.
#' @param Lags A vector of lags
#'
#' @details
#' This function will apply the link ratio algorithm at an individual claim level.
#'
#' @return A claims data frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
#' @examples
#'
#' dfPolicy <- NewPolicyYear(10, 2001)
#' dfClaims <- ClaimsByFirstReport(
#'                dfPolicy
#'              , Frequency = 10
#'              , PaymentSeverity = 100
#'              , Lags = 1)
#' dfClaims <- ClaimsByLinkRatio(dfClaims
#'                               , Links = c(1.25, 1.1, 1.05)
#'                               , Lags = 1:4)
#'
#' @export
ClaimsByLinkRatio <- function(dfClaims, Links, Lags){

  Links <- maybe_wrap_in_list(Links)
  for (iLink in seq.int(length(Links))){

    dfNextLag <- dfClaims[dfClaims$Lag == Lags[iLink], ]

    # samplingx
    links <- sample_or_rep(Links[[iLink]], nrow(dfNextLag))

    dfNextLag$PaymentAmount <- dfNextLag$PaymentAmount * links
    dfNextLag$Lag <- Lags[iLink + 1]

    dfClaims <- rbind(dfClaims, dfNextLag)
  }

  dfClaims
}
