#' @title Claims by link ratio
#'
#' @description
#' Given a data frame of claims, this will simulate claim development by applying a (possibly) random link ratio.
#'
#' @param dfClaims A claims data frame
#' @param Links A list of functions which dictate how severities change from one evaluation date to the next
#' @param Lags A list of functions which dictate how severities change from one evaluation date to the next
#'
#' @details
#' This function will apply the link ratio algorithm at an individual claim level.
#'
#' @return A claims data frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
#' @export
ClaimsByLinkRatio <- function(dfClaims, Links, Lags){

  numLinks <- length(Links)

  for (iLink in seq.int(length(Links))){

    dfNextLag <- dfClaims %>%
      filter(Lag == Lags[iLink + 1])

    links <- Links[[iLink]](nrow(dfNextLag))

    dfNextLag$ClaimValue <- dfNextLag$ClaimValue * links

    dfClaims <- rbind(dfClaims, dfNextLag)
  }

  dfClaims
}
