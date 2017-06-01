#' @title Claims by link ratio
#'
#' @description
#' Given a data frame of claims, this will simulate claim development by applying a (possibly) random link ratio.
#'
#' @param dfClaims A claims data frame
#' @param Links A list of functions which dictate how severities change from one evaluation date to the next
#' @param Lags A vector of lags as integers
#'
#' @details
#' This function will apply the link ratio algorithm at an individual claim level.
#'
#' @return A claims data frame
#'
#' @importFrom magrittr %>%
#'
#' @export
ClaimsByLinkRatio <- function(dfClaims, Links, Lags){

  numLags <- length(Lags)

  # If we only have one evaluation date, then we don't need to loop. We'll
  # leave the function early rather than putting crazy controls on the loop
  if (numLags == 1){
    return(dfClaims)
  } else {
    lstEvals <- vector("list", numLags)
    lstEvals[[1]] <- dfClaims
  }

  for (iLink in seq.int(length(Links))){
    links <- Links[[iLink]](totalClaims)

    currSeverity <- currSeverity * links
    lstEvals[[iLink + 1]] <- data.frame(PolicyID = policyIds
                                        , PolicyEffectiveDate = effectiveDates
                                        , ClaimID = claimIDs
                                        , Lag = Lags[iLink + 1]
                                        , ClaimValue = currSeverity
                                        , stringsAsFactors = FALSE)
  }

  dfClaims <- do.call(rbind, lstEvals)

  dfClaims
}
