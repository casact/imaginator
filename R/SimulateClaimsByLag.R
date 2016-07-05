#' @title Claims by lag
#'
#' @description
#' Given a data frame of policies, this will simulate claims
#'
#' @param dfPolicy A policy data frame
#' @param Frequency A function which will randomly generate number of claims
#' @param Severity A function which will randomly generate the severity of each claim
#' @param Links A list of functions which dictates how severities change from one evaluation date to the next
#' @param Lags A vector of lags
#'
#' @details
#' The claim amounts are presumed to be cumulative
#'
#' @export
#'
#' @return A claims data frame
#'
#' @importFrom magrittr %>%
#'
ClaimsByLag <- function(dfPolicy, Frequency, Severity, Links, Lags){

  numLags <- length(Lags)

  numPolicies <- nrow(dfPolicy)
  claimFrequencies <- Frequency(numPolicies)
  totalClaims <- sum(claimFrequencies)
  claimIDs <- seq.int(totalClaims)

  policyIds <- mapply(rep, dfPolicy$PolicyID, claimFrequencies) %>% unlist()
  currSeverity <- Severity(totalClaims)

  dfClaims <- data.frame(PolicyID = policyIds
                         , ClaimID = claimIDs
                         , Lag = Lags[1]
                         , ClaimValue = currSeverity
                         , stringsAsFactors = FALSE)

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
                                        , ClaimID = claimIDs
                                    , Lag = Lags[iLink + 1]
                                    , ClaimValue = currSeverity
                                    , stringsAsFactors = FALSE)
  }

  # emptyFrames <- sapply(lstClaims, is.null)
  # lstClaims <- lstClaims[!emptyFrames]
  dfClaims <- do.call(rbind, lstEvals)

  # lstReturn <- list(Policies = dfPolicy
  #                   , Claims = dfClaims)
  #
  # lstReturn

  dfClaims
}
