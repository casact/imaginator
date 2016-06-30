EmptyClaimFrame <- function(){

}

#' @title Claims by evaluation date
#'
#' @description
#' Given a data frame of policies, this will simulate claims
#'
#' @param dfPolicy A policy data frame
#' @param Frequency A function which will randomly generate number of claims
#' @param Severity A function which will randomly generate the severity of each claim
#' @param Links A list of functions which dictates how severities change from one evaluation date to the next
#' @param EvaluationDates A vector of evaluation dates
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
ClaimsByEvaluationDate <- function(dfPolicy, Frequency, Severity, Links, EvaluationDates){

  numEvals <- length(EvaluationDates)

  numPolicies <- nrow(dfPolicy)
  claimFrequencies <- Frequency(numPolicies)
  totalClaims <- sum(claimFrequencies)

  policyIds <- mapply(rep, dfPolicy$PolicyID, claimFrequencies) %>% unlist()
  currSeverity <- Severity(totalClaims)

  dfClaims <- data.frame(PolicyID = policyIds
                         , EvaluationDate = EvaluationDates[1]
                         , ClaimValue = currSeverity
                         , stringsAsFactors = FALSE)

  # If we only have one evaluation date, then we don't need to loop. We'll
  # leave the function early rather than putting crazy controls on the loop
  if (numEvals == 1){
    return(dfClaims)
  } else {
    lstEvals <- vector("list", numEvals)
    lstEvals[[1]] <- dfClaims
  }

  for (iLink in seq.int(length(Links))){
    links <- Links[[iLink]](totalClaims)

    currSeverity <- currSeverity * links
    lstEvals[[iLink + 1]] <- data.frame(PolicyID = policyIds
                                    , EvaluationDate = EvaluationDates[iLink + 1]
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
