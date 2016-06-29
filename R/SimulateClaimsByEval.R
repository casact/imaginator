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
#' @param Link A function which dictates how severities change from one evaluation date to the next
#' @param EvaluationDates A vector of evaluation dates
#'
#' @details
#' Each function
#'
#' @export
#'
#' @return A list with revised policy table and a new claims table
ClaimsByEvaluationDate <- function(dfPolicy, Frequency, Severity, Link, EvaluationDates){

  numPolicies <- nrow(dfPolicy)
  dfPolicy$Claims <- Frequency(nrow(dfPolicy))

  lstClaims <- vector("list", numPolicies)
  for (iPolicy in seq.int(numPolicies)){
    numClaims <- dfPolicy$Claims[iPolicy]

    if (numClaims > 0){
      initSeverity <- Severity(numClaims)

      lstClaims[[iPolicy]] <- data.frame(PolicyID = dfPolicy$PolicyID[iPolicy]
                                         , ClaimValue = initSeverity
                                         , EvaluationDate = EvaluationDates[1]
                                         , stringsAsFactors = FALSE)
    }

  }

  emptyFrames <- sapply(lstClaims, is.null)
  lstClaims <- lstClaims[!emptyFrames]
  dfClaims <- do.call(rbind, lstClaims)

  lstReturn <- list(Policies = dfPolicy
                    , Claims = dfClaims)

  lstReturn

}
