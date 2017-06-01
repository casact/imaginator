#' @title Claims by first report
#'
#' @description
#' Given a data frame of policies, this will simulate claims by applying a (possibly) random link ratio.
#'
#' @param dfPolicy A policy data frame
#' @param Frequency A function, or list of functions which will randomly generate the number of claims per policy
#' @param Severity A function, or list of functions which will randomly generate the severity of each claim
#' @param Lags A vector of lags as integers
#'
#' @details
#' Creates a data frame with randomly generated claim values
#'
#' @return A claims data frame
#'
#' @importFrom magrittr %>%
#'
#' @export
ClaimsByFirstReport <- function(dfPolicy, Frequency, Severity, Lags){

  if (missing(Frequency)) stop("Must supply Frequency.")
  if (missing(Severity)) stop("Must supply Severity.")

  numFreq <- length(Frequency)
  numSev <- length(Severity)
  if (numFreq > numSev){
    message("Frequency has more elements than severity. Recycling to accommodate.")
    indices <- rep_len(seq.int(numFreq), length.out = numSev)
    Frequency <- Frequency[indices]
  } else if (numFreq < numSev){
    message("Severity has more elements than frequency. Recycling to accommodate.")
    indices <- rep_len(seq.int(numSev), length.out = numFreq)
    Severity <- Severity[indices]
  }

  if (missing(Lags)) {
    Lags <- seq.int(numFreq)
  }

  numLags <- length(Lags)

  numPolicies <- nrow(dfPolicy)

  lstClaims <- vector("list", numLags)
  first_claim_id <- 1
  for (iLag in Lags){
    claimFrequencies <- Frequency[[iLag]](numPolicies)
    totalClaims <- sum(claimFrequencies)
    claimIDs <- seq.int(from = first_claim_id, length.out = totalClaims)
    first_claim_id <- max(claimIDs) + 1

    policyIds <- mapply(rep, dfPolicy$PolicyID, claimFrequencies, SIMPLIFY = FALSE)
    policyIds <- do.call("c", policyIds)
    effectiveDates <- mapply(rep, dfPolicy$PolicyEffectiveDate, claimFrequencies, SIMPLIFY = FALSE)
    effectiveDates <- do.call("c", effectiveDates)

    lstClaims[[iLag]] <- data.frame(PolicyID = policyIds
                                    , PolicyEffectiveDate = effectiveDates
                                    , ClaimID = claimIDs
                                    , Lag = Lags[iLag]
                                    , ClaimValue = Severity[[iLag]](totalClaims)
                                    , stringsAsFactors = FALSE)
  }
  dfClaims <- do.call(rbind, lstClaims)
}
