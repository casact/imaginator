#' @title Claims by first report
#'
#' @description
#' Given a data frame of policies, this will simulate the number of claims- and their initial payment-
#' per policy by the development lag at which they are first reported.
#'
#' @param dfPolicy A policy data frame.
#' @param Frequency A list of the same length as `Lags` of number of claims per policy or their distributions.
#' @param PaymentSeverity A list of the same length as `Lags` of payment amount for each claim or their distributions.
#' @param Lags A vector of lags as integers.
#'
#' @details
#' Creates a data frame with randomly generated claim values.
#'
#' @return A claims data frame
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' # This will generate a claim data frame which has 1,000 records
#' # each of which has a severity of 100
#' dfPolicy <- NewPolicyYear(100, 2001)
#' dfClaims <- ClaimsByFirstReport(
#'                dfPolicy
#'              , Frequency = 10
#'              , PaymentSeverity = 100
#'              , Lags = 1)
#'
#' @export
ClaimsByFirstReport <- function(dfPolicy, Frequency, PaymentSeverity, Lags){

  if (missing(Frequency)) stop("Must supply Frequency.")
  if (missing(PaymentSeverity)) stop("Must supply PaymentSeverity.")

  numFreq <- length(Frequency)
  numSev <- length(PaymentSeverity)

  Frequency <- maybe_wrap_in_list(Frequency)
  PaymentSeverity <- maybe_wrap_in_list(PaymentSeverity)

  if (numFreq > numSev) {
    message("Frequency has more elements than severity. Recycling to accommodate.")
    indices <- rep_len(seq.int(numSev), length.out = numFreq)
    PaymentSeverity <- PaymentSeverity[indices]
    numSev <- length(PaymentSeverity)
  } else if (numFreq < numSev) {
    message("PaymentSeverity has more elements than frequency. Recycling to accommodate.")
    indices <- rep_len(seq.int(numFreq), length.out = numSev)
    Frequency <- Frequency[indices]
    numFreq <- length(Frequency)
  }

  if (numFreq != numSev) {
    stop("Something very strange has happened. Contact package maintainer for support.")
  }

  if (missing(Lags)) {
    Lags <- seq.int(numFreq)
  }

  numLags <- length(Lags)

  numPolicies <- nrow(dfPolicy)

  lstClaims <- vector("list", numLags)
  first_claim_id <- 1
  for (iLag in seq.int(numLags)){
    claimFrequencies <- sample_or_rep(Frequency[[iLag]], numPolicies)
    if (any(claimFrequencies < 0)){
      message("Some claim frequencies are negative. These will be set to zero.")
      claimFrequencies <- pmax(0, claimFrequencies)
    }
    claimFrequencies <- as.integer(claimFrequencies)
    totalClaims <- sum(claimFrequencies)
    claimIDs <- seq.int(from = first_claim_id, length.out = totalClaims)
    first_claim_id <- max(claimIDs) + 1

    policyholderIds <- mapply(rep, dfPolicy$PolicyholderID, claimFrequencies, SIMPLIFY = FALSE)
    policyholderIds <- do.call("c", policyholderIds)
    effectiveDates <- mapply(rep, dfPolicy$PolicyEffectiveDate, claimFrequencies, SIMPLIFY = FALSE)
    effectiveDates <- do.call("c", effectiveDates)

    severities <- sample_or_rep(PaymentSeverity[[iLag]], totalClaims)

    lstClaims[[iLag]] <- data.frame(PolicyholderID = policyholderIds
                                    , PolicyEffectiveDate = effectiveDates
                                    , ClaimID = claimIDs
                                    , Lag = Lags[iLag]
                                    , PaymentAmount = severities
                                    , stringsAsFactors = FALSE)
  }
  dfClaims <- do.call(rbind, lstClaims)
}
