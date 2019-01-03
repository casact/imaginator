#' @title Claims by first report
#'
#' @description
#' Given a data frame of policies, this will simulate the number of claims- and their initial payment-
#' per policy by the development lag at which they are first reported.
#'
#' @param dfPolicy A policy data frame
#' @param Frequency A function, or list of functions which will randomly generate the number of claims per policy
#' @param PaymentSeverity A function, or list of functions which will randomly generate the payment amount for each claim
#' @param Lags A vector of lags as integers
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
#'              , Frequency = FixedHelper(10)
#'              , PaymentSeverity = FixedHelper(100)
#'              , Lags = 1)
#'
#' @export
ClaimsByFirstReport <- function(dfPolicy, Frequency, PaymentSeverity, Lags){

  if (missing(Frequency)) stop("Must supply Frequency.")
  if (missing(PaymentSeverity)) stop("Must supply PaymentSeverity.")

  numFreq <- length(Frequency)
  numSev <- length(PaymentSeverity)

  if (numFreq == 1) Frequency <- PutFunctionInList(Frequency)
  if (numSev == 1) PaymentSeverity <- PutFunctionInList(PaymentSeverity)

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
    claimFrequencies <- Frequency[[iLag]](numPolicies)
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

    severities <- PaymentSeverity[[iLag]](totalClaims)

    lstClaims[[iLag]] <- data.frame(PolicyholderID = policyholderIds
                                    , PolicyEffectiveDate = effectiveDates
                                    , ClaimID = claimIDs
                                    , Lag = Lags[iLag]
                                    , PaymentAmount = severities
                                    , stringsAsFactors = FALSE)
  }
  dfClaims <- do.call(rbind, lstClaims)
}
