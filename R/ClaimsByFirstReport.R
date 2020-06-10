# This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
# If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' @title Claims by first report
#'
#' @description
#' Given a data frame of policies, this will simulate the number of claims- and their initial payment-
#' per policy by the development lag at which they are first reported.
#'
#' @param tbl_policy A policy data frame.
#' @param frequency A list of the same length as `lags` of number of claims per policy or their distributions.
#' @param payment_severity A list of the same length as `lags` of payment amount for each claim or their distributions.
#' @param lags A vector of lags as integers.
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
#' tbl_policy <- policy_year_new(100, 2001)
#' tbl_claims <- claims_by_first_report(
#'                tbl_policy,
#'                frequency = 10,
#'                payment_severity = 100,
#'                lags = 1)
#'
#' @export
claims_by_first_report <- function(tbl_policy, frequency, payment_severity, lags){

  if (missing(frequency)) stop("Must supply frequency.")
  if (missing(payment_severity)) stop("Must supply payment_severity.")

  numFreq <- length(frequency)
  numSev <- length(payment_severity)

  frequency <- maybe_wrap_in_list(frequency)
  payment_severity <- maybe_wrap_in_list(payment_severity)

  if (numFreq > numSev) {
    message("frequency has more elements than severity. Recycling to accommodate.")
    indices <- rep_len(seq.int(numSev), length.out = numFreq)
    payment_severity <- payment_severity[indices]
    numSev <- length(payment_severity)
  } else if (numFreq < numSev) {
    message("payment_severity has more elements than frequency. Recycling to accommodate.")
    indices <- rep_len(seq.int(numFreq), length.out = numSev)
    frequency <- frequency[indices]
    numFreq <- length(frequency)
  }

  if (numFreq != numSev) {
    stop("Something very strange has happened. Contact package maintainer for support.")
  }

  if (missing(lags)) {
    lags <- seq.int(numFreq)
  }

  num_lags <- length(lags)

  numPolicies <- nrow(tbl_policy)

  lstClaims <- vector("list", num_lags)
  first_claim_id <- 1
  for (iLag in seq.int(num_lags)) {
    claimFrequencies <- sample_or_rep(frequency[[iLag]], numPolicies)
    if (any(claimFrequencies < 0)) {
      message("Some claim frequencies are negative. These will be set to zero.")
      claimFrequencies <- pmax(0, claimFrequencies)
    }
    claimFrequencies <- as.integer(claimFrequencies)
    totalClaims <- sum(claimFrequencies)
    claimIDs <- seq.int(from = first_claim_id, length.out = totalClaims)
    first_claim_id <- max(claimIDs) + 1

    policyholderIds <- mapply(rep, tbl_policy$policyholder_id, claimFrequencies, SIMPLIFY = FALSE)
    policyholderIds <- do.call("c", policyholderIds)
    effectiveDates <- mapply(rep, tbl_policy$policy_effective_date, claimFrequencies, SIMPLIFY = FALSE)
    effectiveDates <- do.call("c", effectiveDates)

    severities <- sample_or_rep(payment_severity[[iLag]], totalClaims)

    lstClaims[[iLag]] <- data.frame(policyholder_id = policyholderIds
                                    , policy_effective_date = effectiveDates
                                    , claim_id = claimIDs
                                    , lag = lags[iLag]
                                    , payment_amount = severities
                                    , stringsAsFactors = FALSE)
  }

  tbl_claims <- do.call(rbind, lstClaims)

  tbl_claims

}
