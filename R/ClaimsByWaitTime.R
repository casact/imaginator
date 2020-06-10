# This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
# If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' @export
#'
#' @description
#'
#' Construct a data frame of claims simulated by time between events.
#'
#' @title claims_by_wait_time
#'
#' @param tbl_policy A data frame of policy records
#' @param claim_frequency Number of claims per policy; can be a distribution.
#' @param payment_frequency Number of payments per claim; can be a distribution.
#' @param occurrence_wait Time until occurrence for each claim; can be a distribution
#' @param report_wait Time until report; can be a distribution.
#' @param pay_wait Lag time between payments; can be a distribution.
#' @param pay_severity Severity of each claim payment; can be a distribution.
#' @param pay_only_positive Boolean indicating whether to discard negative payments.
#'
#' @details
#'
#' This function will generate claim transactions.
#'
#' Wait times and frequencies will be converted to integers with no message. If wait times or claim frequencies
#' are less than zero, or payment frequencies are less than one, they will be converted with a message.
#'
#' @importFrom dplyr left_join
#'
claims_by_wait_time <- function(
  tbl_policy
  , claim_frequency
  , payment_frequency
  , occurrence_wait
  , report_wait
  , pay_wait
  , pay_severity
  , pay_only_positive = TRUE) {

  if (missing(claim_frequency)) stop("Must supply claim_frequency")
  if (missing(pay_severity)) stop("Must supply pay_severity.")

  num_policies <- nrow(tbl_policy)

  claim_counts <- sample_or_rep(claim_frequency, num_policies)

  claim_counts <- as.integer(claim_counts)
  if (any(claim_counts < 0)) {
    message("Some claim_counts were less than zero. These will be set to zero.")
    claim_counts <- pmax(0, claim_counts)
  }

  num_claims <- sum(claim_counts)
  claim_id <- seq.int(num_claims)

  # The block above means that there may be some policies which have zero claims. This is a feature, not a bug.
  # The do.call for "c" means that those policies will drop out of the various claim vectors. We'll want to do
  # an outer join later.
  policyholder_ids <- mapply(rep, tbl_policy$policyholder_id, claim_counts, SIMPLIFY = FALSE)
  policyholder_ids <- do.call("c", policyholder_ids)
  effective_dates <- mapply(rep, tbl_policy$policy_effective_date, claim_counts, SIMPLIFY = FALSE)
  effective_dates <- do.call("c", effective_dates)
  expiration_dates <- mapply(rep, tbl_policy$policy_expiration_date, claim_counts, SIMPLIFY = FALSE)
  expiration_dates <- do.call("c", expiration_dates)

  occurrence_wait <- sample_or_rep(occurrence_wait, num_claims)
  occurrence_wait <- as.integer(occurrence_wait)
  if (any(occurrence_wait) < 0) {
    message("Some occurrence wait times are less than zero. These will be set to zero.")
    report_wait <- pmax(0, occurrence_wait)
  }

  occurrence_date <- effective_dates + occurrence_wait
  if (any(occurrence_date > expiration_dates)) {
    message("Some claims occur after policy expiration. They will be truncated to occur on the expiration date.")
    occurrence_date <- pmin(occurrence_date, expiration_dates)
  }

  report_wait <- sample_or_rep(report_wait, num_claims)
  report_wait <- as.integer(report_wait)
  if (any(report_wait) < 0) {
    message("Some reporting wait times are less than zero. These will be set to zero.")
    report_wait <- pmax(0, report_wait)
  }
  report_date <- occurrence_date + report_wait

  payment_counts <- sample_or_rep(payment_frequency, num_claims)
  payment_counts <- as.integer(payment_counts)
  if (any(payment_counts) < 1) {
    message("Some payment frequencies were less than one. They will be set to one.")
    payment_counts <- pmax(1, payment_counts)
  }
  num_payments <- sum(payment_counts)

  tbl_claims <- data.frame(
    policyholder_id = policyholder_ids
    , claim_id = claim_id
    , policy_effective_date = effective_dates
    , policy_expiration_date = expiration_dates
    , occurrence_date = occurrence_date
    , report_date = report_date
    , number_of_payments = payment_counts)

  claim_trans_ids <- mapply(rep, claim_id, payment_counts, SIMPLIFY = FALSE)
  claim_trans_ids <- do.call("c", claim_trans_ids)
  claim_trans_rep <- mapply(rep, report_date, payment_counts, SIMPLIFY = FALSE)
  claim_trans_rep <- do.call("c", claim_trans_rep)

  pay_wait <- sample_or_rep(pay_wait, num_payments)
  pay_wait <- as.integer(pay_wait)
  if (any(pay_wait < 0)) {
    message("Some payment wait times were less than zero. These will be set to zero.")
    pay_wait <- pmax(0, pay_wait)
  }

  pay_amount <- sample_or_rep(pay_severity, num_payments)
  if (any(pay_amount < 0) & pay_only_positive) {
    message("There were some negative payment amounts. They will be set to zero.
            If you don't want this, be sure to set the pay_only_positive parameter to TRUE")
  }

  dfClaimPayments <- data.frame(
      claim_id = claim_trans_ids
    , payment_date = claim_trans_rep
    , payment_wait_time = pay_wait
    , payment_amount = pay_amount
    , stringsAsFactors = FALSE
  )

  dfClaimPayments <- split(dfClaimPayments, dfClaimPayments$claim_id)
  dfClaimPayments <- lapply(dfClaimPayments, function(df){
    df$payment_wait_time <- cumsum(df$payment_wait_time)
    df$payment_date <- df$payment_date + df$payment_wait_time
    df
  })
  dfClaimPayments <- do.call(rbind, dfClaimPayments)
  dfClaimPayments$payment_wait_time <- NULL

  tbl_claims <- dplyr::left_join(tbl_claims, dfClaimPayments, by = "claim_id")

  # tbl_claims <- dplyr::left_join(tbl_policy, tbl_claims, by = c("policyholder_id", "policy_effective_date", "policy_expiration_date"))
  tbl_claims <- dplyr::left_join(tbl_policy, tbl_claims, by = c("policyholder_id", "policy_effective_date"))

  tbl_claims

}
