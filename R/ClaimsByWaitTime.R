#' @export
#'
#' @description
#'
#' Construct a data frame of claims simulated by time between events.
#'
#' @title ClaimsByWaitTime
#'
#' @param dfPolicy A data frame of policy records
#' @param ClaimFrequency A function which will randomly generate the number of claims per policy
#' @param PaymentFrequency A function which will determine the number of payments per claim
#' @param OccurrenceWait A function which will generate the time until occurrence for each claim
#' @param ReportWait A function which will generate the time until report
#' @param PayWait A function which will generate the lag time between payments
#' @param PaySeverity A function which will randomly generate the severity of each claim payment
#' @param PayOnlyPositive Boolean indicating whether to discard negative payments.
#'
#' @details
#'
#' This function will generate claim transactions.
#'
#' Wait times and frequencies will be converted to integers with no message. If wait times or claim frequencies
#' are less than zero, or payment frequencies are less than one, they will be converted with a message.
#'
#' @importFrom dplyr data_frame
#' @importFrom dplyr left_join
#'
ClaimsByWaitTime <- function(dfPolicy
                             , ClaimFrequency
                             , PaymentFrequency
                             , OccurrenceWait
                             , ReportWait
                             , PayWait
                             , PaySeverity
                             , PayOnlyPositive = TRUE)
  {

  if (missing(ClaimFrequency)) stop("Must supply ClaimFrequency.")
  if (missing(PaySeverity)) stop("Must supply Severity.")

  # TODO: check the class of the wait time numbers using is.integer or some such

  numPolicies <- nrow(dfPolicy)

  claim_counts <- ClaimFrequency(numPolicies)
  claim_counts <- as.integer(claim_counts)
  if (any(claim_counts < 0)) {
    message("Some claim_counts were less than zero. These will be set to zero.")
    claim_counts <- pmax(0, claim_counts)
  }

  numClaims <- sum(claim_counts)
  claim_id <- seq.int(numClaims)

  # The block above means that there may be some policies which have zero claims. This is a feature, not a bug.
  # The do.call for "c" means that those policies will drop out of the various claim vectors. We'll want to do
  # an outer join later.
  policyholder_ids <- mapply(rep, dfPolicy$PolicyholderID, claim_counts, SIMPLIFY = FALSE)
  policyholder_ids <- do.call("c", policyholder_ids)
  effective_dates <- mapply(rep, dfPolicy$PolicyEffectiveDate, claim_counts, SIMPLIFY = FALSE)
  effective_dates <- do.call("c", effective_dates)
  expiration_dates <- mapply(rep, dfPolicy$PolicyExpirationDate, claim_counts, SIMPLIFY = FALSE)
  expiration_dates <- do.call("c", expiration_dates)

  occurrence_wait <- OccurrenceWait(numClaims)
  occurrence_wait <- as.integer(occurrence_wait)
  if (any(occurrence_wait) < 0){
    message("Some occurrence wait times are less than zero. These will be set to zero.")
    report_wait <- pmax(0, occurrence_wait)
  }
  occurrence_date <- effective_dates + occurrence_wait
  occ_after_exp <- any(occurrence_date > expiration_dates)

  if (occ_after_exp) {
    message("Some claims occurr after policy expiration. They will be truncated to occur on the expiration date.")
    occurrence_date <- pmin(occurrence_date, expiration_dates)
  }

  report_wait <- ReportWait(numClaims)
  report_wait <- as.integer(report_wait)
  if (any(report_wait) < 0){
    message("Some reporting wait times are less than zero. These will be set to zero.")
    report_wait <- pmax(0, report_wait)
  }
  report_date <- occurrence_date + report_wait

  payment_counts <- PaymentFrequency(numClaims)
  payment_counts <- as.integer(payment_counts)
  if (any(payment_counts) < 1){
    message("Some payment frequencies were less than one. They will be set to one.")
    payment_counts <- pmax(1, payment_counts)
  }
  num_payments <- sum(payment_counts)

  dfClaims <- dplyr::data_frame(PolicyholderID = policyholder_ids
                                , ClaimID = claim_id
                                , PolicyEffectiveDate = effective_dates
                                , PolicyExpirationDate = expiration_dates
                                , OccurrenceDate = occurrence_date
                                , ReportDate = report_date
                                , NumberOfPayments = payment_counts)


  claim_trans_ids <- mapply(rep, claim_id, payment_counts, SIMPLIFY = FALSE)
  claim_trans_ids <- do.call("c", claim_trans_ids)
  claim_trans_rep <- mapply(rep, report_date, payment_counts, SIMPLIFY = FALSE)
  claim_trans_rep <- do.call("c", claim_trans_rep)

  pay_wait <- PayWait(num_payments)
  pay_wait <- as.integer(pay_wait)
  if (any(pay_wait < 0)){
    message("Some payment wait times were less than zero. These will be set to zero.")
    pay_wait <- pmax(0, pay_wait)
  }

  pay_amount <- PaySeverity(num_payments)
  if (any(pay_amount < 0) & PayOnlyPositive){
    message("There were some negative payment amounts. They will be set to zero.
            If you don't want this, be sure to set the PayOnlyPositive parameter to TRUE")
  }

  dfClaimPayments <- dplyr::data_frame(
      ClaimID = claim_trans_ids
    , PaymentDate = claim_trans_rep
    , PaymentWaitTime = pay_wait
    , PaymentAmount = pay_amount
  )

  dfClaimPayments <- split(dfClaimPayments, dfClaimPayments$ClaimID)
  dfClaimPayments <- lapply(dfClaimPayments, function(df){
    df$PaymentWaitTime <- cumsum(df$PaymentWaitTime)
    df$PaymentDate <- df$PaymentDate + df$PaymentWaitTime
    df
  })
  dfClaimPayments <- do.call(rbind, dfClaimPayments)
  dfClaimPayments$PaymentWaitTime <- NULL

  dfClaims <- dplyr::left_join(dfClaims, dfClaimPayments, by = "ClaimID")

  dfClaims <- dplyr::left_join(dfPolicy, dfClaims, by = c("PolicyholderID", "PolicyEffectiveDate", "PolicyExpirationDate"))

  dfClaims

}
