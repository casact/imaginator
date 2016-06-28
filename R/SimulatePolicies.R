GetExpirationDate <- function(EffectiveDate){
  policyYear <- lubridate::year(EffectiveDate)
  ExpirationDate <- EffectiveDate + days(364)

  addOne <- lubridate::leap_year(lubridate::year(ExpirationDate))

  ExpirationDate[addOne] <- ExpirationDate[addOne] + days(1)

  ExpirationDate

}

#' @importFrom stringi stri_rand_strings
GetPolicyIDs <- function(N){
  policyID_length <- as.integer(log(N, 36)) + 1

  numPortion <- stringi::stri_rand_strings(N, policyID_length, pattern = "[0-9]")

  charPortion <- stringi::stri_rand_strings(N, policyID_length, pattern = "[A-Z]")

  policyID <- paste(numPortion, charPortion, sep = "-")

  policyID
}

#' @title Simulate a new set of policies
#'
#' @name NewPolicies
#'
#' @param N The number of policies to generate
#' @param PolicyYear Scalar integer indicating the policy year to generate
#' @param Exposure Vector of exposures
#'
#' @return Data frame of policy data
#'
#' @details
#' This function will create a data frame of policy data. Effective dates are uniformly distributed throughout the
#' year.
#'
#' @export
#'
#' @importFrom lubridate ymd
#' @importFrom lubridate days
#'
NewPolicies <- function(N, PolicyYear, Exposure = 1){

  days <- 365 + ifelse(lubridate::leap_year(PolicyYear), 1, 0)
  days <- days - 1
  effectiveDates <- lubridate::ymd(paste(PolicyYear, "01", "01", sep="-"))
  dayOffsets <- sample(days, size = N, replace = TRUE)
  effectiveDates <- effectiveDates + lubridate::days(dayOffsets)
  expirationDates <- GetExpirationDate(effectiveDates)

  dfPolicy <- data.frame(PolicyEffectiveDate = effectiveDates
                         , PolicyExpirationDate = expirationDates
                         , Exposure = Exposure
                         , PolicyID = GetPolicyIDs(N)
                         , stringsAsFactors = FALSE)

}

#' @title Simulate policy growth
#'
#' @name GrowPolicies

#' @title Simulate the renewal of a set of policies
#' @name RenewPolicies
#'
#' @param dfPolicy Data frame of policy data
#' @param Renewal Scalar value between zero and one
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.number
#' @importFrom lubridate days
#' @importFrom lubridate leap_year
#'
#' @export
RenewPolicies <- function(dfPolicy, Renewal){
  assertthat::assert_that(is.number(Renewal))
  assertthat::assert_that(Renewal >= 0, Renewal <= 1)

  renewals <- nrow(dfPolicy) * Renewal
  renewals <- base::sample(seq.int(nrow(dfPolicy)), size = renewals)
  dfPolicy <- dfPolicy[renewals, ]

  dfPolicy$PolicyEffectiveDate <- dfPolicy$PolicyExpirationDate +  days(1)
  dfPolicy$PolicyExpirationDate <- GetExpirationDate(dfPolicy$PolicyEffectiveDate)

  dfPolicy
}

#' @title Simulate a data frame of policies
#' @name SimulatePolicies
#' @export
#'
#' @param PolicyYears A vector of integers in sequence
#' @param N An integer giving the number of policies per year
#' @param Exposure Exposure per policy
#' @param Growth A vector indicating the rate of growth of policies
#' @param Retention A vector indicating loss of policies
#'
#' @return A data frame of policy data as explained in
#'
#' @details
#'
#' @importFrom lubridate days
#' @importFrom lubridate years
#' @importFrom lubridate ymd
#'
#' @examples
#'
#' py <- 2001:2010
#'
#' N <- 5000
#'
#' exposure <- 1
#' rate <- 1
#'
#' retention <- 0.9
SimulatePolicies <- function(StartYear, NumYears, N, Exposure = 1)
{

  numYears <- length(PolicyYears)

  N <- rep(N, numYears)

  days <- rep(365, length(numYears)) + ifelse(lubridate::leap_year(PolicyYears), 1, 0)
  days <- days - 1

  lstDF <- vector("list", numYears)

  for (iYear in seq_along(PolicyYears)){

    effectiveDates <- lubridate::ymd(paste(PolicyYears[iYear], "01", "01", sep="-"))
    dayOffsets <- sample(days[iYear], size = N[iYear], replace = TRUE)
    effectiveDates <- effectiveDates + lubridate::days(dayOffsets)
    expirationDates <- effectiveDates + days[iYear]

    policyID_length <- as.integer(log(N[iYear], 36)) + 2

    lstDF[[iYear]] <- data.frame(PolicyEffectiveDate = effectiveDates
                                 , PolicyExpirationDate = expirationDates
                                 , Exposure = Exposure
                                 , PolicyID = stringi::stri_rand_strings(N[iYear], policyID_length)
                                 , stringsAsFactors = FALSE)
  }

  dfPolicy <- do.call(rbind, lstDF)

  dfPolicy
}
