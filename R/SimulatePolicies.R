EmptyPolicyFrame <- function(){
  data.frame(PolicyEffectiveDate = double(0)
             , PolicyExpirationDate = double(0)
             , Exposure = double(0)
             , PolicyID = character(0)
             , stringsAsFactors = FALSE)
}

PolicyTableColumnNames <- function(){
  c("PolicyEffectiveDate"
    , "PolicyExpirationDate"
    , "Exposure"
    , "PolicyID")
}

#' @importFrom lubridate year
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
#' @description This will generate a data frame of policy data. This may be used to construct renewal and growth
#' data frames for subsequent policy years.
#'
#' @param N The number of policies to generate
#' @param PolicyYear Scalar integer indicating the policy year to generate
#' @param Exposure Vector of exposures
#' @param AdditionalColumns A list of addtional column names and values
#'
#' @return Data frame of policy data
#'
#' @details
#' Effective dates are uniformly distributed throughout the
#' year.
#'
#' When providing additional columns, each element of the list must be a scalar and be named.
#'
#' @export
#'
#' @importFrom lubridate ymd
#' @importFrom lubridate days
#'
NewPolicies <- function(N, PolicyYear, Exposure = 1, AdditionalColumns){

  dfPolicy <- EmptyPolicyFrame()

  if (N == 0) {
    return (dfPolicy)
  }

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

  if (!missing(AdditionalColumns)){
    for (i in seq_along(AdditionalColumns)){
      dfPolicy[[names(AdditionalColumns)[i]]] <- AdditionalColumns[[i]]
    }

  }

  dfPolicy

}

#' @title Simulate policy renewal
#'
#' @description Given a policy data frame, this will construct renewal data frames. The number of policies which
#' renew is governed by the the \code{Renewal} parameter.
#'
#' @name RenewPolicies
#'
#' @param dfPolicy Data frame of policy data
#' @param Renewal Scalar value greater than or equal to zero
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

#' @title Simulate policy growth
#'
#' @description Given a policy data frame, this will generate new policies in subsequent policy years.
#'
#' @name GrowPolicies
#'
#' @param dfPolicy Data frame of policy data
#' @param Growth Scalar value greater than or equal to zero
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.number
#' @importFrom lubridate year
#'
#' @export
GrowPolicies <- function(dfPolicy, Growth){
  assertthat::assert_that(is.number(Growth))
  assertthat::assert_that(Growth >= 0)
  assertthat::assert_that(nrow(dfPolicy) > 0)

  if (Growth == 0) return (EmptyPolicyFrame())

  newBizCount <- as.integer(round(nrow(dfPolicy) * Growth))

  dfOneRow <- dfPolicy[1, ]
  policyYear <- lubridate::year(dfOneRow$PolicyEffectiveDate) + 1

  addColumns <- setdiff(names(dfOneRow), PolicyTableColumnNames())

  if (length(addColumns) > 0){
    addColumns <- dfOneRow[, addColumns, drop = FALSE]
    addColumns <- as.list(addColumns)
    dfNew <- NewPolicies(N = newBizCount, PolicyYear = policyYear, AdditionalColumns = addColumns)
  } else {
    dfNew <- NewPolicies(N = newBizCount, PolicyYear = policyYear)
  }

  dfNew
}

#' @title Incremental a policy year
#'
#' @description Given a policy data frame, this will combine the \code{GrowPolicies} and \code{RenewPolicies} functions
#' to produce a subsequent policy year.
#'
#' @name IncrementPolicyYear
#'
#' @param dfPolicy A policy data frame
#' @param Renewal Scalar renewal rate
#' @param Growth Scalar growth rate
#'
#' @return Policy data frame
#'
#' @export
IncrementPolicyYear <- function(dfPolicy, Renewal, Growth){
  dfRenew <- RenewPolicies(dfPolicy, Renewal)

  dfNew <- GrowPolicies(dfPolicy, Growth)

  dfPolicy <- rbind(dfRenew, dfNew)

  dfPolicy
}

#' @title Simulate a data frame of policies
#'
#' @description Given a starting number of policies, this function will generate additional years of policy data.
#'
#' @name SimulatePolicies
#' @export
#'
#' @param N An integer giving the number of policies in the first year
#' @param PolicyYears A vector of integers in sequence
#' @param Exposure Exposure per policy
#' @param Renewal A vector indicating loss of policies
#' @param Growth A vector indicating the rate of growth of policies
#' @param AdditionalColumns A list of addtional column names and values
#'
#' @return A data frame of policy data
#'
#' @importFrom lubridate days
#' @importFrom lubridate years
#' @importFrom lubridate ymd
#'
SimulatePolicies <- function(N, PolicyYears, Exposure = 1, Renewal, Growth, AdditionalColumns)
{

  # assert that renewal and growth are numeric vectors of length one less than PolicyYears
  numYears <- length(PolicyYears)

  lstDF <- vector("list", numYears)

  lstDF[[1]] <- NewPolicies(N, PolicyYears[1], Exposure, AdditionalColumns)

  for (iYear in seq.int(2, numYears)){
    lstDF[[iYear]] <- IncrementPolicyYear(lstDF[[iYear - 1]], Renewal[iYear - 1], Growth[iYear - 1])
  }

  dfPolicy <- do.call(rbind, lstDF)

  dfPolicy
}
