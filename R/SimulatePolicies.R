EmptyPolicyFrame <- function(){
  data.frame(PolicyEffectiveDate = double(0)
             , PolicyExpirationDate = double(0)
             , Exposure = double(0)
             , PolicyID = character(0)
             , stringsAsFactors = FALSE)
}

PolicyTableColumnNames <- function(){
  names(EmptyPolicyFrame())
}

#' @importFrom lubridate year
#' @importFrom lubridate leap_year
#' @importFrom lubridate days
GetExpirationDate <- function(EffectiveDate){
  policyYear <- lubridate::year(EffectiveDate)
  ExpirationDate <- EffectiveDate + lubridate::days(364)

  addOne <- lubridate::leap_year(lubridate::year(ExpirationDate))

  ExpirationDate[addOne] <- ExpirationDate[addOne] + lubridate::days(1)

  ExpirationDate

}

#' @title Simulate a new set of policies
#'
#' @description This will generate a data frame of policy data. This may be used to construct renewal and growth
#' data frames for subsequent policy years.
#'
#' @param N The number of policies to generate
#' @param PolicyYear Scalar integer indicating the policy year to generate
#' @param Exposure Vector of exposures
#' @param StartID Integer of the first number in the policy ID sequence
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
#' @importFrom lubridate leap_year
#'
NewPolicies <- function(N, PolicyYear, Exposure = 1, StartID = 1, AdditionalColumns){

  dfPolicy <- EmptyPolicyFrame()

  if (N == 0) {
    return (dfPolicy)
  }

  days <- 365 + ifelse(lubridate::leap_year(PolicyYear), 1, 0)
  days <- days - 1
  strDayOne <- paste(sprintf("%04d", PolicyYear), "01", "01", sep="-")
  effectiveDates <- lubridate::ymd(strDayOne)
  dayOffsets <- sample(days, size = N, replace = TRUE)
  effectiveDates <- effectiveDates + lubridate::days(dayOffsets)
  expirationDates <- GetExpirationDate(effectiveDates)

  dfPolicy <- data.frame(PolicyEffectiveDate = effectiveDates
                         , PolicyExpirationDate = expirationDates
                         , Exposure = Exposure
                         , PolicyID = seq.int(StartID, length.out = N)
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
#' renew is governed by the the \code{Retention} parameter.
#'
#' @name RenewPolicies
#'
#' @param dfPolicy Data frame of policy data
#' @param Retention Scalar value greater than or equal to zero
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.number
#' @importFrom lubridate days
#' @importFrom lubridate leap_year
#'
#' @export
RenewPolicies <- function(dfPolicy, Retention){
  assertthat::assert_that(is.number(Retention))
  assertthat::assert_that(Retention >= 0, Retention <= 1)

  renewals <- nrow(dfPolicy) * Retention
  renewals <- base::sample(seq.int(nrow(dfPolicy)), size = renewals)
  dfPolicy <- dfPolicy[renewals, ]

  dfPolicy$PolicyEffectiveDate <- dfPolicy$PolicyExpirationDate + lubridate::days(1)
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

  maxPolicyID <- max(dfPolicy$PolicyID)

  if (length(addColumns) > 0){
    addColumns <- dfOneRow[, addColumns, drop = FALSE]
    addColumns <- as.list(addColumns)
    dfNew <- NewPolicies(N = newBizCount, PolicyYear = policyYear, StartID = maxPolicyID + 1, AdditionalColumns = addColumns)
  } else {
    dfNew <- NewPolicies(N = newBizCount, PolicyYear = policyYear, StartID = maxPolicyID + 1)
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
#' @param Retention Scalar renewal rate
#' @param Growth Scalar growth rate
#'
#' @return Policy data frame
#'
#' @export
IncrementPolicyYear <- function(dfPolicy, Retention, Growth){
  dfRenew <- RenewPolicies(dfPolicy, Retention)

  dfNew <- GrowPolicies(dfPolicy, Growth)

  dfPolicy <- rbind(dfRenew, dfNew)

  dfPolicy
}

FixGrowthVector <- function(vecIn, numRenewals, vec_kind)
{

  if (length(vecIn) == 1) {
    vecIn <- rep(vecIn, length.out = numRenewals)
  }

  if (length(vecIn) < numRenewals) {
    message(paste(vec_kind, "vector is > 1, but too short. It will be padded."))
    vecIn = rep(vecIn, length.out = numRenewals)
  }

  if (length(vecIn) > numRenewals) {
    message(paste(vec_kind, "vector is too long. It will be truncated."))
    vecIn = head(vecIn, numRenewals)
  }

  vecIn
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
#' @param NumYears The number of years to simulate. If `PolicyYears` is given, this is ignored.
#' @param Exposure Exposure per policy
#' @param Retention A vector indicating loss of policies
#' @param Growth A vector indicating the rate of growth of policies
#' @param StartID Integer of the first number in the policy ID sequence
#' @param AdditionalColumns A list of addtional column names and values
#'
#' @return A data frame of policy data
#'
#' @importFrom lubridate days
#' @importFrom lubridate years
#' @importFrom lubridate ymd
#'
#' @description Growth is given as a the positive rate of growth of new policies. This may be set to zero.
#'
#' Retention is given as the portion of expiring policies which will renew.
#'
SimulatePolicies <- function(N, PolicyYears, NumYears, Exposure = 1, Retention = 1, Growth = 0, StartID = 1, AdditionalColumns)
{

  if (missing(PolicyYears)) {
    if (missing(NumYears)) {
      stop("At least one of PolicyYears or NumYears must be given.")
    }
    numYears <- NumYears
    PolicyYears <- seq.int(2000, length.out = numYears)
  } else {
    if (min(PolicyYears) <= 0) {
      stop("PolicyYears can't be negative")
    }
    if (length(PolicyYears) != (max(PolicyYears) - min(PolicyYears) + 1)) {
      stop("PolicyYears must be a sequence.")
    }
    numYears <- length(PolicyYears)
  }
  numRenewals <- numYears - 1

  Retention <- FixGrowthVector(Retention, numRenewals, "Retention")
  Growth <- FixGrowthVector(Growth, numRenewals, "Growth")

  if (numYears == 1) {
    return (NewPolicies(N, PolicyYears[1], Exposure, StartID, AdditionalColumns))
  } else {
    lstDF <- vector("list", numYears)
    lstDF[[1]] <- NewPolicies(N, PolicyYears[1], Exposure, StartID, AdditionalColumns)
  }

  for (iYear in seq.int(2, numYears)){
    lstDF[[iYear]] <- IncrementPolicyYear(lstDF[[iYear - 1]], Retention[iYear - 1], Growth[iYear - 1])
  }

  dfPolicy <- do.call(rbind, lstDF)

  dfPolicy
}
