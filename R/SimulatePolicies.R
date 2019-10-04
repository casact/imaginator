empty_policy_table <- function(){

  data.frame(
    policy_effective_date = double(0)
    , policy_expiration_date = double(0)
    , exposure = double(0)
    , policyholder_id = character(0)
    , stringsAsFactors = FALSE
  )
}

PolicyTableColumnNames <- function(){
  names(empty_policy_table())
}

#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom lubridate days
#' @importFrom lubridate years
get_expiration_date <- function(effective_date){

  # If the date is February 29, we subtract one day. Otherwise, adding one year will result in NA
  leap_days <- (lubridate::month(effective_date) == 2 & lubridate::day(effective_date) == 29)
  effective_date[leap_days] <- effective_date[leap_days] - days(1)

  effective_date + lubridate::years(1) - days(1)

}

#' @title Simulate a new policy year
#'
#' @description This will generate a data frame of policy data. This may be used to construct renewal and growth
#' data frames for subsequent policy years.
#'
#' @param n The number of policies to generate
#' @param policy_year Scalar integer indicating the policy year to generate
#' @param exposure Vector of exposures
#' @param start_id Integer of the first number in the policy ID sequence
#' @param additional_columns A list of addtional column names and values
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
policy_year_new <- function(n, policy_year, exposure = 1, start_id = 1, additional_columns){

  if (length(policy_year) != 1) {
    if (length(policy_year) == 0) {
      policy_year <- 2001
    } else {
      warning("policy_year is not scalar. Only the first value will be taken.")
      policy_year <- policy_year[1]
    }
  }

  tbl_policy <- empty_policy_table()

  if (n == 0) {
    return(tbl_policy)
  }

  days <- 365 + ifelse(lubridate::leap_year(policy_year), 1, 0)
  days <- days - 1
  strDayOne <- paste(sprintf("%04d", policy_year), "01", "01", sep = "-")
  effectiveDates <- lubridate::ymd(strDayOne)
  dayOffsets <- sample(days, size = n, replace = TRUE)
  effectiveDates <- effectiveDates + lubridate::days(dayOffsets)
  expirationDates <- get_expiration_date(effectiveDates)

  tbl_policy <- data.frame(policy_effective_date = effectiveDates
                         , policy_expiration_date = expirationDates
                         , exposure = exposure
                         , policyholder_id = seq.int(start_id, length.out = n)
                         , stringsAsFactors = FALSE)

  if (!missing(additional_columns)) {
    for (i in seq_along(additional_columns)) {
      tbl_policy[[names(additional_columns)[i]]] <- additional_columns[[i]]
    }

  }

  tbl_policy

}

#' @title Simulate policy renewal
#'
#' @description Given a policy data frame, this will construct renewal data frames. The number of policies which
#' renew is governed by the the \code{Retention} parameter.
#'
#' @name policies_renew
#'
#' @param tbl_policy Data frame of policy data
#' @param retention Scalar value greater than or equal to zero
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.number
#' @importFrom lubridate days
#' @importFrom lubridate leap_year
#'
#' @export
policies_renew <- function(tbl_policy, retention){
  assertthat::assert_that(is.number(retention))
  assertthat::assert_that(retention >= 0, retention <= 1)

  renewals <- nrow(tbl_policy) * retention
  renewals <- base::sample(seq.int(nrow(tbl_policy)), size = renewals)
  tbl_policy <- tbl_policy[renewals, ]

  tbl_policy$policy_effective_date <- tbl_policy$policy_expiration_date + lubridate::days(1)
  tbl_policy$policy_expiration_date <- get_expiration_date(tbl_policy$policy_effective_date)

  tbl_policy
}

#' @title Simulate policy growth
#'
#' @description Given a policy data frame, this will generate new policies in subsequent policy years.
#'
#' @name policies_grow
#'
#' @param tbl_policy Data frame of policy data
#' @param growth Scalar value greater than or equal to zero
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.number
#' @importFrom lubridate year
#'
#' @export
policies_grow <- function(tbl_policy, growth){

  assertthat::assert_that(is.number(growth))
  assertthat::assert_that(growth >= 0)
  assertthat::assert_that(nrow(tbl_policy) > 0)

  if (growth == 0) return(empty_policy_table())

  newBizCount <- as.integer(round(nrow(tbl_policy) * growth))

  dfOneRow <- tbl_policy[1, ]
  policyYear <- lubridate::year(dfOneRow$policy_effective_date) + 1

  addColumns <- setdiff(names(dfOneRow), PolicyTableColumnNames())

  maxPolicyholderID <- max(tbl_policy$policyholder_id)

  if (length(addColumns) > 0) {
    addColumns <- dfOneRow[, addColumns, drop = FALSE]
    addColumns <- as.list(addColumns)
    dfNew <- policy_year_new(n = newBizCount, policy_year = policyYear, start_id = maxPolicyholderID + 1, additional_columns = addColumns)
  } else {
    dfNew <- policy_year_new(n = newBizCount, policy_year = policyYear, start_id = maxPolicyholderID + 1)
  }

  dfNew
}

#' @title Incremental a policy year
#'
#' @description Given a policy data frame, this will combine the \code{policies_grow} and \code{policies_renew} functions
#' to produce a subsequent policy year.
#'
#' @name policy_year_increment
#'
#' @param tbl_policy A policy data frame
#' @param retention Scalar renewal rate
#' @param growth Scalar growth rate
#'
#' @return Policy data frame
#'
#' @export
policy_year_increment <- function(tbl_policy, retention, growth){
  dfRenew <- policies_renew(tbl_policy, retention)

  dfNew <- policies_grow(tbl_policy, growth)

  tbl_policy <- rbind(dfRenew, dfNew)

  tbl_policy
}

#' @importFrom utils head
FixGrowthVector <- function(vecIn, numRenewals, vec_kind)
{

  if (length(vecIn) == 1) {
    vecIn <- rep(vecIn, length.out = numRenewals)
  }

  if (length(vecIn) < numRenewals) {
    warning(paste(vec_kind, "vector is > 1, but too short. It will be padded."))
    vecIn = rep(vecIn, length.out = numRenewals)
  }

  if (length(vecIn) > numRenewals) {
    warning(paste(vec_kind, "vector is too long. It will be truncated."))
    vecIn = head(vecIn, numRenewals)
  }

  vecIn
}

#' @title Simulate a data frame of policies
#'
#' @description Given a starting number of policies, this function will generate additional years of policy data.
#'
#' @name policies_simulate
#' @export
#'
#' @param n An integer giving the number of policies in the first year
#' @param policy_years A vector of integers in sequence
#' @param num_years The number of years to simulate. If `policy_years` is given, this is ignored.
#' @param exposure Exposure per policy
#' @param retention A vector indicating loss of policies
#' @param growth A vector indicating the rate of growth of policies
#' @param start_id Integer of the first number in the policy ID sequence
#' @param additional_columns A list of addtional column names and values
#'
#' @return A data frame of policy data
#'
#' @importFrom lubridate days
#' @importFrom lubridate years
#' @importFrom lubridate ymd
#' @importFrom checkmate assertIntegerish
#'
#' @description Growth is given as a the positive rate of growth of new policies. This may be set to zero.
#'
#' Retention is given as the portion of expiring policies which will renew.
#'
policies_simulate <- function(n, policy_years, num_years, exposure = 1, retention = 1, growth = 0, start_id = 1, additional_columns)
{

  if (missing(policy_years) & missing(num_years)) {
      stop("At least one of policy_years or num_years must be given.")
  }

  if (missing(policy_years)) {
    policy_years <- seq.int(2000, length.out = num_years)
  } else {
    assertIntegerish(
        policy_years
      , lower = 1
      , any.missing = FALSE
      , unique = TRUE)
    if (!is.integer(policy_years)) {
      policy_years <- as.integer(policy_years)
    }
    if (length(policy_years) != (max(policy_years) - min(policy_years) + 1)) {
      stop("policy_years sequence must not contain any skips.")
    }
    num_years <- length(policy_years)
  }
  numRenewals <- num_years - 1

  retention <- FixGrowthVector(retention, numRenewals, "Retention")
  growth <- FixGrowthVector(growth, numRenewals, "Growth")

  if (num_years == 1) {
    return(policy_year_new(n, policy_years[1], exposure, start_id, additional_columns))
  } else {
    lstDF <- vector("list", num_years)
    lstDF[[1]] <- policy_year_new(n, policy_years[1], exposure, start_id, additional_columns)
  }

  for (iYear in seq.int(2, num_years)) {
    lstDF[[iYear]] <- policy_year_increment(lstDF[[iYear - 1]], retention[iYear - 1], growth[iYear - 1])
  }

  tbl_policy <- do.call(rbind, lstDF)

  tbl_policy
}
