context("claims_by_link_ratio")

test_that("By link ratio", {

  policies <- 10
  dfPolicy <- policy_year_new(policies, 2001)

  dfClaims <- claims_by_first_report(dfPolicy
                                  , frequency = 10:7
                                  , payment_severity = 100 * 1:4
                                  , lags = 1:4)

  dfClaims <- claims_by_link_ratio(dfClaims
                                , links = c(1.5, 1.25, 1.1)
                                , lags = 1:4)

  numClaims <- length(unique(dfClaims$claim_id))

  expect_equal(numClaims, policies * sum(c(10, 9, 8, 7)))
})

test_that("Pad functions", {
  set.seed(12345)
  dfPolicy <- policies_simulate(2, 2001:2005)

  expect_message(
    imaginator::claims_by_first_report(dfPolicy, 4:1, 250, lags = 1:4)
  )

})

test_that("Claim count checks out", {

  require(dplyr)
  set.seed(12345)
  numPolicies <- 2
  dfPolicy <- policies_simulate(numPolicies, 2001:2005)

  dfIBNYR_Fixed <- claims_by_first_report(
    dfPolicy
    , frequency = 4:1
    , payment_severity = rep(250, 4)
    , lags = 1:4)

  claim_count_lag_1 <- dfIBNYR_Fixed %>%
    dplyr::filter(lag == 1) %>%
    dplyr::filter(lubridate::year(policy_effective_date) == 2001) %>%
    nrow()

  expect_equal(claim_count_lag_1, numPolicies * 4)

})
