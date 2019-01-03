context("ClaimsByLinkRatio")

test_that("By link ratio", {

  policies <- 10
  dfPolicy <- NewPolicyYear(policies, 2001)

  dfClaims <- ClaimsByFirstReport(dfPolicy
                                  , Frequency = FixedHelper(10:7)
                                  , PaymentSeverity = FixedHelper(100 * 1:4)
                                  , Lags = 1:4)

  dfClaims <- ClaimsByLinkRatio(dfClaims
                                , Links = FixedHelper(c(1.5, 1.25, 1.1))
                                , Lags = 1:4)

  numClaims <- length(unique(dfClaims$ClaimID))

  expect_equal(numClaims, policies * sum(c(10, 9, 8, 7)))
})

test_that("Pad functions", {
  set.seed(12345)
  dfPolicy <- SimulatePolicies(2, 2001:2005)

  expect_message(
    imaginator::ClaimsByFirstReport(dfPolicy, FixedHelper(4:1), FixedHelper(250), Lags = 1:4)
  )

})

test_that("Claim count checks out", {
  require(dplyr)
  set.seed(12345)
  numPolicies <- 2
  dfPolicy <- SimulatePolicies(numPolicies, 2001:2005)

  dfIBNYR_Fixed <- ClaimsByFirstReport(
    dfPolicy
    , Frequency = FixedHelper(4:1)
    , PaymentSeverity = FixedHelper(rep(250, 4))
    , Lags = 1:4)

  claim_count_lag_1 <- dfIBNYR_Fixed %>%
    dplyr::filter(Lag == 1) %>%
    dplyr::filter(lubridate::year(PolicyEffectiveDate) == 2001) %>%
    nrow()

  expect_equal(claim_count_lag_1, numPolicies * 4)
})
