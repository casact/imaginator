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
