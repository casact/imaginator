context("ClaimsByFirstReport")

test_that("1st report", {

  dfPolicy <- NewPolicyYear(100, 2001)

  dfClaims <- ClaimsByFirstReport(dfPolicy
                                  , Frequency = FixedHelper(10:7)
                                  , PaymentSeverity = FixedHelper(100 * 1:4)
                                  , Lags = 1:4)

  testthat::expect_equal(nrow(dfClaims), 100 * sum(10:7))

  dfClaims <- ClaimsByFirstReport(
      dfPolicy
    , Frequency = FixedHelper(10)
    , PaymentSeverity = FixedHelper(100)
    , Lags = 1)

  testthat::expect_equal(nrow(dfClaims), 100 * 10)

})
