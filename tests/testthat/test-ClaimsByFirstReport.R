context("ClaimsByFirstReport")

test_that("1st report", {

  dfPolicy <- NewPolicyYear(100, 2001)

  lstFreq <- list(
      FixedHelper(10)
    , FixedHelper(9)
    , FixedHelper(8)
    , FixedHelper(7)
  )

  lstSev <- list(
      FixedHelper(100)
    , FixedHelper(200)
    , FixedHelper(300)
    , FixedHelper(400)
  )
  dfClaims <- ClaimsByFirstReport(dfPolicy
                                  , Frequency = lstFreq
                                  , PaymentSeverity = lstSev
                                  , Lags = 1:4)

  testthat::expect_equal(nrow(dfClaims), 100 * sum(10:7))

})
