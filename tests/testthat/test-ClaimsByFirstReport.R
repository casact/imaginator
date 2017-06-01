context("ClaimsByFirstReport")

test_that("1st report", {

  dfPolicy <- NewPolicies(100, 2001)

  lstFreq <- list(
      FixedVal(10)
    , FixedVal(9)
    , FixedVal(8)
    , FixedVal(7)
  )

  lstSev <- list(
      FixedVal(100)
    , FixedVal(200)
    , FixedVal(300)
    , FixedVal(400)
  )
  dfClaims <- ClaimsByFirstReport(dfPolicy
                                  , Frequency = lstFreq
                                  , Severity = lstSev
                                  , Lags = 1:4)

  testthat::expect_equal(nrow(dfClaims), 100 * sum(10:7))

})
