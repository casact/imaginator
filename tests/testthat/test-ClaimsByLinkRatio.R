context("ClaimsByLinkRatio")

test_that("By link ratio", {

  policies <- 10
  dfPolicy <- NewPolicyYear(policies, 2001)

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
  fixedLinks <- list(
      FixedHelper(1.5)
    , FixedHelper(1.25)
    , FixedHelper(1.1)
  )

  dfClaims <- ClaimsByLinkRatio(dfClaims
                                , Links = fixedLinks
                                , Lags = 1:4)

  numClaims <- length(unique(dfClaims$ClaimID))

  expect_equal(numClaims, policies * sum(c(10, 9, 8, 7)))
})
