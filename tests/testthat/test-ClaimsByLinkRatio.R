context("ClaimsByLinkRatio")

test_that("By link ratio", {

  policies <- 10
  dfPolicy <- NewPolicies(policies, 2001)

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
  fixedLinks <- list(
      FixedVal(1.5)
    , FixedVal(1.25)
    , FixedVal(1.1)
  )

  dfClaims <- ClaimsByLinkRatio(dfClaims
                                , Links = fixedLinks
                                , Lags = 1:4)

  numClaims <- length(unique(dfClaims$ClaimID))

  expect_equal(numClaims, policies * sum(c(10, 9, 8, 7)))
})
