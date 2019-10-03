context("ClaimsByWaitTime")

test_that("Wait time", {

  dfPolicy <- NewPolicyYear(100, 2001)

  ClaimFrequency <- 10
  PaymentFrequency <- 10
  OccurrenceWait <- 180
  ReportWait <- 25
  PayWait <- 25
  PaySeverity <- 250

  dfClaims <- ClaimsByWaitTime(dfPolicy
                               , ClaimFrequency
                               , PaymentFrequency
                               , OccurrenceWait
                               , ReportWait
                               , PayWait
                               , PaySeverity)

  testthat::expect_equal(nrow(dfClaims), 100 * 10 * 10)

  dfPolicy <- SimulatePolicies(2, 2001:2005)
  dfClaim <- ClaimsByWaitTime(
      dfPolicy
    , ClaimFrequency = 2
    , PaymentFrequency = 1
    , OccurrenceWait = 10
    , ReportWait = 5
    , PayWait = 5
    , PaySeverity = 50)

  testthat::expect_equal(nrow(dfClaim), 2 * 5 * 2)
})
