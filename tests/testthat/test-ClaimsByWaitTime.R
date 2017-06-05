context("ClaimsByWaitTime")

test_that("Wait time", {

  dfPolicy <- NewPolicyYear(100, 2001)

  ClaimFrequency <- FixedHelper(10)
  PaymentFrequency <- FixedHelper(10)
  OccurrenceWait <- FixedHelper(180)
  ReportWait <- FixedHelper(25)
  PayWait <- FixedHelper(25)
  PaySeverity <- FixedHelper(250)

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
    , ClaimFrequency = FixedHelper(2)
    , PaymentFrequency = FixedHelper(1)
    , OccurrenceWait = FixedHelper(10)
    , ReportWait = FixedHelper(5)
    , PayWait = FixedHelper(5)
    , PaySeverity = FixedHelper(50))

  testthat::expect_equal(nrow(dfClaim), 2 * 5 * 2)
})
