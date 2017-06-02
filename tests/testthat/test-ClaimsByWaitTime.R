context("ClaimsByWaitTime")

test_that("Wait time", {

  dfPolicy <- NewPolicies(100, 2001)

  ClaimFrequency <- FixedVal(10)
  PaymentFrequency <- FixedVal(10)
  OccurrenceWait <- FixedVal(180)
  ReportWait <- FixedVal(25)
  PayWait <- FixedVal(25)
  PaySeverity <- FixedVal(250)

  dfClaims <- ClaimsByWaitTime(dfPolicy
                               , ClaimFrequency
                               , PaymentFrequency
                               , OccurrenceWait
                               , ReportWait
                               , PayWait
                               , PaySeverity)

  testthat::expect_equal(nrow(dfClaims), 100 * 10 * 10)

})
