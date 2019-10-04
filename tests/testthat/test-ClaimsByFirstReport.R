context("claims_by_first_report")

test_that("1st report", {

  dfPolicy <- policy_year_new(100, 2001)

  dfClaims <- claims_by_first_report(
    dfPolicy
    , frequency = 10:7
    , payment_severity = 100 * 1:4
    , lags = 1:4)

  testthat::expect_equal(nrow(dfClaims), 100 * sum(10:7))

  dfClaims <- claims_by_first_report(
      dfPolicy
    , frequency = 10
    , payment_severity = 100
    , lags = 1)

  testthat::expect_equal(nrow(dfClaims), 100 * 10)

})
