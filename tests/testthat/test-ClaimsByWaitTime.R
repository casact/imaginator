context("claims_by_wait_time")

test_that("Wait time", {

  num_policies <- 100
  claim_frquency <- 10
  payment_frequency <- 10

  tbl_policy <- policy_year_new(num_policies, 2001)

  tbl_claim <- claims_by_wait_time(
    tbl_policy
    , claim_frquency
    , payment_frequency
    , occurrence_wait = 10
    , report_wait = 5
    , pay_wait = 5
    , pay_severity = 50)

  testthat::expect_equal(nrow(tbl_claim), num_policies * claim_frquency * payment_frequency)

  num_policies <- 2
  payment_frequency <- 1
  claim_frequency <- 10

  policy_years <- 2001:2003

  tbl_policy <- policies_simulate(num_policies, policy_years)
  tbl_claim <- claims_by_wait_time(
      tbl_policy
    , claim_frequency
    , payment_frequency
    , occurrence_wait = 10
    , report_wait = 5
    , pay_wait = 5
    , pay_severity = 50)

  testthat::expect_equal(nrow(tbl_claim), length(policy_years) * num_policies * claim_frquency * payment_frequency)

})
