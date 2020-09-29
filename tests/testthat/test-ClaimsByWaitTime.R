context("claims_by_wait_time")

get_basic_claim_table <- function() {
  num_policies <- 1e3
  claim_frquency <- 1
  payment_frequency <- 1

  tbl_policy <- policy_year_new(num_policies, 2001)

  claims_by_wait_time(
    tbl_policy
    , claim_frquency
    , payment_frequency
    , occurrence_wait = 10
    , report_wait = 5
    , pay_wait = 5
    , pay_severity = 50)

}

test_that("Proper number of rows in wait time", {

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

test_that("No columns missing from a join", {

  num_policies <- 10
  claim_frquency <- 1
  payment_frequency <- 1

  tbl_policy <- policy_year_new(num_policies, 2001)

  tbl_claim <- claims_by_wait_time(
    tbl_policy
    , claim_frquency
    , payment_frequency
    , occurrence_wait = 10
    , report_wait = 5
    , pay_wait = 5
    , pay_severity = 50)

  # The test is a bit hacky, but should work. We're looking for '.x' in a column name.
  # The various join functions will append that to a column name when a duplicated name appears in the joined result.
  junk_col_names <- grep("\\.x", names(tbl_claim))

  testthat::expect_length(junk_col_names, 0)

})

test_that("Dates are sensible", {

  tbl_claim <- get_basic_claim_table()

  # No report before occurrence
  testthat::expect_false(any(tbl_claim$report_date < tbl_claim$occurrence_date))
  # No payment before occurrence
  testthat::expect_false(any(tbl_claim$payment_date < tbl_claim$occurrence_date))
  # No payment before report
  testthat::expect_false(any(tbl_claim$payment_date < tbl_claim$report_date))

})
