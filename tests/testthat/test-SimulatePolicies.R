context("policies_simulate")

test_that("Test inputs", {
  expect_error(imaginator::policies_simulate(10))

  expect_silent(imaginator::policies_simulate(10, num_years = 5))

  expect_silent(imaginator::policies_simulate(1000, num_years = 5, retention = rep(0.8, 4)))
  expect_warning(imaginator::policies_simulate(1000, num_years = 5, retention = rep(0.8, 5)))
  expect_warning(imaginator::policies_simulate(1000, num_years = 5, retention = rep(0.8, 3)))

  expect_silent(imaginator::policies_simulate(1000, num_years = 5, growth = rep(0.8, 4)))
  expect_warning(imaginator::policies_simulate(1000, num_years = 5, growth = rep(0.8, 3)))
  expect_warning(imaginator::policies_simulate(1000, num_years = 5, growth = rep(0.8, 5)))

  expect_warning(imaginator::policy_year_new(2, 2001:2005))

})

test_that("No negative inputs", {
  expect_error(imaginator::policies_simulate(10, policy_years = -1:-4))
})

test_that("No skips in policy years", {
  expect_error(imaginator::policies_simulate(10, policy_years = c(1,5)))
})

test_that("Only one policy year", {
  expect_silent(imaginator::policies_simulate(10, num_years = 1))
  expect_silent(imaginator::policies_simulate(10, policy_years = 2001))
})

test_that("Form a policy table", {
  expect_silent(imaginator::policies_simulate(50, policy_years = 2011:2015, retention = 0.8, growth = .1))
})

test_that("New policies", {
  df <- policy_year_new(50, 2001, 1)
  expect_equal(nrow(df), 50)
  expect_equal(ncol(df), 4)
})

test_that("Leap year works", {
  dfLeapYear <- data.frame(
    policy_effective_date = as.Date("2014-03-01")
    , policy_expiration_date = as.Date("2015-02-28")
    , exposure = 1
    , stringsAsFactors = FALSE)

  dfRenew <- policies_renew(dfLeapYear, 1.0)
  expect_equal(dfRenew$policy_effective_date, as.Date("2015-03-01"))
  expect_equal(dfRenew$policy_expiration_date, as.Date("2016-02-29"))

  dfLeapYear <- data.frame(policy_effective_date = as.Date("2000-02-12")
                           , policy_expiration_date = as.Date("2001-02-11")
                           , exposure = 1
                           , stringsAsFactors = FALSE)
  dfRenew <- policies_renew(dfLeapYear, 1.0)
  expect_equal(dfRenew$policy_effective_date, as.Date("2001-02-12"))
  expect_equal(dfRenew$policy_expiration_date, as.Date("2002-02-11"))

  set.seed(1234)
  dfPolicies <- policies_simulate(n = 2, num_years = 5)
  library(dplyr)

  dfPolicies %>%
    arrange(policy_effective_date) %>%
    slice(c(1,3))

  # expect_equal(dfPolicies$PolicyExpirationDate[1], as.Date("2001-02-11"))
  # expect_equal(dfPolicies$PolicyEffectiveDate[2], as.Date("2001-02-12"))
})
