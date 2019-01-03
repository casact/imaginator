context("SimulatePolicies")

test_that("Test inputs", {
  expect_error(imaginator::SimulatePolicies(10))

  expect_silent(imaginator::SimulatePolicies(10, NumYears = 5))

  expect_silent(imaginator::SimulatePolicies(1000, NumYears = 5, Retention = rep(0.8, 4)))
  expect_warning(imaginator::SimulatePolicies(1000, NumYears = 5, Retention = rep(0.8, 5)))
  expect_warning(imaginator::SimulatePolicies(1000, NumYears = 5, Retention = rep(0.8, 3)))

  expect_silent(imaginator::SimulatePolicies(1000, NumYears = 5, Growth = rep(0.8, 4)))
  expect_warning(imaginator::SimulatePolicies(1000, NumYears = 5, Growth = rep(0.8, 3)))
  expect_warning(imaginator::SimulatePolicies(1000, NumYears = 5, Growth = rep(0.8, 5)))

  expect_warning(imaginator::NewPolicyYear(2, 2001:2005))

})

test_that("No negative inputs", {
  expect_error(imaginator::SimulatePolicies(10, PolicyYears = -1:-4))
})

test_that("No skips in policy years", {
  expect_error(imaginator::SimulatePolicies(10, PolicyYears = c(1,5)))
})

test_that("Only one policy year", {
  expect_silent(imaginator::SimulatePolicies(10, NumYears = 1))
  expect_silent(imaginator::SimulatePolicies(10, PolicyYears = 2001))
})

test_that("Form a policy table", {
  expect_silent(imaginator::SimulatePolicies(50, PolicyYears = 2011:2015, Retention = 0.8, Growth = .1))
})

test_that("New policies", {
  df <- NewPolicyYear(50, 2001, 1)
  expect_equal(nrow(df), 50)
  expect_equal(ncol(df), 4)
})

test_that("Leap year works", {
  dfLeapYear <- data.frame(PolicyEffectiveDate = as.Date("2014-03-01")
                           , PolicyExpirationDate = as.Date("2015-02-28")
                           , Exposure = 1
                           , stringsAsFactors = FALSE)

  dfRenew <- RenewPolicies(dfLeapYear, 1.0)
  expect_equal(dfRenew$PolicyEffectiveDate, as.Date("2015-03-01"))
  expect_equal(dfRenew$PolicyExpirationDate, as.Date("2016-02-29"))

  dfLeapYear <- data.frame(PolicyEffectiveDate = as.Date("2000-02-12")
                           , PolicyExpirationDate = as.Date("2001-02-11")
                           , Exposure = 1
                           , stringsAsFactors = FALSE)
  dfRenew <- RenewPolicies(dfLeapYear, 1.0)
  expect_equal(dfRenew$PolicyEffectiveDate, as.Date("2001-02-12"))
  expect_equal(dfRenew$PolicyExpirationDate, as.Date("2002-02-11"))

  set.seed(1234)
  dfPolicies <- SimulatePolicies(N = 2, NumYears = 5)
  library(dplyr)

  dfPolicies %>%
    arrange(PolicyEffectiveDate) %>%
    slice(c(1,3))

  # expect_equal(dfPolicies$PolicyExpirationDate[1], as.Date("2001-02-11"))
  # expect_equal(dfPolicies$PolicyEffectiveDate[2], as.Date("2001-02-12"))
})
