context("SimulatePolicies")

test_that("Test inputs", {
  expect_error(imaginator::SimulatePolicies(10))
  expect_error(imaginator::SimulatePolicies(10, PolicyYears = -1:-4))
  expect_error(imaginator::SimulatePolicies(10, PolicyYears = c(1,5)))
})

test_that("Form a policy table", {
  expect_silent(imaginator::SimulatePolicies(50, 2011:2015, Retention = 0.8, Growth = .1))
})
