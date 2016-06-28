context("NewPolicies")

test_that("New policies", {
  df <- NewPolicies(50, 2001, 1)
  expect_equal(nrow(df), 50)
  expect_equal(ncol(df), 4)
})


