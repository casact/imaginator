context("UniqueIDs")

test_that("UniqueIDs", {
  # N <- 200
  # set.seed(1234)
  # mojo <- GetUniqueIDs(N)
  #
  # set.seed(1234)
  # gonzo <- GetUniqueIDs(N)
  # expect_identical(mojo, gonzo)
  #
  # gonzo <- GetUniqueIDs(N, mojo)
  # expect_length(gonzo, N)
  #
  # cazart <- intersect(gonzo, mojo)
  # expect_length(cazart, 0)
  #
  # gonzo <- GetUniqueIDs(N * 10, mojo)
  # expect_length(gonzo, N * 10)
  #
  # cazart <- intersect(gonzo, mojo)
  # expect_length(cazart, 0)

})

test_that("IncrementID", {
  dfPolicy <- NewPolicyYear(200, 2000, StartID = 100)

  dfPolicy2 <- GrowPolicies(dfPolicy, Growth = 1)

  expect_gt(min(dfPolicy2$PolicyholderID), max(dfPolicy$PolicyholderID))

  dfPolicy <- SimulatePolicies(10, 2000:2001, Retention = 0, Growth = 1)

  expect_equal(length(unique(dfPolicy$PolicyholderID)), nrow(dfPolicy))

})
