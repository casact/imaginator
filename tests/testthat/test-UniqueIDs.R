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
  dfPolicy <- policy_year_new(200, 2000, start_id = 100)

  dfPolicy2 <- policies_grow(dfPolicy, growth = 1)

  expect_gt(min(dfPolicy2$policyholder_id), max(dfPolicy$policyholder_id))

  dfPolicy <- policies_simulate(10, 2000:2001, retention = 0, growth = 1)

  expect_equal(length(unique(dfPolicy$policyholder_id)), nrow(dfPolicy))

})
