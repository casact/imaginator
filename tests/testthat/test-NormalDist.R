context("NormalDist")

test_that("multiplication works", {
  mean <- 1.8
  sd <- .08
  N <- 10
  set.seed(1234)
  norms <- rnorm(N, mean, sd)

  myHelper <- NormalHelper(mean, sd)
  set.seed(1234)
  otherNorms <- myHelper(N)

  testthat::expect_equal(norms, otherNorms)

  myHelper <- NormalHelper(mean, sd, .01, 4)
  set.seed(1234)
  limitedNorms <- myHelper(N)

  testthat::expect_equal(norms, otherNorms)
})
