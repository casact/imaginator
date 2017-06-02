context("OtherHelpers")

test_that("Normal dist", {
  mean <- 1.8
  sd <- .08
  N <- 10
  set.seed(1234)
  norms <- rnorm(N, mean, sd)

  myHelper <- NormalHelper(mean, sd)
  set.seed(1234)
  otherNorms <- myHelper(N)

  testthat::expect_equal(norms, otherNorms)

  myHelper <- NormalHelper(mean, sd, lowerBound = .01, upperBound = 4)
  set.seed(1234)
  otherLimitedNorms <- myHelper(N)

  set.seed(1234)
  limitedNorms <- pmin(rnorm(N, mean, sd), 4)
  limitedNorms <- pmax(limitedNorms, .01)
  testthat::expect_equal(limitedNorms, otherLimitedNorms)
})


test_that("Uniform dist", {
  set.seed(1234)
  N <- 5000
  lower <- -1000
  upper <- 1000
  uniforms <- runif(N, lower, upper)
  set.seed(1234)
  other_uniforms <- UniformHelper(lower, upper)(N)
  testthat::expect_equal(uniforms, other_uniforms)
})

test_that("Fixed vals", {
  testthat::expect_equal(c(4,4), FixedHelper(4)(2))
})
