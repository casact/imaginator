context("FrequencyHelpers")

test_that("Poisson", {
  N <- 5000
  lambda <- 5
  set.seed(1234)
  poissons <- rpois(N, lambda)

  myHelper <- PoissonHelper(lambda)
  set.seed(1234)
  other_poissons <- myHelper(N)

  testthat::expect_equal(poissons, other_poissons)

})
