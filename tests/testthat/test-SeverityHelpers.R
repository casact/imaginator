context("SeverityHelpers")

test_that("Gamma", {

  N <- 5000
  alpha <- 5
  beta <- 300

  set.seed(1234)
  vals <- rgamma(N, alpha, beta)

  myHelper <- GammaHelper(alpha, beta)
  set.seed(1234)
  other_vals <- myHelper(N)

  testthat::expect_equal(vals, other_vals)

})

test_that("Lognormal", {

  N <- 5000
  meanlog <- log(10000)
  sdlog <- meanlog * .5

  set.seed(1234)
  vals <- rlnorm(N, meanlog, sdlog)

  myHelper <- LognormalHelper(meanlog, sdlog)
  set.seed(1234)
  other_vals <- myHelper(N)

  testthat::expect_equal(vals, other_vals)

})
