#' NormalHelper
#'
#' Returns a normal distribution with optional bounds on the returned values
#'
#' @param mean The mean
#' @param sd The standard deviation
#' @param lowerBound Lower boundary of the return values
#' @param upperBound Upper bound of the returned values
#'
#' @return A function
#'
#' @export
NormalHelper <- function(mean, sd, lowerBound, upperBound){
  hasLower <- !missing(lowerBound)
  hasUpper <- !missing(upperBound)
  theFunc <- function(n){
    z <- rnorm(n, mean, sd)
    if (hasLower) z <- pmax(z, lowerBound)
    if (hasUpper) z <- pmin(z, upperBound)
    z
  }
}

#' UniformHelper
#'
#' Returns a uniform distribution function
#'
#' @param min The minimum value
#' @param max The maximum value
#'
#' @return A function
#'
#' @export
UniformHelper <- function(min, max){
  theFunc <- function(n){
    runif(n, min, max)
  }

  theFunc
}

#' FixedVal
#'
#' Returns the same, non-stochastic, value
#'
#' @param Fixed A scalar value
#'
#' @return A function
#'
#' @export
FixedVal <- function(Fixed){
  theFunc <- function(n){
    rep(Fixed, n)
  }
  theFunc
}
