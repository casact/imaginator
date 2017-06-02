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
#' @importFrom stats rnorm
#'
#' @export
NormalHelper <- function(mean, sd, lowerBound, upperBound){
  hasLower <- !missing(lowerBound)
  hasUpper <- !missing(upperBound)

  myMean <- mean
  mySD <- sd
  if (hasLower) myLower <- lowerBound
  if (hasUpper) myUpper <- upperBound
  theFunc <- function(n){
    z <- rnorm(n, myMean, mySD)
    if (hasLower) z <- pmax(z, myLower)
    if (hasUpper) z <- pmin(z, myUpper)
    z
  }

  theFunc

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
#' @importFrom stats runif
#'
#' @export
UniformHelper <- function(min, max){
  theFunc <- function(n){
    runif(n, min, max)
  }

  theFunc
}

#' FixedHelper
#'
#' Returns the same, non-stochastic, value
#'
#' @param Fixed A scalar value
#'
#' @return A function
#'
#' @export
FixedHelper <- function(Fixed){
  theFunc <- function(n){
    rep(Fixed, n)
  }
  theFunc
}
