#' NormalHelper
#'
#' Returns a normal distribution with optional bounds on the returned values
#'
#' @param mean The mean
#' @param sd The standard deviation
#' @param lowerBound Lower boundary of the return values
#' @param upperBound Upper bound of the returned values
#' @param SingletonList Boolean indicating whether to return a single function as a list. Default FALSE.
#'
#' @return A function
#'
#' @importFrom stats rnorm
#'
#' @export
NormalHelper <- function(mean, sd, lowerBound, upperBound, SingletonList = FALSE){
  hasLower <- !missing(lowerBound)
  hasUpper <- !missing(upperBound)

  mean <- PadParameters(mean, sd)
  sd <- PadParameters(sd, mean)
  if (hasLower) {
    lowerBound <- PadParameters(lowerBound, sd)
  } else {
    lowerBound <- NA
  }
  if (hasUpper) {
    upperBound <- PadParameters(upperBound, sd)
  } else {
    upperBound <- NA
  }

  funcHelp <- mapply(mean, sd, FUN = function(x, y){
    function(n){
      z <- stats::rnorm(n, x, y)
      z <- pmax(z, lowerBound, na.rm = TRUE)
      z <- pmin(z, upperBound, na.rm = TRUE)
      z
    }
  })

  if (length(funcHelp) == 1 & !SingletonList){
    funcHelp <- funcHelp[[1]]
  }

  funcHelp

}

#' UniformHelper
#'
#' Returns a uniform distribution function
#'
#' @param min The minimum value
#' @param max The maximum value
#' @param SingletonList Boolean indicating whether to return a single function as a list. Default FALSE.
#'
#' @return A function
#'
#' @importFrom stats runif
#'
#' @export
UniformHelper <- function(min, max, SingletonList = FALSE){

  min <- PadParameters(min, max)
  max <- PadParameters(max, min)

  funcHelp <- mapply(min, max, FUN = function(x, y){
    function(n){
      stats::runif(n, x, y)
    }
  })

  if (length(funcHelp) == 1 & !SingletonList){
    funcHelp <- funcHelp[[1]]
  }

  funcHelp

}

#' FixedHelper
#'
#' Returns the same, non-stochastic, value
#'
#' @param Fixed A scalar value
#' @param SingletonList Boolean indicating whether to return a single function as a list. Default FALSE.
#'
#' @return A function
#'
#' @export
FixedHelper <- function(Fixed, SingletonList = FALSE){

  funcHelp <- lapply(Fixed, function(x){
    function(n){
      rep(x, n)
    }
  })

  if (length(funcHelp) == 1 & !SingletonList){
    funcHelp <- funcHelp[[1]]
  }

  funcHelp
}
