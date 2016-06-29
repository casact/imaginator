#' PoissonHelper
#'
#' This will return a poisson random number generator with a user-defined mean.
#'
#' @param lambda Expected value of poisson function
#'
#' @return
#'
#' A function
#'
#' @export
PoissonHelper <- function(lambda){
  poisHelp <- function(n){
    stats::rpois(n, lambda)
  }

  poisHelp

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
