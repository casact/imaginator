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

# TODO:
# binomial
# zero-inflated binomial
# zero-inflated poisson
