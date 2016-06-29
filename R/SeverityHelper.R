#' GammaHelper
#'
#' This will create a random number generator for the gamma function.
#'
#' @param alpha Double for the \eqn{\alpha} parameter
#' @param beta Double for the \eqn{\beta} parameter
#'
#' @export
GammaHelper <- function(alpha, beta){

  gammaHelp <- function(n){
    stats::rgamma(n, alpha, beta)
  }

  gammaHelp

}
