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

#' LognormalHelper
#'
#' This will create a random number generator for the lognormal function
#'
#' @param meanlog Mean on the log scale
#' @param sdlog Standard deviation on the log scale
#'
#' @export
LognormalHelper <- function(meanlog, sdlog){

  lnHelp <- function(n){
    stats::rlnorm(n, meanlog, sdlog)
  }
}
