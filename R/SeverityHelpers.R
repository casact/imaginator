#' GammaHelper
#'
#' This will create a random number generator for the gamma function.
#'
#' @param alpha Double for the \eqn{\alpha} parameter
#' @param beta Double for the \eqn{\beta} parameter
#' @param SingletonList Boolean indicating whether to return a single function as a list. Default FALSE.
#'
#' @export
GammaHelper <- function(alpha, beta, SingletonList = FALSE){

  alpha <- PadParameters(alpha, beta)
  beta <- PadParameters(beta, alpha)

  funcHelp <- mapply(alpha, beta, FUN = function(x, y){
    function(n){
      stats::rgamma(n, x, y)
    }
  })

  if (length(funcHelp) == 1 & !SingletonList){
    funcHelp <- funcHelp[[1]]
  }

  funcHelp

}

#' LognormalHelper
#'
#' This will create a random number generator for the lognormal function
#'
#' @param meanlog Mean on the log scale
#' @param sdlog Standard deviation on the log scale
#' @param SingletonList Boolean indicating whether to return a single function as a list. Default FALSE.
#'
#' @export
LognormalHelper <- function(meanlog, sdlog, SingletonList = FALSE){

  meanlog <- PadParameters(meanlog, sdlog)
  sdlog <- PadParameters(sdlog, meanlog)

  funcHelp <- mapply(meanlog, sdlog, FUN = function(x, y){
    function(n){
      stats::rlnorm(n, x, y)
    }
  })

  if (length(funcHelp) == 1 & !SingletonList){
    funcHelp <- funcHelp[[1]]
  }

  funcHelp

}
