#' PoissonHelper
#'
#' This will return a list of poisson random number generator functions.
#'
#' @param lambda Expected value of poisson function
#' @param SingletonList Boolean indicating whether to return a single function as a list. Default FALSE.
#'
#' @return
#'
#' A list of functions or a single function (see Details).
#'
#' @details
#' The function is vectorised in the sense that one may pass in a vector of function parameters and receive a list of
#' functions with the same length.
#'
#' If only one parameter is supplied and the user does not ask for a list to be returned, this will return a function.
#'
#' @examples
#'
#' myFuncs <- PoissonHelper(c(10, 20))
#' myFuncs[[1]](10)
#' myFuncs[[2]](10)
#'
#' myFunc <- PoissonHelper(10)
#' is.function(myFunc)
#' myFunc(10)
#'
#' myFunc <- PoissonHelper(10, SingletonList = TRUE)
#' is.list(myFunc)
#' myFunc[[1]](10)
#'
#' @export
PoissonHelper <- function(lambda, SingletonList = FALSE){
  poisHelp <- lapply(lambda, function(x){
    function(n){
      stats::rpois(n, x)
    }
  })

  if (length(poisHelp) == 1 & !SingletonList){
    poisHelp <- poisHelp[[1]]
  }
  poisHelp

}

#' BinomialHelper
#'
#' This will return a list of poisson random number generator functions.
#'
#' @param size number of trials
#' @param prob probability of success on each trial
#' @param SingletonList Boolean indicating whether to return a single function as a list. Default FALSE.
#'
#' @return
#'
#' A list of functions.
#'
#' @details
#' The lambda parameter may be a vector.
#'
#' @examples
#'
#' myFuncs <- BinomialHelper(size = 100, prob = c(.9, .1))
#' myFuncs[[1]](4)
#'
#' @export
BinomialHelper <- function(size, prob, SingletonList){

  size <- PadParameters(size, prob)
  prob <- PadParameters(prob, size)

  funcHelp <- mapply(size, prob, FUN = function(size, prob){
    function(n){
      stats::rbinom(n, size = size, prob = prob)
    }
  })

  if (length(funcHelp) == 1 & !SingletonList){
    funcHelp <- funcHelp[[1]]
  }

  funcHelp

}

# TODO:
# negative binomial
# zero-inflated binomial
# zero-inflated poisson
