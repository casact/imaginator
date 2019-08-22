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
