#' @importFrom distributions random is_distribution
sample_or_rep <- function(x, n) {
  if (distributions::is_distribution(x))
    distributions::random(x, n)
  else
    rep(x, n)
}

PadParameters <- function(paramA, paramB){
  num_a <- length(paramA)
  num_b <- length(paramB)
  if (num_a < num_b){
    indices <- rep_len(seq.int(num_a), length.out = num_b)
    paramA <- paramA[indices]
  }

  paramA
}

GetFirstFunction <- function(obj){
  if (!is.list(obj)) {
    warning("Object is not a list. Returning NULL.")
  }

  obj <- obj[[1]]

  if (!is.function(obj)){
    warning("Object is not a function. Returning NULL.")
    obj <- NULL
  }

  obj
}

PutFunctionInList <- function(obj){
  if (!is.function(obj)){
    warning("Object is not a function. Returning NULL.")
    obj <- NULL
  }

  obj <- list(obj)

  obj
}
