sample_or_rep <- function(x, n) {
  if (distributions3::is_distribution(x))
    distributions3::random(x, n)
  else
    rep(x, n)
}

maybe_wrap_in_list <- function(x) {
  if (length(x) == 1 && !rlang::is_bare_list(x))
    list(x)
  else
    x
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
