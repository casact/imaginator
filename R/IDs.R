#' @importFrom stringi stri_rand_strings
GetUniqueIDs <- function(N, ExistingIDs){

  if (missing(ExistingIDs)) ExistingIDs <- character(0)

  ID_length <- as.integer(log(N, 36)) + 1

  numPortion <- stringi::stri_rand_strings(N, ID_length, pattern = "[0-9]")

  charPortion <- stringi::stri_rand_strings(N, ID_length, pattern = "[A-Z]")

  uniqueID <- paste(numPortion, charPortion, sep = "-")

  newOnes <- setdiff(uniqueID, ExistingIDs)
  repeats <- intersect(uniqueID, ExistingIDs)
  needNovel <- N - length(newOnes)

  uniqueID <- unique(uniqueID)

  if (needNovel > 0){
    uniqueID <- c(uniqueID, GetUniqueIDs(needNovel, uniqueID))
  }

  uniqueID

}
