# This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
# If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
