#' @title Policy Characteristics
#' @name pol_char
#' @description A structure for storing policy characteristics and how frequently they occur.
#' @param char_levels A named list of vectors. The vector names are characteristic names and the vectors contain all possible levels of each characteristic
#' @param frequencies A names list of numeric vectors. The names must match those in char_levels. The vectors must be positive numbers which indicate relative freqeuncy of each level within each characteristic. If the values in a vector do not sum to 1.0 then they will be off-balanced in the output.
#' @returns \code{pol_char()} checks that the inputs conform to requirements and returns a pol_char object which is a list containing char_list and freq_list
#' @details \code{pol_char()} does not do any calculations. It checks that the list which is input has a valid
#' structure to be a pol_char object. The benefit is that functions which use pol_char objects do not have to run
#' tests to check validity of the structure. The only have to do \code{is.pol_char(obj)}
#' @export
#' @examples
#' test_char <- pol_char(
#'   list(state = c('AL','AR','AZ')
#'        , line = c('auto', 'home')
#'        , uw_score = LETTERS[1:5])
#'   ,list(state = c(.5, .2, .3)
#'         , line = c(.6, .4)
#'         , uw_score = c(.15, .25, .4, .2, .1))
#' )
#'
#' test_char <- pol_char(
#'   list(state = c('AL','AR','AZ')
#'        , line = c('auto', 'home')
#'        , uw_score = LETTERS[1:5])
#'   ,list(state = c(5, 2, 3)
#'         , line = c(6, 4)
#'         , uw_score = c(15, 25, 40, 20, 10))
#' )
pol_char <- function(char_levels, frequencies) {
  assertthat::assert_that(is.list(char_levels))
  assertthat::assert_that(is.list(frequencies))
  assertthat::assert_that(length(char_levels) == length(frequencies))
  assertthat::assert_that(all(names(char_levels) != ''))
  assertthat::assert_that(all(sapply(char_levels, length) == sapply(frequencies, length)))
  assertthat::assert_that(all(names(char_levels) == names(frequencies)))
  assertthat::assert_that(length(names(char_levels)) == length(unique(names(char_levels))))
  assertthat::assert_that(all(sapply(frequencies, is.numeric)))
  # check that levels of each characteristic are unique
  assertthat::assert_that(all(sapply(char_levels, function(v) length(v) == length(unique(v)))))
  return(structure(
    list(char_list = char_levels
         , freq_list = frequencies)
    , class = 'pol_char')
    )
}

#' is.pol_char
#'
#' @param plc A \code{pol_list} object
#' @returns Boolean value indicating if plc is a pol_char object
#' @export
is.pol_char <- function(plc) inherits(plc, 'pol_char')

#' @title Fill the additional characteristics of a simulated policy table, or create
#' a simulated policy table filling in additional characteristics from a pol_char object
#'
#' @param plc a \code{pol_char} object used to fill additional fields
#' @param tbl_policy A simulated policy table. If NULL then one will be created
#' @param ... Used to for parameters to call \code{policies_simulate} if needed.
#' Do not include \code{additional_columns}; those will be filled with pol_char
#' @export
fill_addl <- function(plc, tbl_policy = NULL, ...) {
  assertthat::assert_that(is.pol_char(plc))
  if (is.null(tbl_policy)) {
    misc_params <- names(list(...))
    if (!('n' %in% misc_params)) {
      stop('When tbl_policy is not supplied, a value for "n" must be supplied to create a simulated policy table')
    }
    if (!(any(c('policy_years', 'num_years') %in% misc_params))) {
        stop("At least one of policy_years or num_years must be given.")
    }
    addl_cols <- list()
    for (k in 1:length(plc$char_list)) {
      addl_cols <- append(addl_cols, list(NA))
    }
    names(addl_cols) <- names(plc$char_list)
    tbl_policy <- policies_simulate(..., additional_columns = addl_cols)
  }
  assertthat::assert_that(nrow(tbl_policy) > 0)

  plc_tbl_nms <- names(plc$char_list)
  pol_tbl_nms <- grep('policy_effective_date|policy_expiration_date|exposure|policyholder_id'
                      , names(tbl_policy), value = TRUE, invert = TRUE)

  if (length(setdiff(plc_tbl_nms, pol_tbl_nms)) > 0) {
    message(paste('The following characteristics will be added to the policy table:'
                  , paste(setdiff(plc_tbl_nms, pol_tbl_nms), collapse = ', '), '\n'))
    for (nm in setdiff(plc_tbl_nms, pol_tbl_nms)) {
      tbl_policy[[nm]] <- NA
    }
  }

  if (length(setdiff(pol_tbl_nms, plc_tbl_nms)) > 0) {
    warning(paste('The following policy characteristics are not affected:'
                  , paste(setdiff(pol_tbl_nms, plc_tbl_nms), collapse = ', ')))
  }

  for (nm in plc_tbl_nms) {
    tbl_policy[[nm]] <- sample(x = plc$char_list[[nm]]
                         , size = nrow(tbl_policy)
                         , replace = TRUE
                         , prob = plc$freq_list[[nm]])
  }

  return(tbl_policy)
}

#' @name print.pol_char
#' @param plc A \code{pol_char} object
#' @param ... optional parameters for future use
print.pol_char <- function(plc, ...) {
  assertthat::assert_that(is.pol_char(plc))
  charnames <- names(plc$char_list)
  top_levels <- sapply(plc$char_list,
                       function(v) {
                         top <- paste(v[1:(min(3, length(v)))], collapse = ', ')
                         top <- ifelse(length(v) > 3, paste0(top, ', ...'), top)
                       }
                       )
  top_frq <- sapply(plc$freq_list,
                       function(v) {
                         top <- paste(v[1:(min(3, length(v)))], collapse = ', ')
                         top <- ifelse(length(v) > 3, paste0(top, ', ...'), top)
                       }
                    )
  top_levels <- as.vector(top_levels)
  top_frq <- as.vector(top_frq)
  cat('Characteric names and top three levels:\n')
  result <- ascii::ascii(data.frame(
    char_name = charnames
    , levels = top_levels
    , frequencies = top_frq
  ), include.rownames = FALSE
  , type = 'rest'
  , header = F)
  print(result)
}
