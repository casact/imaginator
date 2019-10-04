#' @title Claims by link ratio
#'
#' @description
#' Given a data frame of claims, this will simulate claim development by applying a (possibly) random link ratio.
#'
#' @param tbl_claims A claims data frame
#' @param links A vector of the same length as `lags` of factors, or their
#'   distributions, determining how severities change from one evaluation date
#'   to the next.
#' @param lags A vector of lags
#'
#' @details
#' This function will apply the link ratio algorithm at an individual claim level.
#'
#' @return A claims data frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
#' @examples
#'
#' tbl_policy <- policy_year_new(10, 2001)
#' tbl_claims <- claims_by_first_report(
#'                tbl_policy,
#'                frequency = 10,
#'                payment_severity = 100,
#'                lags = 1)
#' tbl_claims <- claims_by_link_ratio(
#'                tbl_claims,
#'                links = c(1.25, 1.1, 1.05),
#'                lags = 1:4)
#'
#' @export
claims_by_link_ratio <- function(tbl_claims, links, lags){

  links <- maybe_wrap_in_list(links)
  for (iLink in seq.int(length(links))) {

    tbl_next_lag <- tbl_claims[tbl_claims$lag == lags[iLink], ]

    # samplingx
    links <- sample_or_rep(links[[iLink]], nrow(tbl_next_lag))

    tbl_next_lag$payment_amount <- tbl_next_lag$payment_amount * links
    tbl_next_lag$lag <- lags[iLink + 1]

    tbl_claims <- rbind(tbl_claims, tbl_next_lag)
  }

  tbl_claims
}
