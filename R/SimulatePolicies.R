#' @export
#'
#' @examples
#'
#' py <- 2001:2010
#'
#' N <- 5000
#'
#' exposure <- 1
#' rate <- 1
#'
#' retention <- 0.9
SimulatePolicies <- function(PolicyYears, N1, growth, retention, exposure, rate)
{

  numYears <- length(PolicyYears)
  days <- rep(365, length(numYears)) + ifelse(lubridate::leap_year(PolicyYears), 1, 0)
  days <- days-1

  effectiveDates <- ymd(paste(PolicyYears, "01", "01", sep="-"))
  effectiveDates <- effectiveDates + days

  df <- data.frame()
}
