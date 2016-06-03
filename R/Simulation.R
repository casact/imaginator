# AY <- a
# MonthLag <- M
# ReportLag <- Q
# PaymentLag <- P
# OccMonth <- m
# RepMonth <- r
# PaymentMonth <- p
# Claim <- C

#' @export
#'
#' @description
#'
#' Construct a data frame of simulated claims.
#'
#' @title OneSimulation
#'
#' @param AY Vector of accident years
#' @param freq A function and arguments to simulate frequency
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
OneSimulation <- function(AY = 0:5
                          , freqFn = rnorm
                          , freqArgs = list(40, 7.745967)
                          , occFn = runif
                          , occArgs = list(1, 12)
                          , reportFn = rexp
                          , reportArgs = list(1/18)
                          , payFn = rexp
                          , payArgs = list(1/12)
                          , sevFn = rlnorm
                          , sevArgs = list(8, 1.5815085)){

  AY <- AY - min(AY)

  numPeriods <- length(AY)
  lstAY <- vector("list", numPeriods)
  MonthLag <- vector("list", numPeriods)
  ReportLag <- vector("list", numPeriods)
  PaymentLag <- vector("list", numPeriods)
  Claim <- vector("list", numPeriods)

  freqArgs$n <- numPeriods
  nClaims <- do.call(freqFn, freqArgs)
  nClaims <- as.integer(nClaims)
  nClaims <- ifelse(nClaims < 1, 1, nClaims)

  for (i in seq_along(nClaims)){
    lstAY[[i]] <- rep(AY[i], nClaims[i])
    occArgs$n <- lstAY[[i]]
    reportArgs$n <- lstAY[[i]]
    payArgs$n <- lstAY[[i]]
    sevArgs$n <- lstAY[[i]]
    MonthLag[[i]] <- do.call(occFn, occArgs)
    ReportLag[[i]] <- do.call(reportFn, reportArgs)
    PaymentLag[[i]] <- do.call(payFn, payArgs)
    Claim[[i]] <- do.call(sevFn, sevArgs)
  }

  AY <- unlist(lstAY)
  MonthLag <- unlist(MonthLag)
  ReportLag <- unlist(ReportLag)
  PaymentLag <- unlist(PaymentLag)
  Claim <- unlist(Claim)

  OccMonth <- 12 * AY + MonthLag

  dfSimulation <- data.frame(AY
                             , MonthLag
                             , ReportLag
                             , PaymentLag
                             , OccMonth
                             , Claim) %>%
    mutate(ReportMonth = OccMonth + ReportLag
           , PaymentMonth = ReportLag + PaymentLag
           , ReportYear = floor(ReportMonth / 12)
           , ReportLagYear = ReportYear - AY)

  dfSimulation
}

#' #' @export
#' #'
#' #' @title UpperTriangle
#' #'
#' #' @param df A simulation data.frame
#' #'
#' #' @importFrom magrittr %>%
#' #' @importFrom dplyr filter
#' #' @importFrom dplyr group_by
#' #' @importFrom dplyr summarise
#' #' @importFrom dplyr arrange
#' #' @importFrom dplyr mutate
#' #' @importFrom tidyr complete
#' #'
#' UpperTriangle <- function(df){
#'
#'   dfUpperTriangle <- df %>%
#'     tidyr::complete(AY, ReportLagYear, fill = list(Count = 0)) %>%
#'     filter(ReportLayYear < 5) %>%
#'     group_by(AY, ReportLagYear) %>%
#'     summarise(Count = n()) %>%
#'     arrange(AY, ReportLagYear) %>%
#'     mutate(Count = ifelse(is.na(Count), 0, Count)
#'            , Count = cumsum(Count)) %>%
#'     tidyr::spread(ReportLagYear, Count)
#'
#'   dfUpperTriangle
#'
#' }
