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
#' @param freqFn A function to simulate frequency
#' @param freqArgs A list of arguments to the frequency function
#' @param occFn A function to simulate the month of occurrence
#' @param occArgs A list of arguments to the occurrence function
#' @param reportFn A function to simulate the report lag
#' @param reportArgs A list of arguments to the report lag function
#' @param payFn A function to simulate the lag until payment
#' @param payArgs A list of arguments to the payment lag function
#' @param sevFn A function to simulate the severity of payment
#' @param sevArgs A list of arguments to the severity function
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
OneSimulation <- function(AY = 0:5
                          , freqFn = stats::rnorm
                          , freqArgs = list(40, 7.745967)
                          , occFn = stats::runif
                          , occArgs = list(1, 12)
                          , reportFn = stats::rexp
                          , reportArgs = list(1/18)
                          , payFn = stats::rexp
                          , payArgs = list(1/12)
                          , sevFn = stats::rlnorm
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
                             , Claim)

  dfSimulation$ReportMonth <- dfSimulation$OccMonth + dfSimulation$ReportLag
  dfSimulation$PaymentMonth <- dfSimulation$ReportLag + dfSimulation$PaymentLag
  dfSimulation$ReportYear <- floor(dfSimulation$ReportMonth / 12)
  dfSimulation$ReportLagYear <- dfSimulation$ReportYear - dfSimulation$AY

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
