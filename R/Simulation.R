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
#' @title OneSimulation
#'
#' @param AY Vector of accident years
#'
#' @importFrom magrittr %>%
#'
OneSimulation <- function(AY){

  nClaims <- round(rnorm(length(AY), 40, 60))
  nClaims <- ifelse(nClaims < 1, 1, nClaims)

  AY <- list()
  for (i in seq_along(nClaims)){
    AY[[i]] <- rep(i - 1, nClaims[i])
  }

  AY <- unlist(AY)

  MonthLag <- lapply(nClaims, runif, 0, 11) %>% unlist()
  ReportLag <- lapply(nClaims, rexp, 1/18) %>% unlist()
  PaymentLag <- lapply(nClaims, rexp, 1/12) %>% unlist()

  OccMonth <- 12 * AY + MonthLag
  ReportMonth <- OccMonth + ReportLag
  PaymentMonth <- ReportLag + PaymentLag

  mean <- 10400
  sd <- 34800
  logMean <- log(mean) - log((sd / mean) ^ 2 + 1) / 2
  logSD <- sqrt(log((sd / mean) ^ 2 + 1))
  Claim <- lapply(nClaims, rlnorm, logMean, logSD) %>% unlist()

  dfSimulation <- data.frame(AY, MonthLag, ReportLag, PaymentLag
                             , OccMonth, ReportMonth, PaymentMonth
                             , Claim) %>%
    mutate(ReportYear = floor(ReportMonth / 12)
           , ReportLagYear = ReportYear - AY)

  dfSimulation
}

#' @export
#'
#' @title UpperTriangle
#'
#' @param df A simulation data.frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#'
UpperTriangle <- function(df){

  dfUpperTriangle <- df %>%
    tidyr::complete(AY, ReportLag, fill = 0) %>%
    filter(ReportMonth < 60) %>%
    group_by(AY, ReportLagYear) %>%
    summarise(Count = n()) %>%
    arrange(AY, ReportLagYear) %>%
    mutate(Count = ifelse(is.na(Count), 0, Count)
           , Count = cumsum(Count)) %>%
    tidyr::spread(ReportLagYear, Count)

  dfUpperTriangle

  }
