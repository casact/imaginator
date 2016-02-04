library(dplyr)

set.seed(1234)

a <- 0:5
nClaims <- round(rnorm(length(a), 40, 60))
nClaims <- ifelse(nClaims < 1, 1, nClaims)

a <- list()
for (i in seq_along(nClaims)){
  a[[i]] <- rep(i - 1, nClaims[i])
}

a <- unlist(a)

M <- lapply(nClaims, runif, 0, 11) %>% unlist()
Q <- lapply(nClaims, rexp, 1/18) %>% unlist()
P <- lapply(nClaims, rexp, 1/12) %>% unlist()

m <- 12 * a + M
r <- m + Q
p <- r + P

mean <- 10400
sd <- 34800
logMean <- log(mean) - log((sd / mean) ^ 2 + 1) / 2
logSD <- sqrt(log((sd / mean) ^ 2 + 1))
C <- lapply(nClaims, rlnorm, logMean, logSD) %>% unlist()

dfSimulation <- data.frame(a, M, Q, P, m, r, p, C)
