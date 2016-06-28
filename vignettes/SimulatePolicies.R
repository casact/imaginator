## ------------------------------------------------------------------------
library(imagine)
dfPolicy <- NewPolicies(N = 5000, 2001, 1)
dfPolicy2 <- RenewPolicies(dfPolicy, Renewal = 0.65)
dfPolicy3 <- RenewPolicies(dfPolicy2, Renewal = 0.65)

