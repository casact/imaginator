## ------------------------------------------------------------------------
library(imaginator)
set.seed(1234)
dfPolicies <- SimulatePolicies(N = 10, NumYears = 5)

## ------------------------------------------------------------------------
dfPolicies <- SimulatePolicies(N = 10
                               , NumYears = 5
                               , Retention = 0.9
                               , Growth = 0.1)

## ------------------------------------------------------------------------
# Gradually expanding book of business
dfPolicies <- SimulatePolicies(N = 100
                               , NumYears = 5
                               , Retention = 0.9
                               , Growth = 0.2)

# Gradually contracting book of business
dfPolicies <- SimulatePolicies(N = 100
                               , NumYears = 5
                               , Retention = 0.8
                               , Growth = 0.1)

## ------------------------------------------------------------------------
dfPolicies <- SimulatePolicies(N = 100
                               , NumYears = 5
                               , Retention = 0.0
                               , Growth = 1.0)

## ------------------------------------------------------------------------
dfPolicies <- SimulatePolicies(N = 100
                               , NumYears = 5
                               , Retention = c(0.95, 0.9, 0.85, 0.8)
                               , Growth = c(0.25, 0.2, 0.1, 0.05))

## ------------------------------------------------------------------------
dfPolicies <- SimulatePolicies(N = 100
                               , NumYears = 5
                               , Retention = 0.9
                               , Growth = runif(4, .05, .15))

## ----eval=FALSE----------------------------------------------------------
#  dfRenewals <- RenewPolicies(dfMyData, Retention = 0.8)
#  dfNewBusiness <- GrowPolicies(dfMyData, 0.2)

## ------------------------------------------------------------------------
dfGL_CA <- SimulatePolicies(N = 5
                            , NumYears = 5
                            , AdditionalColumns = list(Line = "GL", State = "CA"))

## ----echo=FALSE----------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
dfGL_CA %>% 
  arrange(PolicyholderID) %>% 
  head() %>% 
  knitr::kable()

## ------------------------------------------------------------------------
dfGL_CA <- SimulatePolicies(N = 500
                            , NumYears = 5
                            , AdditionalColumns = list(Line = "GL", State = "CA")
                            , Retention = 0.5
                            , Growth = .01)

dfGL_NY <- SimulatePolicies(N = 50
                            , NumYears = 5
                            , AdditionalColumns = list(Line = "GL", State = "NY")
                            , Retention = 0.9
                            , Growth = .5)

dfGL <- dplyr::bind_rows(dfGL_CA, dfGL_NY)

## ------------------------------------------------------------------------
dfPolicies <- SimulatePolicies(N = 5, PolicyYears = 1:3, Growth = c(1, 0.5))

## ------------------------------------------------------------------------
dfPolicies %>% 
  mutate(PolicyYear = lubridate::year(PolicyEffectiveDate)) %>% 
  group_by(PolicyYear) %>% 
  summarise(MaxPolicyholderID = max(PolicyholderID)) %>% 
  knitr::kable()

