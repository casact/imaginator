## ------------------------------------------------------------------------
dfPolicies <- SimulatePolicies(N = 10, PolicyYears = 5)

## ------------------------------------------------------------------------
dfPolicies <- SimulatePolicies(N = 100
                               , NumYears = 5
                               , Retention = 0.9
                               , Growth = 0.1)

## ------------------------------------------------------------------------
# Gradually expanding book of business
dfPolicies <- SimulatePolicies(N = 100
                               , PolicyYears = 5
                               , Retention = 0.9
                               , Growth = 0.2)

# Gradually contracting book of business
dfPolicies <- SimulatePolicies(N = 100
                               , PolicyYears = 5
                               , Retention = 0.8
                               , Growth = 0.1)

## ------------------------------------------------------------------------
library(imaginator)
set.seed(1234)
dfPolicy <- NewPolicies(N = 5000, 2001, 1)

## ----echo=FALSE, message=FALSE-------------------------------------------
library(dplyr)
dfDisplay <- dfPolicy %>% 
  arrange(PolicyID) %>% 
  head()

knitr::kable(dfDisplay)

## ------------------------------------------------------------------------
set.seed(1234)
dfGL <- NewPolicies(N = 5000, 2001, 1, AdditionalColumns = list(Line = "GL", State = "CA"))

## ----echo=FALSE----------------------------------------------------------
dfDisplay <- dfGL %>% 
  arrange(PolicyID) %>% 
  head()

knitr::kable(dfDisplay)

## ------------------------------------------------------------------------
dfRenewal <- RenewPolicies(dfPolicy, Retention = 0.65)

## ----echo=FALSE----------------------------------------------------------
dfDisplay <- dfRenewal %>% 
  arrange(PolicyID) %>% 
  head()

knitr::kable(dfDisplay)

## ------------------------------------------------------------------------
dfGrowth <- GrowPolicies(dfPolicy, Growth = 0.1)

## ------------------------------------------------------------------------
dfGrowGL <- GrowPolicies(dfGL, Growth = 0.1)

## ----echo=FALSE----------------------------------------------------------
dfDisplay <- dfGrowth %>% 
  arrange(PolicyID) %>% 
  head()

knitr::kable(dfDisplay)

## ------------------------------------------------------------------------
dfNextYear <- IncrementPolicyYear(dfGL, Retention = 0.65, Growth = 0.1)

## ----echo=FALSE----------------------------------------------------------
dfDisplay <- dfNextYear %>% 
  arrange(PolicyID) %>% 
  head()

knitr::kable(dfDisplay)

## ------------------------------------------------------------------------
dfTwoYears <- SimulatePolicies(1000, 2001:2002, Retention = .9, Growth = .05)

## ----echo=FALSE----------------------------------------------------------
dfDisplay <- dfNextYear %>% 
  arrange(PolicyID) %>% 
  head()

knitr::kable(dfDisplay)

