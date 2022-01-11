## -----------------------------------------------------------------------------
library(imaginator)
set.seed(1234)
tbl_policies <- policies_simulate(
  n = 10,
  num_years = 5)

## -----------------------------------------------------------------------------
tbl_policies <- policies_simulate(
  n = 10,
  num_years = 5,
  retention = 0.9,
  growth = 0.1)

## -----------------------------------------------------------------------------
# Gradually expanding book of business
tbl_policies <- policies_simulate(
  n = 100,
  num_years = 5,
  retention = 0.9,
  growth = 0.2)

# Gradually contracting book of business
tbl_policies <- policies_simulate(
  n = 100,
  num_years = 5,
  retention = 0.8,
  growth = 0.1)

## -----------------------------------------------------------------------------
tbl_policies <- policies_simulate(
  n = 100,
  num_years = 5,
  retention = 0.0,
  growth = 1.0)

## -----------------------------------------------------------------------------
tbl_policies <- policies_simulate(
  n = 100,
  num_years = 5,
  retention = c(0.95, 0.9, 0.85, 0.8),
  growth = c(0.25, 0.2, 0.1, 0.05))

## -----------------------------------------------------------------------------
tbl_policies <- policies_simulate(
  n = 100,
  num_years = 5,
  retention = 0.9, 
  growth = runif(4, .05, .15))

## ----eval=FALSE---------------------------------------------------------------
#  dfRenewals <- policies_renew(dfMyData, retention = 0.8)
#  dfNewBusiness <- policies_grow(dfMyData, 0.2)

## -----------------------------------------------------------------------------
dfGL_CA <- policies_simulate(
  n = 5, 
  num_years = 5, 
  additional_columns = list(Line = "GL", State = "CA"))

## ----echo=FALSE---------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
dfGL_CA %>% 
  arrange(policyholder_id) %>% 
  head() %>% 
  knitr::kable()

## -----------------------------------------------------------------------------
dfGL_CA <- policies_simulate(
  n = 500, 
  num_years = 5, 
  additional_columns = list(Line = "GL", State = "CA"), 
  retention = 0.5, 
  growth = .01)

dfGL_NY <- policies_simulate(
  n = 50, 
  num_years = 5, 
  additional_columns = list(Line = "GL", State = "NY"), 
  retention = 0.9, 
  growth = .5)

dfGL <- dplyr::bind_rows(dfGL_CA, dfGL_NY)

## -----------------------------------------------------------------------------
tbl_policies <- policies_simulate(
  n = 5, 
  policy_years = 1:3, 
  growth = c(1, 0.5))

## -----------------------------------------------------------------------------
tbl_policies %>% 
  mutate(PolicyYear = lubridate::year(policy_effective_date)) %>% 
  group_by(PolicyYear) %>% 
  summarise(MaxPolicyholderID = max(policyholder_id)) %>% 
  knitr::kable()

