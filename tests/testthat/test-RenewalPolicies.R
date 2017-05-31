context("RenewalPolicies")

dfLeapYear1 <- data.frame(PolicyEffectiveDate = as.Date("2014-03-01")
                          , PolicyExpirationDate = as.Date("2015-02-28")
                          , Exposure = 1
                          , stringsAsFactors = FALSE)

test_that("Leap year works", {
  dfRenew <- RenewPolicies(dfLeapYear1, 1.0)
  expect_equal(dfRenew$PolicyEffectiveDate, as.Date("2015-03-01"))
  expect_equal(dfRenew$PolicyExpirationDate, as.Date("2016-02-29"))
})

# test_that("Proper renewal", {
#   expect
# })
