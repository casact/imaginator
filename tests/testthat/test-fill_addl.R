context("claims_by_first_report")

test_that("fill_addl", {

  dfPolicy <- policies_simulate(100, num_years = 3)
  dfPolicy2 <- policies_simulate(100, num_years = 3
                                 , additional_columns = list(
                                   state = NA
                                   , line = NA
                                   , uw_score = NA
                                   , ntr = NA
                                 ))
  dfPolicy3 <- policies_simulate(100, num_years = 3
                                 , additional_columns = list(
                                   state = NA
                                   , line = NA
                                 ))


  test_chars <- pol_char(
    list(state = c('AL','AR','AZ')
                     , line = c('auto', 'home')
                     , uw_score = LETTERS[1:5])
    ,list(state = c(.5, .2, .3)
                       , line = c(.6, .4)
                       , uw_score = c(.15, .25, .4, .2, .1))
    )

  less_chars <- pol_char(
    list(state = c('AL','AR','AZ')
                     , line = c('auto', 'home')
    )
    ,list(state = c(.5, .2, .3)
                       , line = c(.6, .4)
          )
    )

  expect_silent(imaginator::fill_addl(plc = test_chars, n=50, num_years = 2))
  expect_warning(imaginator::fill_addl(less_chars, dfPolicy2))
  expect_message(imaginator::fill_addl(test_chars, dfPolicy3))
  expect_error(imaginator::fill_addl(test_chars, n=50))
  expect_error(imaginator::fill_addl(test_chars, num_years=50))



})
