context("claims_by_first_report")

test_that("test inputs", {

  expect_error(imaginator::pol_char(c('state','line')))

  expect_silent(imaginator::pol_char(
    list(state = c('AL','AR','AZ')
                     , line = c('auto', 'home')
                     , uw_score = LETTERS[1:5])
    ,list(state = c(.5, .2, .3)
                       , line = c(.6, .4)
                       , uw_score = c(.15, .25, .4, .2, .1))
  ))

  expect_error(imaginator::pol_char(
    list(state = c('AL','AR','AZ','DE')
                     , line = c('auto', 'home')
                     , uw_score = LETTERS[1:5])
    ,list(state = c(.5, .2, .3)
                       , line = c(.6, .4)
                       , uw_score = c(.15, .25, .4, .2, .1))
  ))

  expect_error(imaginator::pol_char(
     list(c('AL','AR','AZ')
                     , line = c('auto', 'home')
                     , uw_score = LETTERS[1:5])
    , list(c(.5, .2, .3)
                       , line = c(.6, .4)
                       , uw_score = c(.15, .25, .4, .2, .1))
  ))

  expect_error(imaginator::pol_char(
    list(state = c('AL','AR','AZ')
                     , line = c('auto', 'home')
                     , uw_score = LETTERS[1:5])
    , list(state = c(.5, .2, .3)
                       , line = c('.6', '.4')
                       , uw_score = c(.15, .25, .4, .2, .1))
  ))

})
