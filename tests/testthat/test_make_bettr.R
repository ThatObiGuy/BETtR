library(bettr)
library(testthat)
test_that("make_bettr converts to  a tsibble", {
  x <- make_bettr(football, make_tsibble = TRUE)
  expect_s3_class(x, "tbl_ts")
})

test_that("make_bettr warns user over making bettr objects bettr", {
  x <- make_bettr(football, make_tsibble = FALSE)
  expect_error(make_bettr(x),"already of class 'bettr_data'")
})

test_that("error when function is missing", {
  bad_data <- football[, -which(names(football) == "home_odds")]
  expect_error(make_bettr(bad_data),"data input is missing the following columns: "
)})


test_that("errors when odds columns arent numeric", {
  x <- football
  x$away_odds <- as.character(x$away_odds)
  expect_error(make_bettr(x),"(home_odds, away_odds, draw_odds) must be numeric.",
               fixed = TRUE)
})
