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

test_that("errors when non Posixct values in logged time", {
  x <- football
  x$logged_time <- as.character(x$logged_time[1])
  expect_error(make_bettr(x),"logged time column must be in the format POSIXct: 'YYYY-MM-DD HH:MM:SS'",
               fixed = TRUE)
})

test_that("Cant have NA Values in logged-time column + Warning for errors", {
  x <- football
  x$logged_time[1] <- NA
  expect_warning(
    expect_error(
      make_bettr(x),"Logged time column has missing values."),
    "Data has 1 row with NA values")
})

test_that("custom column names work", {
  generatedExample <- data.frame(idOfEvent = 1:3,
  log_time = as.POSIXct(c("2024-01-01 12:00:00",
  "2024-01-02 12:00:00","2024-01-03 12:00:00")),
  home_odds = c(2.55, 1.55, 3.11), away_odds = c(2.11, 3.22, 1.33),
  drawOdds = c(3.36, 3.00, 3.0))
  x1 <- make_bettr(generatedExample, draw_odds = "drawOdds",
    away_odds = "away_odds", logged_time = "log_time", event_id = "idOfEvent", make_tsibble = FALSE)
  expect_s3_class(x1, "bettr_data")
})

test_that("event_id converts to factor", {
  x <- make_bettr(football, make_tsibble = FALSE)
  expect_true(is.factor(x$event_id))
})

