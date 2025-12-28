library(bettr)

test_that("plot.bettr_data errors on wrong class", {
  expect_error(plot.bettr_data(mtcars),"x must be of class 'bettr_data'")
})

test_that("plot.bettr_data errors on invalid odd argument", {
  expect_error(plot(football, odd = "bad"))
})

test_that("plot.bettr_data runs silently on bettr_data", {
  expect_silent(plot(football, odd = "CF"))
  expect_silent(plot(football, odd = "OF"))
})
