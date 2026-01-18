library(bettr)
test_that("make_bettr converts to  a tsibble", {
  x <- make_bettr(football, make_tsibble = TRUE)
  expect_s3_class(x, "tbl_ts")
})
