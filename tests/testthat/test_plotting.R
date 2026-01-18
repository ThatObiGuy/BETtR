library(bettr)
library(dplyr)

test_that("plot.bettr_data errors on wrong class", {
  expect_error(plot.bettr_data(mtcars),"x must be of class 'bettr_data'")
})

football <- make_bettr(football, make_tsibble = FALSE)

test_that("plot.bettr_data errors on invalid odd argument", {
  expect_error(plot(football, odd = "bad"))
})

test_that("plot.bettr_data errors on invalid fixture column", {
  expect_error(plot(football, fixture = "nonexistent_column"),
    "Column 'nonexistent_column' not found in x"
  )
})

test_that("plot.bettr_data runs successfully with CF odds", {
  expect_silent(plot(football, odd = "CF"))

  result <- plot(football, odd = "CF")
  expect_true(
    inherits(result, "girafe") || inherits(result, "gg")
  )
})

test_that("plot.bettr_data runs successfully with OF odds", {
  expect_silent(plot(football, odd = "OF"))

  result <- plot(football, odd = "OF")
  expect_true(
    inherits(result, "girafe") || inherits(result, "gg")
  )
})

test_that("plot.bettr_data runs successfully with default non-specified arguments", {
  expect_silent(plot(football))

  result <- plot(football)
  expect_true(
    inherits(result, "girafe") || inherits(result, "gg")
  )
})

test_that("plot.bettr_data runs silently on bettr_data", {
  expect_silent(plot(football, odd = "CF"))
  expect_silent(plot(football, odd = "OF"))
})

test_that("plot.bettr_data works with valid fixture column", {
  football2 <- football |>
    mutate(fixture = paste(home_team, "vs", away_team))

  expect_silent(plot(football2, fixture = "fixture"))

  result_cf <- plot(football2, odd = "CF", fixture = "fixture")
  expect_true(
    inherits(result_cf, "girafe") || inherits(result_cf, "gg")
  )

  result_of <- plot(football2, odd = "OF", fixture = "fixture")
  expect_true(
    inherits(result_of, "girafe") || inherits(result_of, "gg")
  )
})

test_that("plot.bettr_data passes additional arguments to girafe", {
  expect_silent(plot(football, odd = "CF", width_svg = 8))
  expect_silent(plot(football, odd = "OF", width_svg = 10, height_svg = 6))

  result <- plot(football, odd = "CF", width_svg = 8)
  expect_true(
    inherits(result, "girafe") || inherits(result, "gg")
  )
})

test_that("plot.bettr_data works with combined parameters", {
  football2 <- football |>
    mutate(fixture = paste(home_team, "vs", away_team))

  # Testing combinations
  expect_silent(plot(football2, odd = "CF", fixture = "fixture", width_svg = 8))
  expect_silent(plot(football2, odd = "OF", fixture = "fixture", height_svg = 6))

  result <- plot(football2, odd = "CF", fixture = "fixture", width_svg = 8)
  expect_true(
    inherits(result, "girafe") || inherits(result, "gg")
  )
})

test_that("plot.bettr_data handles single event correctly", {
  single_event <- football |> filter(event_id == 1)

  expect_silent(plot(single_event, odd = "CF"))
  expect_silent(plot(single_event, odd = "OF"))

  result <- plot(single_event, odd = "CF")
  expect_true(
    inherits(result, "girafe") || inherits(result, "gg")
  )
})

test_that("plot.bettr_data generates correct plot titles", {
  # For CF odds
  result_cf <- plot(football, odd = "CF")
  if (inherits(result_cf, "gg")) {
    expect_equal(
      result_cf$labels$title,
      "Change over time of closing favourite odds"
    )
  }

  # For OF odds
  result_of <- plot(football, odd = "OF")
  if (inherits(result_of, "gg")) {
    expect_equal(
      result_of$labels$title,
      "Change over time of opening favourite odds"
    )
  }
})

test_that("plot.bettr_data generates correct axis labels", {
  result <- plot(football, odd = "CF")
  if (inherits(result, "gg")) {
    expect_equal(result$labels$x, "Time")
    expect_equal(result$labels$y, "Odds Change (%)")
  }
})

test_that("plot.bettr_data uses correct legend labels", {
  # Without fixture (should use Event ID)
  result_no_fixture <- plot(football, odd = "CF")
  if (inherits(result_no_fixture, "gg")) {
    expect_equal(result_no_fixture$labels$colour, "Event ID")
  }

  # With fixture (should use Fixture)
  football <- football |>
    mutate(fixture = paste(home_team, "vs", away_team))

  result_with_fixture <- plot(football, odd = "CF", fixture = "fixture")
  if (inherits(result_with_fixture, "gg")) {
    expect_equal(result_with_fixture$labels$colour, "Fixture")
  }
})

