#' Apply a list of models to a dataset featuring betting odds
#'
#' A wrapper function for \code{\link{arima_model}}, \code{\link{ets_model}} and \code{\link{skellam_model}}
#' with some built-in checks for univariate time series and NA value handling necessary for arima and ets.
#'
#' @param object An object of class \code{bettr_data}
#' @param odds A character string which is the name of the column the user wishes to investigate.
#' @param h The number of hours to be forecast. 36 hours by default.
#' @param model Models to be applied to the data. Defaults to "all".
#' @param has_na Should be true if there are NA values present in \code{object} and/or
#' if \code{object} has gaps in time data. If not specified, the function checks for gaps and
#' NAs and passes the result of the checks to the named functions.
#' @param ... Extra parameters to be passed to the Skellam model.
#'
#' @returns A list featuring the models fitted, their forecast plot and forecasted values.
#' @export
#' @author Ivan Cakic - <\email{ivan.cakic.2023@@mumail.ie}>
#'
#' @importFrom tsibble "fill_gaps" "has_gaps"
#' @importFrom ggplot2 "ggplot"
#' @importFrom utils "tail"
#' @importFrom magrittr "%<>%"
#'
#' @examples
#' data <- subset(football, home_team == "Sunderland")
#' data <- make_bettr(data)
#' predict(data, odds = "home_odds", h = 24, model = "all", sims = 1000, tickSize = 0.01)
predict.bettr_data <- function(object, odds, h = 36, model = c("all", "skellam", "arima", "ets"), has_na = NULL, ...) {
  if(!inherits(object, "bettr_data"))
    stop("Calling predict.bettr_data on a non-bettr object")

  model <- match.arg(model)
  if(model != "skellam" && is.null(has_na)){ # We only care about NAs for arima and ets
    has_na <- ifelse(any(is.na(object)) || tsibble::has_gaps(object)$.gaps, TRUE, FALSE)
  }else{
    has_na = FALSE
  }
  switch (model,
        "skellam" = {
          odds_vec <- object[[odds]]
          skellam <- skellam_model(object = object, odds_vec = odds_vec, h = h, ...)
          return(skellam)
          },

        "arima" = {
          if(has_na){
            warning("NA values detected.\n NA values result in interpolated models")
            object %<>% tsibble::fill_gaps()
            odds_vec <- object[[odds]]
            arima <- arima_model(object = object, odds_vec = odds_vec, h = h, has_na = has_na)
          }else{
            odds_vec <- object[[odds]]
            arima <- arima_model(object = object, odds_vec = odds_vec, h = h, has_na = has_na)
          }
          return(arima)
          },

        "ets" = {
          if(has_na){
            warning("NA values detected.\n NA values result in interpolated models")
            object %<>% tsibble::fill_gaps()
            odds_vec <- object[[odds]]
            ets <- ets_model(object = object, odds_vec = odds_vec, h = h, has_na = has_na)
          }else{
            odds_vec <- object[[odds]]
            ets <- ets_model(object = object, odds_vec = odds_vec, h = h, has_na = has_na)
          }
          return(ets)
          },

        "all" = {
          if(has_na){
            warning("NA values detected.\n NA values result in interpolated models")
            odds_vec <- object[[odds]]
            skellam <- skellam_model(object = object, odds_vec = odds_vec, h = h, ...)
            object %<>% tsibble::fill_gaps()
            odds_vec <- object[[odds]]
            arima <- arima_model(object = object, odds_vec = odds_vec, h = h, has_na = has_na)
            ets <- ets_model(object = object, odds_vec = odds_vec, h = h, has_na = has_na)
          }else{
            odds_vec <- object[[odds]]
            arima <- arima_model(object = object, odds_vec = odds_vec, h = h, has_na = has_na)
            ets <- ets_model(object = object, odds_vec = odds_vec, h = h, has_na = has_na)
            skellam <- skellam_model(object = object, odds_vec = odds_vec, h = h, ...)
          }
          return(list(
            skellam = skellam,
            arima = arima,
            ets = ets
          ))
          }
    )
}


#' Arima-based betting odds forecasting
#'
#' Creates an Arima model given an object of class "\code{bettr_data}" and a vector of associated odds.
#' Gives the model specification, the predicted values and a plot of the predicted and forecasted
#' odds over time, similarly to \code{\link{ets_model}} and \code{\link{skellam_model}}
#'
#' @param object An object of class \code{bettr_data}.
#' @param odds_vec A numeric vector of odds.
#' @param h The number of hours to be forecast. 36 hours by default..
#' @param has_na Should be true if there are NA values present in \code{object} and/or \code{odds_vec}. Note
#' that \code{predict.bettr_data} automatically checks whether NA values and/or gaps exist and deals
#' with that accordingly. False by default.
#'
#' @returns A list featuring the forecasted values, a forecast plot and forecasted values.
#'
#' @export
#' @author Ivan Cakic - <\email{ivan.cakic.2023@@mumail.ie}>
#'
#' @importFrom fabletools "forecast" "model"
#' @importFrom lubridate "hours"
#' @importFrom stats "na.omit"
#' @importFrom fable "ARIMA"
#' @importFrom ggplot2 "ggplot" "geom_line" "geom_ribbon" "labs" "aes" "theme_bw"
#'
#' @examples
#' match <- subset(football, football$home_team == "Brentford")
#' match <- tsibble::fill_gaps(make_bettr(match))
#' arima_model(match, match$away_odds, h = 48, has_na = TRUE)
arima_model <- function(object, odds_vec, h = 36, has_na = FALSE){
  if(has_na){
    odds_interp <- stats::approx(object$logged_time, xout = object$logged_time , odds_vec, rule = 2)
    object$new_odds <- odds_interp$y
  }else{
    object$new_odds <- odds_vec
  }

  arima_fit <- fabletools::model(object, arima = fable::ARIMA(new_odds))
  arima_forecast <- fabletools::forecast(arima_fit, h = lubridate::hours(h))

  arima_forecast_plot <- ggplot2::ggplot(object) +
    ggplot2::geom_line(ggplot2::aes(x = logged_time, y = new_odds)) +
    ggplot2::geom_line(data = arima_forecast, ggplot2::aes(x = logged_time, y = .mean), colour = "blue") +
    ggplot2::geom_ribbon(data = arima_forecast, ggplot2::aes(x = logged_time, ymin = apply(cbind(.mean), 2, quantile, 0.025, na.rm = TRUE), ymax = apply(cbind(.mean), 2, quantile, 0.975, na.rm = TRUE)), alpha = 0.2) +
    ggplot2::labs(title = "ARIMA Forecast", y = "Odds", x = "Time") +
    ggplot2::theme_bw()


  list <- list(
    forecast = arima_forecast,
    forecast_plot = arima_forecast_plot,
    model = arima_fit
  )
}


#' ETS-based betting odds forecasting
#'
#' Creates an ETS model given an object of class "\code{bettr_data}" and a vector of associated odds.
#' Gives the model specification, the predicted values and a plot of the predicted and forecasted
#' odds over time, similarly to \code{\link{arima_model}} and \code{\link{skellam_model}}
#'
#' @param object An object of class \code{bettr_data}.
#' @param odds_vec A numeric vector of odds.
#' @param h The number of hours to be forecast. 36 hours by default.
#' @param has_na True if there are NA values present in \code{object} and/or \code{odds_vec}. Note
#' that \code{predict.bettr_data} automatically checks whether NA values and/or gaps exist in the two and deals
#' with that accordingly. False by default.
#'
#' @returns A list featuring the forecasted values, a forecast plot and forecasted values.
#'
#' @export
#' @author Ivan Cakic - <\email{ivan.cakic.2023@@mumail.ie}>
#'
#' @importFrom fabletools "forecast" "model"
#' @importFrom lubridate "hours"
#' @importFrom stats "approx" "na.omit"
#' @importFrom fable "ETS"
#' @importFrom ggplot2 "ggplot" "geom_line" "geom_ribbon" "labs" "aes" "theme_bw"
#'
#' @examples
#' match <- subset(football, football$home_team == "Crystal Palace")
#' match <- tsibble::fill_gaps(make_bettr(match))
#' ets_model(match, match$draw_odds, h = 72, has_na = TRUE)
ets_model <- function(object, odds_vec, h = 36, has_na = FALSE) {
  if(has_na){
    odds_interp <- stats::approx(object$logged_time, xout = object$logged_time , odds_vec, rule = 2)
    object$new_odds <- odds_interp$y
  }else{
    object$new_odds <- odds_vec
  }

  ets_fit <- fabletools::model(object, ets = fable::ETS(new_odds))
  ets_forecast <- fabletools::forecast(ets_fit, h = lubridate::hours(h))

  ets_forecast_plot <- ggplot2::ggplot(object) +
    ggplot2::geom_line(ggplot2::aes(x = logged_time, y = new_odds)) +
    ggplot2::geom_line(data = ets_forecast, ggplot2::aes(x = logged_time, y = .mean), colour = "blue") +
    ggplot2::geom_ribbon(data = ets_forecast, ggplot2::aes(x = logged_time, ymin = apply(cbind(.mean), 2, quantile, 0.025, na.rm = TRUE), ymax = apply(cbind(.mean), 2, quantile, 0.975, na.rm = TRUE)), alpha = 0.2) +
    ggplot2::labs(title = "ETS Forecast", y = "Odds", x = "Time") +
    ggplot2::theme_bw()

  list <- list(
    forecast = ets_forecast,
    forecast_plot = ets_forecast_plot,
    model = ets_fit
  )
}
#' Skellam based betting odds forecasting
#'
#' Creates a Skellam model given an object of class "\code{bettr_data}" and a vector of associated odds.
#' Gives the model specification, the predicted values and a plot of the predicted and forecasted
#' odds over time, similarly to \code{\link{arima_model}} and \code{\link{ets_model}}
#'
#' @param object An object of class \code{bettr_data} on which \code{skellam_model} is performed on.
#' @param odds_vec A numeric vector of odds.
#' @param h The number of hours to be forecast. 36 hours by default.
#' @param tickSize The minimum possible change in odds. 0.01 by default.
#' @param sims The number of simulations. 2000 by default.
#'
#' @returns A list featuring the forecasted values, a forecast plot and forecasted values.
#'
#' @export
#' @author Ivan Cakic - <\email{ivan.cakic.2023@@mumail.ie}>
#'
#' @importFrom skellam "rskellam"
#' @importFrom stats "quantile" "na.omit"
#' @importFrom ggplot2 "ggplot" "geom_line" "geom_ribbon" "labs" "aes" "theme_bw"
#' @importFrom magrittr "%<>%"
#'
#' @examples
#' match <- subset(football, football$home_team == "Aston Villa")
#' match <- tsibble::fill_gaps(make_bettr(match))
#' ets_model(match, match$draw_odds, h = 18)
skellam_model <- function(object, odds_vec, h = 36, tickSize = 0.01, sims = 2000) {
  if(any(is.na(object)) || any(is.na(odds_vec))){
    object %<>% na.omit()
    odds_vec %<>% na.omit()
  }

  ticks <- round(odds_vec/tickSize)
  delta_ticks <- c(0L, diff(ticks))
  time_intervals <- diff(as.numeric(object$logged_time))

  pos_count <- sum(delta_ticks[which(delta_ticks > 0)], na.rm = TRUE)
  neg_count <- abs(sum(delta_ticks[which(delta_ticks < 0)], na.rm = TRUE))

  lambda_pos_hat <- pos_count / (sum(time_intervals, na.rm = TRUE) / 3600)
  lambda_neg_hat <- neg_count / (sum(time_intervals, na.rm = TRUE) / 3600)

  sims_net <- matrix(
    skellam::rskellam(sims * h, lambda1 = lambda_pos_hat, lambda2 = lambda_neg_hat),
    nrow = sims, ncol = h, byrow = TRUE
  )
  sims_cum_ticks <- t(apply(sims_net, 1, cumsum))

  last_odds <- tail(odds_vec[!is.na(odds_vec)], 1)
  sims_odds <- sweep(sims_cum_ticks * tickSize, 2, last_odds, "+")

  forecast <- data.frame(
    horizon = 1:h,
    mean = colMeans(sims_odds),
    lower = apply(sims_odds, 2, quantile, 0.025, na.rm = TRUE),
    upper = apply(sims_odds, 2, quantile, 0.975, na.rm = TRUE)
  )

  start_time <- max(object$logged_time)
  forecast$logged_time <- start_time + lubridate::hours(forecast$horizon)

  forecast_plot <- ggplot2::ggplot(object) +
    ggplot2::geom_line(ggplot2::aes(x = logged_time, y = odds_vec)) +
    ggplot2::geom_line(data = forecast, ggplot2::aes(x = logged_time, y = mean), colour = "blue") +
    ggplot2::geom_ribbon(data = forecast, ggplot2::aes(x = logged_time, ymin = lower, ymax = upper), alpha = 0.2) +
    ggplot2::labs(title = "Skellam Forecast", y = "Odds", x = "Time") +
    ggplot2::theme_bw()

  list(
  forecast = forecast,
  forecast_plot = forecast_plot,
  params = list(
    lambda_pos = lambda_pos_hat,
    lambda_neg = lambda_neg_hat
    )
  )
}
