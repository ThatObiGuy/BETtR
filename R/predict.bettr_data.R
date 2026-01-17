#' Apply a list of models to a dataset featuring betting odds
#'
#' @param object The object on which the Skellam, Arima and ETS models may be applied.
#' @param odds A string which is the name of the column the user wishes to investigate.
#' @param h The number of hours to be forecast. Defaults to 36 hours.
#' @param model Which models are to be applied to the data. Defaults to "all".
#' @param ... Extra parameters to be passed to the Skellam model.
#'
#' @returns A list featuring the models fitted, their forecast plot and forecasted values.
#' @export
#'
#' @importFrom tsibble "fill_gaps" "scan_gaps"
#' @importFrom ggplot2 "ggplot"
#' @importFrom utils "tail"
#' @importFrom magrittr "%<>%"
#' @importFrom dplyr "count"
#'
#' @examples
#' data <- subset(football, home_team == "Sunderland")
#' data <- make_bettr(data)
#' predict(data, odds = "home_odds", h = 24, model = "all", M = 1000, tickSize = 0.01)
predict.bettr_data <- function(object, odds, h = 36, model = c("all", "skellam", "arima", "ets"), ...) {
  if(!inherits(object, "bettr_data"))
    stop("Calling predict.bettr_data on a non-bettr object")

  model <- match.arg(model)
  gaps <- ifelse(dplyr::count(tsibble::scan_gaps(object)) > 0, TRUE, FALSE)
  switch (model,
        "skellam" = {
          odds_vec <- object[[odds]]
          skellam <- skellam_model(object = object, odds_vec = odds_vec, h = h, ...)
          return(skellam)
          },

        "arima" = {
          if(gaps){
            warning("Gaps in time data detected.\n Filling missing values with tsibble::fill_gaps()")
            object %<>% tsibble::fill_gaps()
            odds_vec <- object[[odds]]
            arima <- arima_model(object = object, odds_vec = odds_vec, h = h)
          }else{
            odds_vec <- object[[odds]]
            arima <- arima_model(object = object, odds_vec = odds_vec, h = h)
          }
          return(arima)
          },

        "ets" = {
          if(gaps){
            warning("Gaps in time data will result in an interpolated ETS model.\n  Filling missing values with tsibble::fill_gaps()")
            object %<>% tsibble::fill_gaps()
            odds_vec <- object[[odds]]
            ets <- ets_model(object = object, odds_vec = odds_vec, h = h)
          }else{
            odds_vec <- object[[odds]]
            ets <- ets_model(object = object, odds_vec = odds_vec, h = h)
          }
          return(ets)
          },

        "all" = {
          if(gaps){
            warning("Gaps in time data will result in an interpolated ETS model.\n  Filling missing values with tsibble::fill_gaps()")
            temp <- odds
            odds_vec <- object[[odds]]
            skellam <- skellam_model(object = object, odds_vec = odds_vec, h = h, ...)
            object %<>% tsibble::fill_gaps()
            odds_vec <- object[[temp]] ## if it aint broke dont fix it I can make it look nicer later I just want it to work
            arima <- arima_model(object = object, odds_vec = odds_vec, h = h)
            ets <- ets_model(object = object, odds_vec = odds_vec, h = h)
          }else{
            odds_vec <- object[[odds]]
            arima <- arima_model(object = object, odds_vec = odds_vec, h = h)
            ets <- ets_model(object = object, odds_vec = odds_vec, h = h)
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


#' Arima model forecasting future betting odds
#'
#' @param object Object on which \code{arima_model} is performed on.
#' @param odds_vec A vector of odds.
#' @param h The number of hours to be forecast. Defaults to 36 hours.
#'
#' @returns A list featuring the forecasted values, the plot of the odds with respect to time and the model that was fitted
#' @export
#'
#' @importFrom fabletools "forecast" "model"
#' @importFrom lubridate "hours"
#' @importFrom forecast "autoplot"
#' @importFrom stats "na.omit"
#' @importFrom fable "ARIMA"
#'
#' @examples
#' match <- subset(football, football$home_team == "Brentford")
#' match <- tsibble::fill_gaps(make_bettr(match))
#' arima_model(match, match$away_odds, h = 48)
arima_model <- function(object, odds_vec, h = 36){
  arima_fit <- fabletools::model(object, arima = fable::ARIMA(odds_vec ~ pdq(p = 0:3, d = 0:2, q = 0:3)))
  arima_forecast <- fabletools::forecast(arima_fit, h = lubridate::hours(h))
  arima_forecast_plot <- forecast::autoplot(arima_forecast, stats::na.omit(object)) +
    ggplot2::labs(title = "ARIMA Forecast",
         y = "Odds",
         x = "Time")

  list <- list(
    forecast = arima_forecast,
    forecast_plot = arima_forecast_plot,
    model = arima_fit
  )

}

#' ETS model forecasting future betting odds
#'
#' @param object Object on which \code{ets_model} is performed on.
#' @param odds_vec A vector of odds.
#' @param h The number of hours to be forecast. Defaults to 36 hours.
#'
#' @returns A list featuring the forecasted values, the plot of the odds with respect to time and the model that was fitted
#' @export
#'
#' @importFrom fabletools "forecast" "model"
#' @importFrom lubridate "hours"
#' @importFrom forecast "autoplot"
#' @importFrom stats "approx" "na.omit"
#' @importFrom fable "ETS"
#'
#' @examples
#' match <- subset(football, football$home_team == "Crystal Palace")
#' match <- make_bettr(match) %>% fill_gaps()
#' ets_model(match, match$draw_odds, h = 72)
ets_model <- function(object, odds_vec, h = 36) {
  odds_interp <- stats::approx(object$logged_time, xout = object$logged_time , odds_vec, rule = 2)
  odds_vec <- odds_interp$y

  ets_fit <- fabletools::model(object, ets = fable::ETS(odds_vec))
  ets_forecast <- fabletools::forecast(ets_fit, h = lubridate::hours(h))
  ets_forecast_plot <- forecast::autoplot(ets_forecast, stats::na.omit(object)) +
    ggplot2::labs(title = "ETS Forecast",
         y = "Odds",
         x = "Time")

  list <- list(
    forecast = ets_forecast,
    forecast_plot = ets_forecast_plot,
    model = ets_fit
  )
}
#' Skellam model forecasting future betting odds
#'
#' @param object Object on which \code{skellam_model} is performed on.
#' @param odds_vec A vector of odds.
#' @param h The number of hours to be forecast. Defaults to 36 hours.
#' @param tickSize The minimum change in odds. Defaults to 0.01
#' @param M The number of simulations
#'
#' @returns A list featuring the forecasted values, the plot of the odds with respect to time and the model that was fitted
#' @export
#'
#' @importFrom skellam "rskellam"
#' @importFrom stats "quantile"
#' @importFrom ggplot2 "ggplot"
#' @importFrom ggplot2 "geom_line"
#' @importFrom ggplot2 "geom_ribbon"
#' @importFrom ggplot2 "labs"
#' @importFrom ggplot2 "aes"
#'
#' @examples
#' match <- subset(football, football$home_team == "Aston Villa")
#' match <- make_bettr(match) %>% fill_gaps()
#' ets_model(match, match$draw_odds, h = 18)
skellam_model <- function(object, odds_vec, h = 36, tickSize = 0.01, M = 2000) {
  ticks <- round(odds_vec/tickSize)
  delta_ticks <- c(0L, diff(ticks))
  time_intervals <- diff(as.numeric(object$logged_time))

  pos_count <- sum(delta_ticks[which(delta_ticks > 0)], na.rm = TRUE)
  neg_count <- abs(sum(delta_ticks[which(delta_ticks < 0)], na.rm = TRUE))

  lambda_pos_hat <- pos_count / (sum(time_intervals, na.rm = TRUE) / 3600)
  lambda_neg_hat <- neg_count / (sum(time_intervals, na.rm = TRUE) / 3600)

  sims_net <- matrix(
    skellam::rskellam(M * h, lambda1 = lambda_pos_hat, lambda2 = lambda_neg_hat),
    nrow = M, ncol = h, byrow = TRUE
  )
  sims_cum_ticks <- t(apply(sims_net, 1, cumsum))

  last_odds <- tail(odds_vec[!is.na(odds_vec)], 1)
  sims_odds <- sweep(sims_cum_ticks * tickSize, 2, last_odds, "+")

  forecast <- data.frame(
    horizon = 1:h,
    mean = colMeans(sims_odds),
    lower = apply(sims_odds, 2, quantile, 0.025),
    upper = apply(sims_odds, 2, quantile, 0.975)
  )

  start_time <- max(object$logged_time)
  forecast$logged_time <- start_time + lubridate::hours(forecast$horizon)

  forecast_plot <- ggplot2::ggplot(object) +
    ggplot2::geom_line(ggplot2::aes(x = logged_time, y = odds_vec)) +
    ggplot2::geom_line(data = forecast, ggplot2::aes(x = logged_time, y = mean), colour = "blue") +
    ggplot2::geom_ribbon(data = forecast, ggplot2::aes(x = logged_time, ymin = lower, ymax = upper), alpha = 0.2) +
    ggplot2::labs(title = "Skellam Forecast", y = "Odds", x = "Time")

  list(
  forecast = forecast,
  forecast_plot = forecast_plot,
  params = list(
    lambda_pos = lambda_pos_hat,
    lambda_neg = lambda_neg_hat
    )
  )
}
