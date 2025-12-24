predict.bettr_data <- function(data, h, model = c("all", "skellam", "arima", "ets"), ...) {
  if(!inherits(data, "bettr_data"))
    stop("Calling predict.bettr_data on a non-bettr object")

  model <- match.arg(model)

  switch (model,
        "skellam" = {
          skellam <- skellam_model(data = data, h = h, ...)
          return(skellam)
          },

        "arima" = {
          arima <- data %>% fill_gaps() %>% arima_model(h = h)
          return(arima)
          },

        "ets" = {
          warning("Gaps in time data will result in an interpolated ETS model.\n  Filling missing values with tsibble::fill_gaps()")
          ets <- data %>% fill_gaps() %>% ets_model(h = h)
          return(ets)
          },

        "all" = {
          warning("Gaps in time data will result in an interpolated ETS model.\n  Filling missing values with tsibble::fill_gaps()")
          skellam <- skellam_model(data = data, h = h, ...)
          data %<>% fill_gaps()
          arima <- arima_model(data = data, h = h)
          ets <- ets_model(data = data, h = h)
          return(list(
            skellam = skellam,
            arima = arima,
            ets = ets
          ))
          }
    )
}

arima_model <- function(data, h){
  t_cut <- max(data$logged_time) - lubridate::hours(h)
  split_rows_on <- which(min(abs(t_cut - data$logged_time)) == abs(t_cut - data$logged_time))
  data_train <- data[1:split_rows_on, ]
  data_test <- data[-(1:split_rows_on), ]

  arima_fit <- data_train %>% model(arima = ARIMA(home_odds ~ pdq(p = 0:3, d = 0:2, q = 0:3)))
  arima_forecast <- arima_fit %>%  forecast(h = 2*h)
  arima_forecast_plot <- arima_forecast %>% autoplot(na.omit(data)) +
    labs(title = "ARIMA Forecast",
         y = "Home Odds",
         x = "Time")

  list <- list(
    accuracy = accuracy(arima_forecast, data_test),
    forecast = arima_forecast,
    arima_forecast_plot = arima_forecast_plot,
    model = arima_fit
  )

}

ets_model <- function(data, h) {
  t_cut <- max(data$logged_time) - lubridate::hours(h)
  split_rows_on <- which( min(abs(t_cut - data$logged_time)) == abs(t_cut - data$logged_time))
  odds_interp <- approx(data$logged_time, xout = data$logged_time , data$home_odds, rule = 2)
  data$home_odds <- odds_interp$y
  data_train <- data[1:split_rows_on, ]
  data_test <- data[-(1:split_rows_on), ]

  ets_fit <- data_train %>% model(ets = ETS(home_odds))
  ets_forecast <- ets_fit %>% forecast(h = 2*h)
  ets_forecast_plot <- ets_forecast %>% autoplot(na.omit(data)) +
    labs(title = "ETS Forecast",
         y = "Home Odds",
         x = "Time")

  list <- list(
    accuracy = accuracy(ets_forecast, data_test),
    forecast = ets_forecast,
    forecast_plot = ets_forecast_plot,
    model = ets_fit
  )
}
skellam_model <- function(data, tickSize = 0.01, h = 36, M = 2000) {
  t_cut <- max(data$logged_time) - lubridate::hours(h)
  split_rows_on <- which( min(abs(t_cut - data$logged_time)) == abs(t_cut - data$logged_time))
  data_train <- data[1:split_rows_on, ]
  data_test <- data[-(1:split_rows_on), ]

  ticks <- round(data_train$home_odds/tickSize)
  delta_ticks <- c(0L, diff(ticks))
  time_intervals <- diff(as.numeric(data_train$logged_time))

  pos_count <- sum(delta_ticks[which(delta_ticks > 0)], na.rm = TRUE)
  neg_count <- abs(sum(delta_ticks[which(delta_ticks < 0)], na.rm = TRUE))

  # divide by 3600 because our units of time are hours
  lambda_pos_hat <- pos_count / (sum(time_intervals) / 3600)
  lambda_neg_hat <- neg_count / (sum(time_intervals) / 3600)

  sims_net <- matrix(
    rskellam(M * h, lambda1 = lambda_pos_hat, lambda2 = lambda_neg_hat),
    nrow = M, ncol = h, byrow = TRUE
  )
  sims_cum_ticks <- t(apply(sims_net, 1, cumsum))

  last_odds <- tail(data_train$home_odds[!is.na(data_train$home_odds)], 1)
  sims_odds <- sweep(sims_cum_ticks * tickSize, 2, last_odds, "+")

  forecast <- tibble(
    horizon = 1:h,
    mean = colMeans(sims_odds),
    lower = apply(sims_odds, 2, quantile, 0.025),
    upper = apply(sims_odds, 2, quantile, 0.975)
  )

  start_time <- max(data_train$logged_time)
  forecast$logged_time <- start_time + hours(forecast$horizon)
  accuracy <- tibble(
    RMSE = sqrt(mean((data_test$home_odds - forecast$mean[1:nrow(data_test)])^2, na.rm = TRUE)),
    MAE  = mean(abs(data_test$home_odds - forecast$mean[1:nrow(data_test)]), na.rm = TRUE)
  )

  forecast_plot <- ggplot(na.omit(data)) +
    geom_line(aes(logged_time, home_odds)) +
    geom_line(data = forecast, aes(logged_time, mean), colour = "blue") +
    geom_ribbon(data = forecast, aes(logged_time, ymin = lower, ymax = upper), alpha = 0.2) +
    labs(title = "Skellam Forecast", y = "Home Odds", x = "Time")

  list(
  accuracy = accuracy,
  forecast = forecast,
  forecast_plot = forecast_plot,
  params = list(
    lambda_pos = lambda_pos_hat,
    lambda_neg = lambda_neg_hat
    )
  )
}
