predict.bettr_data <- function(data, h) {
  if(sum(has_gaps(data)$.gaps) > 0) {
    warning("Gaps in time data will result in an interpolated ETS model.\n  Filling missing values with tsibble::fill_gaps()")
    data2 <- fill_gaps(data)
    arima <- arima_model(data2, h)
    ets <- ets_model(data2, h)
  } else{
    arima <- arima_model(data, h)
    ets <- ets_model(data, h)
  }
  skellam <- skellam_model(data, h = h, tickSize = 0.01, M = 2000)

  list(
    arima = arima,
    ets = ets,
    skellam = skellam
  )
}
arima_model <- function(data, h){
  t_cut <- max(data$logged_time) - lubridate::hours(h)
  split_rows_on <- which(min(abs(t_cut - data$logged_time)) == abs(t_cut - data$logged_time))
  data_train <- data[1:split_rows_on, ]
  data_test <- data[-(1:split_rows_on), ]

  arima_fit <- data_train %>% model(arima = ARIMA(home_odds ~ pdq(p = 0:3, d = 0:2, q = 0:3)))

  arima_forecast <- arima_fit %>%  forecast(h = h)

  odds_plot <- na.omit(data) %>% ggplot(aes(x = logged_time, y = home_odds)) +
    geom_line()

  arima_forecast_plot <- arima_forecast %>% autoplot(na.omit(data)) +
    labs(title = "ARIMA Forecast",
         y = "Home Odds",
         x = "Time")


  list <- list(
    accuracy = accuracy(arima_forecast, data_test),
    forecast = arima_forecast,
    odds_plot = odds_plot,
    arima_forecast_plot = arima_forecast_plot,
    model = arima_fit
  )

}

ets_model <- function(data, h) {
  t_cut <- max(data$logged_time) - lubridate::hours(h)
  split_rows_on <- which( min(abs(t_cut - data$logged_time)) == abs(t_cut - data$logged_time))
  data_train <- data[1:split_rows_on, ]
  data_test <- data[-(1:split_rows_on), ]

  y_interp <- approx(data_train$logged_time, xout = data_train$logged_time , data_train$home_odds, rule = 2)
  data_train$home_odds <- y_interp$y

  ets_fit <- data_train %>% model(ets = ETS(home_odds ~ error(c("A", "M")) + trend(c("A", "N", "Ad")) + season(c("N", "A", "M"))))
  ets_forecast <- ets_fit %>% forecast(h = h)

  odds_plot <- na.omit(data) %>% ggplot(aes(x = logged_time, y = home_odds)) +
    geom_line()

  ets_forecast_plot <- ets_forecast %>% autoplot(na.omit(data)) +
    labs(title = "ETS Forecast",
         y = "Home Odds",
         x = "Time")

  list <- list(
    accuracy = accuracy(ets_forecast, data_test),
    forecast = ets_forecast,
    odds_plot = odds_plot,
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

  odds_plot <- na.omit(data) %>%
    ggplot(aes(x = logged_time, y = home_odds)) +
    geom_line() +
    labs(title = "Odds time series")

  forecast_plot <- ggplot(na.omit(data_train)) +
    geom_line(aes(logged_time, home_odds)) +
    geom_line(data = forecast, aes(logged_time, mean), colour = "blue") +
    geom_ribbon(data = forecast, aes(logged_time, ymin = lower, ymax = upper), alpha = 0.2) +
    labs(title = "SKELLAM Forecast", y = "Home Odds", x = "Time")

  list(
  accuracy = accuracy,
  forecast = forecast,
  odds_plot = odds_plot,
  forecast_plot = forecast_plot,
  model = list(
    lambda_pos = lambda_pos_hat,
    lambda_neg = lambda_neg_hat
    )
  )
}
