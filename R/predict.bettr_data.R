predict.bettr_data <- function(data, odds, ) {


}
match <- subset(football, home_team == "Sunderland")

arima_model <- function(match){
  match_train <-  match[1:(0.8*nrow(match)),]
  match_test <- match[-(1:(0.8*nrow(match))),]

  model_fit <- match_train %>% model(ARIMA(home_odds ~ pdq(p = 0:3, d = 0:2, q = 0:3)))

  forecast <- model_fit %>%  forecast(h = "8000 minutes")

  odds_plot <- match %>% ggplot(aes(x = logged_time, y = home_odds)) +
    geom_line()

  forecast_plot <- forecast %>% autoplot(match_train) +
    labs(title = "ARIMA Forecast",
         y = "Home Odds",
         x = "Time")

  list <- list(
    accuracy = accuracy(forecast, match_test),
    odds_plot = odds_plot,
    forecast_plot = forecast_plot,
    models = model_fit
  )
}

ets_model <- function(match) {
  match_train <-  match[1:(0.8*nrow(match)),]
  match_test <- match[-(1:(0.8*nrow(match))),]

  model_fit <- match_train %>% model(ETS(back_home ~ error(c("A", "M")) + trend(c("A", "N", "Ad")) + season(c("N", "A", "M"))))

  forecast <- model_fit %>% forecast(h = "60 minutes")

  odds_plot <- match %>% ggplot(aes(x = time_data, y = back_home)) +
    geom_line()

  forecast_plot <- forecast %>% autoplot(match_train) +
    labs(title = "ETS Forecast",
         y = "Home Odds",
         x = "Time")

  list <- list(
    accuracy = accuracy(forecast, match_test),
    odds_plot = odds_plot,
    forecast_plot = forecast_plot,
    models = fit
  )
}


skellam_model <- function(match, tickSize = 0.01, interval = 60, H = 600, M = 2000) {
  match_train <- match[1:(0.8 * nrow(match)), ]
  match_test  <- match[-(1:(0.8 * nrow(match))), ]

  match_train %<>%
    mutate(
      ticks = round(home_odds / tickSize),
      delta_ticks = c(0L, diff(ticks))
    )

  t_numeric <- as.numeric(match_train$logged_time)
  match_train$interval <- floor(t_numeric / interval)

  expand_ticks <- function(delta) {
    if (is.na(delta) || delta == 0L) return(integer(0))
    if (delta > 0L) return(rep(1L, delta))
    rep(-1L, abs(delta))
  }

  by_interval <- split(match_train$delta_ticks, match_train$interval)

  agg <- lapply(by_interval, function(vec) {
    events <- unlist(lapply(vec, expand_ticks), use.names = FALSE)
    tibble(
      N_pos = sum(events == 1L),
      N_neg = sum(events == -1L),
      Net   = sum(events)
    )
  }) |> bind_rows()

  m <- mean(agg$Net)
  v <- var(agg$Net)

  lambda_pos <- max((v + m) / 2, 0)
  lambda_neg <- max((v - m) / 2, 0)

  sims_net <- matrix(
    rskellam(M * H, lambda_pos, lambda_neg),
    nrow = M, ncol = H, byrow = TRUE
  )

  sims_cum_ticks <- t(apply(sims_net, 1, cumsum))

  last_odds <- tail(match_train$home_odds[!is.na(match_train$home_odds)], 1)

  sims_odds <- sweep(
    sims_cum_ticks * tickSize,
    2,
    last_odds,
    "+"
  )

  forecast <- tibble(
    horizon = 1:H,
    mean = colMeans(sims_odds),
    lower = apply(sims_odds, 2, quantile, 0.025),
    upper = apply(sims_odds, 2, quantile, 0.975)
  )

  start_time <- max(match_train$logged_time, na.rm = TRUE)
  forecast$logged_time <- start_time + minutes(forecast$horizon)

  test_aligned <- match_test %>%
    filter(logged_time <= max(forecast$logged_time)) %>%
    select(logged_time, home_odds)

  accuracy <- tibble(
    RMSE = sqrt(mean((test_aligned$home_odds - forecast$mean[1:nrow(test_aligned)])^2, na.rm = TRUE)),
    MAE  = mean(abs(test_aligned$home_odds - forecast$mean[1:nrow(test_aligned)]), na.rm = TRUE)
  )

  odds_plot <- match %>%
    ggplot(aes(x = logged_time, y = home_odds)) +
    geom_line() +
    labs(title = "Odds time series")

  forecast_plot <- ggplot() +
    geom_line(data = match_train, aes(logged_time, home_odds)) +
    geom_line(data = forecast, aes(logged_time, mean), colour = "blue") +
    geom_ribbon(data = forecast, aes(logged_time, ymin = lower, ymax = upper), alpha = 0.2) +
    labs(title = paste("Skellam forecast", H, "minutes"), y = "Home Odds", x = "Time")

  list(
  accuracy = accuracy,
  odds_plot = odds_plot,
  forecast_plot = forecast_plot,
  model = list(
    lambda_pos = lambda_pos,
    lambda_neg = lambda_neg,
    tickSize = tickSize,
    interval = interval
    )
  )
}


skellam_loglik <- function(param, x) {
  lambda1 <- param[1]
  lambda2 <- param[2]

  if (lambda1 < 0 || lambda2 < 0) return(-Inf)

  sum(dskellam(x, lambda1, lambda2, log = TRUE), na.rm = TRUE)
}



skellam_loglik(initial_params, agg$Net)

mle_results_custom <- maxLik(
  logLik = skellam_loglik,
  start = initial_params,
  x = agg$Net,
  method = "NM"
)
