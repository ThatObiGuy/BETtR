make_bettr <- function(data,
                       event_id = "event_id",
                       logged_time = "logged_time",
                       home_odds = "home_odds",
                       away_odds = "away_odds",
                       draw_odds = "draw_odds", ...) {
  #checking if it has the 5 required columns to be a bettr object
  req <- c(event_id, logged_time, home_odds, away_odds, draw_odds)
  allReqNotInData <- setdiff(req, names(data))
  if(length(allReqNotInData) != 0) {
    stop("data input is missing the following columns: ",
         paste(allReqNotInData, collapse = ", "))
  }
   numericCols <- c("home_odds", "away_odds", "draw_odds")
  if(!all(sapply(data[numericCols], is.numeric))) {
    stop("(home_odds, away_odds, draw_odds) must be numeric.")
  }
  if (any(is.na(as.POSIXct(data[[logged_time]], format = "%Y-%m-%d %H:%M:%S")))) {
    stop("input must be in the format 'YYYY-MM-DD HH:MM:SS'.")
  }
  data_tibble <- data |>
  as_tsibble(key = event_id, index = logged_time)
  class(data_tibble) <- c("bettr_data", class(data_tibble))
  return(data_tibble)
}
x1 <- epl_weekend |> make_bettr()
class(x1)

