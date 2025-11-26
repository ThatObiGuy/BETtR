plot.bettr_data <- function(x, ...) {
  if(!inherits(x, "bettr_data")) stop("x must be of class 'bettr_data'")

  ## takes an object with the bettr_data attribute plots odds movement over time as well as information gain over time. (Owen)
  time = x$time
  odds = x$odds
  if(!is.numeric(time)) warning("needs to be a numerical input")
}
