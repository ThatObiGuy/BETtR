#'  Convert a dataset to a bettr dataset ready for analysis
#'
#'
#' @param data An object containing betting data.
#' @param event_id Name of the column containing the unique event identifier in
#' \code{data}.
#' @param logged_time Name of the column containing the logged time for
#' each event in \code{data}.
#' @param home_odds Name of the column containing home odds in \code{data}.
#' @param away_odds Name of the column containing away odds in \code{data}.
#' @param draw_odds Name of the column containing draw odds in \code{data}.
#' @param make_tsibble Logical; if \code{TRUE}, convert data to a tsibble.
#' @param ... Catches unused arguments
#'
#' @returns returns an object of class \code{bettr_data}.
#' @export
#' @importFrom tsibble "as_tsibble"
#' @importFrom dplyr "arrange"
#' @examples
#' # Example with package dataset
#' data(football)
#'
#' x <- make_bettr(football, make_tsibble = TRUE)
#' x2 <- make_bettr(football, make_tsibble = FALSE)
#' class(x)
#' class(x2)
#'
#' # Example with a generated dataset
#' generatedExample <- data.frame(idOfEvent = 1,
#' log_time = as.POSIXct("2024-01-01 12:00:00"), home_odds = 2.89,
#' away-odds = 2.40, drawOdds = 3.36)
#'
#' x3 <- make_bettr(generatedExample, draw_odds = "drawOdds",
#' away_odds="away-odds, logged_time= "log_time", event_id = "idOfEvent")
#' class(x3)


make_bettr <- function(data,
                       event_id = "event_id",
                       logged_time = "logged_time",
                       home_odds = "home_odds",
                       away_odds = "away_odds",
                       draw_odds = "draw_odds", make_tsibble = TRUE, ...) {
  #checking if it has the 5 required columns to be a bettr object
  req <- c(event_id, logged_time, home_odds, away_odds, draw_odds)
  allReqNotInData <- setdiff(req, names(data))
  if(length(allReqNotInData) != 0) {
    stop("data input is missing the following columns: ",
         paste(allReqNotInData, collapse = ", "))
  }
  numericCols <- c(home_odds, away_odds, draw_odds)
  if(!all(sapply(data[numericCols], is.numeric))) {
    stop("(home_odds, away_odds, draw_odds) must be numeric.")
  }
  if(!inherits(data[[logged_time]], "POSIXct"))
  {
    stop("input must be in the format POSIXct: 'YYYY-MM-DD HH:MM:SS'.")
  }
  data <- data %>% dplyr::arrange(.data[[event_id]], .data[[logged_time]])
  # arrange by ids then time and .data so we can allow event_id <- "event"
  data[[event_id]] <- factor(data[[event_id]],  # factors ids and rids dups
                             levels = unique(data[[event_id]]))

  if (make_tsibble) {
    data <- tsibble::as_tsibble(data, key = event_id, index = logged_time)
  }
  class(data) <- unique(c("bettr_data", class(data))) # unique to not have 2 bettr objs
  return(data)
}
