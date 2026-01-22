#' Convert a dataset to a bettr dataset ready for analysis
#'
#' make_bettr makes a betting dataset ready for analysis by checking for the
#' required columns (\code{event_id}, \code{logged_time}, \code{home_odds},
#' \code{away_odds}, and \code{draw_odds}) that each bettr dataset must have.
#' Make_bettr checks that the odds (home, away, draw) columns are numeric.
#' Counts rows with missing values with a warning message and can optionally
#' remove those rows if the logical argument \code{drop_NA_values = TRUE}.
#' Additionally it converts \code{data} into a \code{tsibble} ready for time
#' series analysis, or it can be left as the original dataframe by setting
#' \code{make_tsibble = FALSE}.
#'
#'
#' @param data An object containing betting data
#' @param event_id  A string of the name of the column containing the event id
#' \code{data}. For example
#' \code{event_id = "name of column with event_id"}. The default being
#' \code{event_id = "event_id"}.
#' @param logged_time A string of the name of the column containing the logged
#' time for each event in \code{data}. For example
#' \code{logged_time = "name of column with logged_time"}. The default being
#' \code{logged_time = "logged_time"}.
#' @param home_odds A string of the name of the column containing home odds in
#' \code{data}. For example \code{home_odds = "name of column with home_odds"}.
#' The default being \code{home_odds = "home_odds"}.
#' @param away_odds A string of the name of the column containing away odds in
#' \code{data}. For example \code{away_odds = "name of column with away_odds"}.
#' The default being \code{away_odds = "away_odds"}.
#' @param draw_odds A string of the name of the column containing draw odds in
#' \code{data}. For example \code{draw_odds = "name of column with draw_odds"}.
#' The default being \code{draw_odds = "draw_odds"}.
#' @param make_tsibble A logical argument that converts the object to a type
#' tsibble, argument is set to \code{make_tsibble = TRUE} by default.
#' @param drop_NA_values A logical argument that drops any rows with NA values,
#' argument is set to \code{drop_NA_values = FALSE} by default.
#' @param ... additional arguments passed to  \code{as_tsibble()}
#'
#' @returns returns an object of class \code{bettr_data}. Also adds class
#' \code{tbl_ts}, \code{tbl_df} and \code{tbl} if \code{make_tsibble = TRUE}.
#'
#' @note The columns (home_odds, away_odds, draw_odds) must be numeric.
#'  The column logged_time must be of class POSIXct. If converting to a
#'  tsibble, the rows with NA values in logged_time will prevent the
#'  as_tsibble conversion.
#'  If there are missing values in the required columns and drop_NA_values is
#'  set to False, a warning is given. It is recommended to remove these for
#'  proper analysis.
#'
#'
#' @export
#' @author Sorin Bivol - <\email{SORIN.BIVOL.2023@@mumail.ie}>
#' @importFrom dplyr "arrange" "all_of" "across"
#' @importFrom tsibble "as_tsibble"
#' @importFrom stats  "complete.cases"

#' @examples
#'
#' # Example with package dataset
#' data(football)
#'
#' x <- make_bettr(football, make_tsibble = TRUE)
#'
#' x2 <- make_bettr(football, make_tsibble = FALSE)
#'
#' #example with generated dataset that contains NAs and custom column names
#' generatedExample <- data.frame(idOfEvent = 1:3,
#'   log_time = as.POSIXct(c("2024-01-01 12:00:00",
#'                           "2024-01-02 12:00:00",NA)),
#'   home_odds = c(2.55, 1.55, 3.11), away_odds = c(2.11, 3.22, 1.33),
#'   drawOdds = c(3.36, 3.00, NA)
#' )
#'
#'\dontrun{
#' # NAs will be kept and just issue a warning
#' x3 <- make_bettr(generatedExample, draw_odds = "drawOdds",
#'                  away_odds = "away_odds", logged_time = "log_time",
#'                  event_id = "idOfEvent") }
#'
#' # Remove NAs by setting drop_NA_values to TRUE
#' x4 <- make_bettr(generatedExample, draw_odds = "drawOdds",
#'                  away_odds = "away_odds", logged_time = "log_time",
#'                  event_id = "idOfEvent", drop_NA_values = TRUE)

make_bettr <- function(data,
                       event_id = "event_id",
                       logged_time = "logged_time",
                       home_odds = "home_odds",
                       away_odds = "away_odds",
                       draw_odds = "draw_odds", make_tsibble = TRUE,
                       drop_NA_values = FALSE, ...) {


  if (inherits(data, "bettr_data")) {
    stop("Dataset is already of class 'bettr_data'.",
    "\nApply make_bettr() to non bettr datasets.", call. = FALSE)
  }

  #checking if it has the 5 required columns to be a bettr object
  req <- c(event_id, logged_time, home_odds, away_odds, draw_odds)
  allReqNotInData <- setdiff(req, names(data))
  if(length(allReqNotInData) != 0)
  {
    stop("data input is missing the following columns: ",
         paste(allReqNotInData, collapse = ", "))
  }
  numericCols <- c(home_odds, away_odds, draw_odds)
  if(!all(vapply(data[numericCols], is.numeric, logical(1))))
  {
    stop("(home_odds, away_odds, draw_odds) must be numeric.")
  }
  # Checks for missing rows:
  # use complete.cases: want rows
  ok <- stats::complete.cases(data[, req])
  if(any(!ok)) {
   if(sum(!ok) == 1) {
    # rows is plural cant have that for 1 row
     warning("Data has ", sum(!ok), " row with NA values",
    "Get rid of these NA values by setting argument drop_NA_values = TRUE in make_bettr().",
     "\nRecommended to remove all rows with NAs for proper analysis."
             , call. = FALSE)
  } else {
    warning("Data has ", sum(!ok), " rows with NA values",
    "\nGet rid of these NA values by setting argument drop_NA_values = TRUE in make_bettr().",
    "\nRecommended to remove all rows with NAs for proper analysis."
    , call. = FALSE)
  } }

  if(drop_NA_values)
  {
    data <- data[ok, ]
  }

  if(!inherits(data[[logged_time]], "POSIXct"))
  {
    stop("logged time column must be in the format POSIXct: 'YYYY-MM-DD HH:MM:SS'")
  }
  if (!(is.numeric(data[[event_id]])
       || is.character(data[[event_id]]) || is.factor(data[[event_id]]))){
    stop("event_id must be of type character, factor or numeric")
  }
  # arrange by ids then time
  data <- dplyr::arrange(data,
                         dplyr::across(dplyr::all_of(c(event_id, logged_time))))
  data[[event_id]] <- factor(data[[event_id]],# factors ids and rids duplicates
                             levels = unique(data[[event_id]]))

  if (make_tsibble)
  {
    if (anyNA(data[[logged_time]])) {
      stop("\nLogged time column has missing values (NAs).",
      "\nThey need to be removed by setting drop_NA_values to TRUE."
      , "\nCannot create a tsibble if there's missing values in logged_time.")
    }
    data <- tsibble::as_tsibble(data, key = event_id, index = logged_time, ...)
  }
  class(data) <- unique(c("bettr_data", class(data))) # unique to not have 2 bettr objs
  return(data)
}



