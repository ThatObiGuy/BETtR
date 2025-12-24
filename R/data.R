#' Odds from 10 different soccer matches
#'
#' Data on the odds at different time points of a series of matches from the 2025 English premier league, including max bettable amounts at these intervals.
#' @format A data frame with 1573 observations (10 matches with ~160 observations from each) and 9 columns. It contains the required columns of a bettr_data object with additional info about maximum bettable amount and start times.
#' @examples
#' data(football, package="bettr")
#' pairs(football[,-(1:2)], col=football$home_team)
#' @docType data
#' @keywords datasets
#' @usage data(football)
"football"
