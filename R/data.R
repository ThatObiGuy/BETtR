#' English Premier League betting odds data
#'
#' Betting market odds from 10 English Premier League football matches in 2025,
#' recorded at multiple timepoints before each match. Includes home, away, and
#' draw odds along with maximum allowable bet amounts in euros.
#'
#' @format A data frame with 1,573 observations and 9 variables:
#' \describe{
#'   \item{event_id}{Numeric identifier for each match}
#'   \item{logged_time}{POSIXct timestamp when odds were recorded}
#'   \item{home_odds}{Decimal odds for home team win}
#'   \item{away_odds}{Decimal odds for away team win}
#'   \item{draw_odds}{Decimal odds for draw}
#'   \item{home_team}{Character string with home team name}
#'   \item{away_team}{Character string with away team name}
#'   \item{starts}{POSIXct timestamp when match started for each event}
#'   \item{max_money_line}{Numeric value for Maximum money line available}
#' }
#' @examples
#' data(football, package="bettr")
#' pairs(football[,-(1:2)], col=football$home_team)
#' @docType data
#' @keywords datasets
#' @usage data(football)
#' @source English Premier League 2025 season betting markets
"football"
