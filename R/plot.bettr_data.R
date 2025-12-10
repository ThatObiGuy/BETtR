#' Plot BETtR data
#'
#' Plots percentage change of the odds of a (or many) opening favorite(s) change over time.
#'
#' @param x An object of class \code{"bettr_data"} - You can use the inbuilt dataset, \code{"football"} or using your own odds data after using the \code{link{make_bettr}} function.
#' @param ... Catches unused arguments to \code{plot} (not currently implemented).
#'
#' @returns a ggplot using geom_line to show the odds movement over time
#'
#' @export
#' @author Owen F. O'Connor - <\email{owen.oconnor.2024@@mumail.ie}>
#'
#' @importFrom dplyr "as_tibble" "group_by" "slice_min" "mutate" "case_when" "select" "left_join"
#' @importFrom ggplot2 "ggplot" "geom_line"
#'
#' @examples
#' plot(football)
plot.bettr_data <- function(x, ...) {
  if(!inherits(x, "bettr_data")) stop("x must be of class 'bettr_data'")
  x <- dplyr::as_tibble(x) # fixing compatability with tstibble bettr_data

  # Identify opening favourites and their opening odds from given dataframe
  x |>
    dplyr::group_by(event_id) |>
    dplyr::slice_min(logged_time, n = 1) |>
    dplyr::mutate(fav_choice = c("home_odds", "draw_odds", "away_odds")[which.max(c(home_odds, draw_odds, away_odds))],
                  opening_fav_odds = dplyr::case_when(
                    fav_choice == "home_odds" ~ home_odds,
                    fav_choice == "draw_odds" ~ draw_odds,
                    fav_choice == "away_odds" ~ away_odds
                  )) |>
    dplyr::select(event_id, fav_choice, opening_fav_odds) -> x_opening_favourites

  # Appending original data with this info
  x |>
    dplyr::left_join(x_opening_favourites, by = "event_id") |>
    dplyr::mutate(
      fav_odds = dplyr::case_when(
        fav_choice == "home_odds" ~ home_odds,
        fav_choice == "draw_odds" ~ draw_odds,
        fav_choice == "away_odds" ~ away_odds
      ),
      location_adjusted_fav_odds = fav_odds - opening_fav_odds, # Bringing all odds to start at zero
      scale_adjusted_fav_odds = location_adjusted_fav_odds / opening_fav_odds # Adjusting scale
    ) -> x_plottable
  # Scale and location adjustments are so that we can focus on displaying movement on a similar scale with a fixed start.
  # We could in future allow the user to specify scaling parameter

  p <- ggplot2::ggplot(x_plottable, ggplot2::aes(x=logged_time, y=scale_adjusted_fav_odds, colour=event_id)) +
    ggplot2::geom_line()

  print(p)
}
