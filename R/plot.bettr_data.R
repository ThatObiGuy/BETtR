#' Plot BETtR data
#'
#' Plots how the odds of a (or many) opening favorite(s) change over time.
#'
#' @param x An object of class \code{"bettr_data"} - You can use the inbuilt dataset, \code{"epl_weekend"} or using your own odds data after using the \code{link{make_bettr}} function.
#' @param ... Catches unused arguments to \code{plot} (not currently implemented).
#'
#' @returns a ggplot using geom_line to show the odds movement over time
#' @export
#'
#' @examples
plot.bettr_data <- function(x, ...) {
  if(!inherits(x, "bettr_data")) stop("x must be of class 'bettr_data'")

  # Identify opening favourite
  x |>
    dplyr::group_by(event_id) |>
    dplyr::mutate(
      fav_choice = {
        last_row <- dplyr::slice_max(logged_time)
        c("home_odds", "draw_odds", "away_odds")[
          which.max(c(last_row$home_odds, last_row$draw_odds, last_row$away_odds))
        ]
      }
    ) |>
    dplyr::mutate(
      fav_odds = case_when(
        fav_choice == "home_odds" ~ home_odds,
        fav_choice == "draw_odds" ~ draw_odds,
        fav_choice == "away_odds" ~ away_odds
      )
    ) |>
    dplyr::select(event_id, logged_time, fav_odds) |>
    dplyr::mutate(
      opening_fav_odds = dplyr::first(fav_odds),  # First appearing fav_odds per group
      location_adjusted_fav_odds = fav_odds - opening_fav_odds,
      scale_adjusted_fav_odds = location_adjusted_fav_odds / opening_fav_odds
    ) |>
    dplyr::ungroup() -> plottable

  p <- ggplot2::ggplot(plottable, ggplot2::aes(x=logged_time, y=scale_adjusted_fav_odds, colour=event_id)) +
    ggplot2::geom_line()

  print(p)
}
