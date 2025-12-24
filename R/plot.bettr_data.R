#' Plot BETtR data
#'
#' Plots percentage change of the odds of a (or many) opening favorite(s) change over time.
#'
#' @param x An object of class \code{"bettr_data"} - You can use the inbuilt dataset,
#' \code{"football"} or using your own odds data after using the \code{link{make_bettr}} function.
#' @param odd Either \code{"CF"} or \code{"OF"} for Closing Favorite or Opening Favorite respectively.
#' Defaults to \code{"CF"}.
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
plot.bettr_data <- function(x, odd = c("CF", "OF"), ...) {

  # Checking x is of class bettr_data
  if(!inherits(x, "bettr_data")) stop("x must be of class 'bettr_data'")

  # Checking the given argument for the odd parameter is valid
  odd <- match.arg(odd)

  # Fixing compatability with tstibble bettr_data
  x <- dplyr::as_tibble(x)

  # We require an object containing opening odds
  x |>
    dplyr::group_by(event_id) |>
    dplyr::slice_min(logged_time, n = 1) |>
    dplyr::select(event_id, home_odds, draw_odds, away_odds) -> opening_odds

  # We must identify the opening or closing favourites based on user's argument
  if (odd == "OF") {
    opening_odds |>
      dplyr::mutate(fav_choice = c("home_odds", "draw_odds", "away_odds")[which.max(c(home_odds, draw_odds, away_odds))],
                    opening_fav_odds = dplyr::case_when(
                      fav_choice == "home_odds" ~ home_odds,
                      fav_choice == "draw_odds" ~ draw_odds,
                      fav_choice == "away_odds" ~ away_odds
                    )) |>
      dplyr::select(event_id, fav_choice, opening_fav_odds) -> x_target_odd

  } else if (odd == "CF") {
    x |>
      dplyr::group_by(event_id) |>
      dplyr::slice_max(logged_time, n = 1) |>
      dplyr::mutate(fav_choice = c("home_odds", "draw_odds", "away_odds")[which.max(c(home_odds, draw_odds, away_odds))]) |>
      dplyr::select(event_id, fav_choice) -> x_target_odd

    x_target_odd |>
      dplyr::left_join(opening_odds, by = "event_id") |>
      dplyr::mutate(opening_fav_odds = dplyr::case_when(
                      fav_choice == "home_odds" ~ home_odds,
                      fav_choice == "draw_odds" ~ draw_odds,
                      fav_choice == "away_odds" ~ away_odds
                    )) |>
      dplyr::select(event_id, fav_choice, opening_fav_odds) -> x_target_odd

  }

  # Appending original data with this info and transforming data
  x |>
    dplyr::left_join(x_target_odd, by = "event_id") |>
    dplyr::mutate(
      fav_odds = dplyr::case_when(
        fav_choice == "home_odds" ~ home_odds,
        fav_choice == "draw_odds" ~ draw_odds,
        fav_choice == "away_odds" ~ away_odds
      ),
      pct_change_fav_odds = (fav_odds - opening_fav_odds) / opening_fav_odds * 100 ) |>
    dplyr::select(event_id, logged_time, pct_change_fav_odds) -> x_plottable
  # Scale and location adjustments are so that we can focus on displaying movement on a similar scale with a fixed start.
  # A percentage change is a very reasonable scale

  p <- ggplot2::ggplot(x_plottable, ggplot2::aes(x=logged_time, y=pct_change_fav_odds, colour=event_id)) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Odds Change (%)") +
    ggplot2::ggtitle(paste("Change over time of odds data (", odd, ")")) +
    ggplot2::geom_line()

  print(p)
}
