#' Plot betting odds movements over time
#'
#' Creates an interactive visualisation of percentage changes in betting odds
#' relative to opening values. Produces a \code{\link[ggiraph]{girafe}} object
#' with interactive tooltips, falling back to static \code{\link[ggplot2]{ggplot}}
#' if interactive plotting fails.
#'
#' @param x An object of class \code{"bettr_data"}. See \code{\link{make_bettr}}.
#' @param odd Character string. Either \code{"CF"} (closing favourite) or
#'   \code{"OF"} (opening favourite). Default is \code{"CF"}.
#' @param fixture Character string or \code{NULL}. Optional column name in \code{x}
#'   to use for tooltips and legend labels instead of \code{event_id}.
#'   Default is \code{NULL}.
#' @param ... Additional arguments passed to \code{\link[ggiraph]{girafe}} for
#'   controlling plot dimensions (e.g., \code{width_svg}, \code{height_svg}).
#'
#' @returns An object of class \code{"girafe"} (interactive plot) or \code{"gg"}
#'   (static ggplot2 plot if interactive plotting fails).
#'
#' @export
#' @author Owen F. O'Connor - <\email{owen.oconnor.2024@@mumail.ie}>
#'
#' @importFrom dplyr "as_tibble" "slice_min" "slice_max" "case_when" "left_join"
#' @importFrom genzplyr "squad_up" "glow_up" "vibe_check"
#' @importFrom ggplot2 "ggplot" "geom_line" "scale_color_discrete"
#' @importFrom ggiraph "geom_line_interactive" "girafe"
#'
#' @seealso \code{\link{make_bettr}}, \code{\link{predict.bettr_data}}
#'
#' @examples
#' football <- make_bettr(football)
#' plot(football)
#' plot(football, odd = "CF", width_svg = 8)
#'
#' football2 <- football |>
#'   genzplyr::glow_up(fixture = paste(home_team, "vs", away_team))
#'
#' plot(football2, fixture = "fixture")
plot.bettr_data <- function(x, odd = c("CF", "OF"), fixture = NULL, ...) {

  # Checking x is of class bettr_data
  if(!inherits(x, "bettr_data")) stop("x must be of class 'bettr_data'")

  # Checking the given argument for the odd parameter is valid
  odd <- match.arg(odd)

  # Validate fixture column if provided
  if (!is.null(fixture)) {
    if (!fixture %in% names(x)) {
      stop("Column '", fixture, "' not found in x")
    }
  }

  # We require an object containing opening odds
  x |>
    genzplyr::squad_up(event_id) |>
    dplyr::slice_min(logged_time, n = 1) |>
    genzplyr::vibe_check(event_id, home_odds, draw_odds, away_odds) -> opening_odds

  # We must identify the opening or closing favourites based on user's argument
  if (odd == "OF") {
    opening_odds |>
      genzplyr::glow_up(fav_choice = c("home_odds", "draw_odds", "away_odds")[which.max(c(home_odds, draw_odds, away_odds))],
                        opening_fav_odds = dplyr::case_when(
                          fav_choice == "home_odds" ~ home_odds,
                          fav_choice == "draw_odds" ~ draw_odds,
                          fav_choice == "away_odds" ~ away_odds)) |>
      genzplyr::vibe_check(event_id, fav_choice, opening_fav_odds) -> x_target_odd

  } else if (odd == "CF") {
    x |>
      genzplyr::squad_up(event_id) |>
      dplyr::slice_max(logged_time, n = 1) |>
      genzplyr::glow_up(fav_choice = c("home_odds", "draw_odds", "away_odds")[which.max(c(home_odds, draw_odds, away_odds))]) |>
      genzplyr::vibe_check(event_id, fav_choice) -> x_target_odd

    x_target_odd |>
      dplyr::left_join(opening_odds, by = "event_id") |>
      genzplyr::glow_up(opening_fav_odds = dplyr::case_when(
        fav_choice == "home_odds" ~ home_odds,
        fav_choice == "draw_odds" ~ draw_odds,
        fav_choice == "away_odds" ~ away_odds
      )) |>
      genzplyr::vibe_check(event_id, fav_choice, opening_fav_odds) -> x_target_odd
  }

  # Appending original data with this info and transforming data
  x |>
    dplyr::left_join(x_target_odd, by = "event_id") |>
    genzplyr::glow_up(
      fav_odds = dplyr::case_when(
        fav_choice == "home_odds" ~ home_odds,
        fav_choice == "draw_odds" ~ draw_odds,
        fav_choice == "away_odds" ~ away_odds
      ),
      pct_change_fav_odds = (fav_odds - opening_fav_odds) / opening_fav_odds * 100
    ) -> x_base

  # Create legend_label and tooltip columns
  if (is.null(fixture)) {
    x_base |>
      genzplyr::glow_up(
        legend_label = event_id,
        tooltip = paste0("event_id: ", event_id)
      ) -> x_plottable
  } else {
    x_base |>
      genzplyr::glow_up(
        legend_label = fixture,
        tooltip = fixture
      ) -> x_plottable
  }

  x_plottable <- x_plottable |>
    genzplyr::vibe_check(event_id, legend_label, logged_time, pct_change_fav_odds, tooltip)

  # Core ggplot object (shared by both interactive and static)
  p_base <- ggplot2::ggplot(
    x_plottable,
    ggplot2::aes(
      x = logged_time,
      y = pct_change_fav_odds,
      colour = legend_label
    )
  ) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Odds Change (%)") +
    ggplot2::ggtitle(ifelse(odd == "CF",
                            "Change over time of closing favourite odds",
                            "Change over time of opening favourite odds")) +
    ggplot2::scale_color_discrete(name = if (is.null(fixture)) "Event ID" else "Fixture")

  output <- try(
    {
      p_interactive <- p_base +
        ggiraph::geom_line_interactive(
          ggplot2::aes(tooltip = tooltip, data_id = event_id)
        )
      ggiraph::girafe(ggobj = p_interactive, ...)
    },
    silent = TRUE
  )

  if (inherits(output, "try-error")) {
    message("interactive plotting failed, reverting to static")
    p_base + ggplot2::geom_line()
  } else {
    output
  }
}
