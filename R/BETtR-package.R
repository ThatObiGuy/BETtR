#' BETtR: An R package for structuring, visualising, and exploring betting market odds data.
#'
#' BETtR is an R package for structuring and visualising betting market odds data. It provides tools for converting raw odds data into time-indexed objects, plotting odds movements over time, and applying exploratory time-series-based prediction methods to betting odds trajectories.
#'
#'
#' @details
#' \describe{
#' \item{Type: }{Package}
#' \item{Package: }{\pkg{BETtR}}
#' \item{Version: }{0.1.0}
#' \item{Date: }{2025-12-28 }
#' \item{Licence: }{GPL (>= 3)}
#' }
#'
#' @section Usage:
#' The core functionality of the \pkg{BETtR} package is centred around the \code{\link{make_bettr}} function, which converts raw betting odds data into objects of class \code{"bettr_data"}. These objects provide a structured representation of event-level betting odds indexed by time.
#'
#' A dedicated plotting method, \code{\link{plot.bettr_data}}, is provided for visualising the evolution of betting odds over time. Odds are scaled relative to their opening values, allowing movements across different events to be compared on a common scale.
#'
#' BETtR also provides a prediction interface via the method \code{\link[=predict.bettr_data]{predict.bettr_data}} for objects of class \code{"bettr_data"}. This method applies multiple time-series based forecasting approaches to the odds trajectory of a given event, including ARIMA models, exponential smoothing (ETS) models, and a Skellam-based model designed for discrete odds movements. The prediction method automatically checks for gaps in the time index and applies interpolation where necessary.
#'
#' The package contains a data set called: \code{football}
#'
#' @author
#' Owen F. O'Connor [aut, cre],
#' Sorin Bivol [aut],
#' Sean O'Leary [aut],
#' Ivan Cakic [aut]
#'
#' @seealso Useful links:
#' \itemize{
#' \item Report bugs at \url{https://github.com/ThatObiGuy/bettr/issues}
#' }
#'
#' Further details and examples are given in the associated vignette document:
#' \preformatted{vignette("BETtR", package = "bettr")}
#'
#' @examples
#' # Load the package
#' library(bettr)
#'
#' # Load example betting odds dataset
#' data(football)
#'
#' # Inspect the dataset
#' football
#'
#' # Convert data to bettr_data explicitly
#' x_ts <- make_bettr(football, make_tsibble = TRUE)
#' x_df <- make_bettr(football, make_tsibble = FALSE)
#'
#' # Check resulting classes
#' class(x_ts)
#' class(x_df)
#'
#' # Plot odds movements for opening favourites
#' plot(football, odd = "OF")
#'
#' # Plot odds movements for closing favourites
#' plot(football, odd = "CF")
#'
#'
#' # Add a fixture label for interactive tooltips
#' football2 <- football |>
#'   dplyr::mutate(fixture = paste(home_team, "vs", away_team))
#'
#' plot(football2, fixture = "fixture")
#'
#' # Apply prediciton method to football example dataset
#' predict(football, h = 67)
#'
#'
#' # Generate a small synthetic dataset to show general use of Bettr function & methods
#' generated_data <- data.frame(
#'   event_id = rep(1, 5),
#'   logged_time = as.POSIXct(
#'     c("2024-01-01 12:00:00",
#'       "2024-01-01 12:15:00",
#'       "2024-01-01 12:30:00",
#'       "2024-01-01 12:45:00",
#'       "2024-01-01 13:00:00")
#'   ),
#'   home_odds = c(1.90, 1.88, 1.85, 1.87, 1.84),
#'   draw_odds = c(3.40, 3.45, 3.50, 3.48, 3.55),
#'   away_odds = c(4.20, 4.30, 4.35, 4.40, 4.50)
#' )
#'
#' # Convert synthetic data to bettr_data
#' example_bettr <- make_bettr(generated_data)
#'
#' # Plot synthetic example
#' plot(example_bettr)
#'
#' # Predict synthetic example
#' predict(example_bettr, h = 67)
#'
#' @docType package
#' @keywords package
"_PACKAGE" # Stops dev tools from building a .onAttach.Rd

.onAttach <- function(libname, pkgname)
{

  desc_path <- file.path(libname, pkgname, "DESCRIPTION")
  desc <- read.dcf(desc_path)

  pkg <- desc[1, "Package"]
  ver <- desc[1, "Version"]

  if (interactive())
    {
      packageStartupMessage(paste("\n ____  _____ _____ _   ____  \n| __ )| ____|_   _| |_|  _ \\      Calculate Shannon Entropy and\n|  _ \\|  _|   | | | __| |_) |     Provide Optimisation Functions\n| |_) | |___  | | | |_|  _ <      Version:", ver,"\n|____/|_____| |_|  \\__|_| \\_\\ \t\n\n\nType", sQuote("?BETtR"), "to see a brief guide on how to use this R-Package."))
    }
  else
    {
      packageStartupMessage(
      paste("Package", sQuote(pkg),"version", ver))
    }
}

