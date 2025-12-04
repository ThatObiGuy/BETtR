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

  p <- ggplot2::ggplot(x, ggplot2::aes(x=, y=)) +
    ggplot2::geom_line(ggplot2::aes(colour= ))
  print(p)
}
