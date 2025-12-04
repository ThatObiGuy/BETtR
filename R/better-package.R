#' Title
#'
#' @param x
#' @param y
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
.onAttach <- function(x,y,...){

  packageStartupMessage(paste("\n ____  _____ _____ _   ____  \n| __ )| ____|_   _| |_|  _ \\      Calculate Shannon Entropy and\n|  _ \\|  _|   | | | __| |_) |     Provide Optimisation Functions\n| |_) | |___  | | | |_|  _ <      Version: 0.1.0\n|____/|_____| |_|  \\__|_| \\_\\ \t\n\n\nType", sQuote("?BETtR"), "to see a brief guide on how to use this R-Package."))

}
