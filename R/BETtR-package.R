#' BETtR:An R package for structuring, visualising, and exploring betting market odds data.
#'
#' This function serves as the package startup. Displaying an ASCII art as well as relevant information for package purpose, version history and other helpul tips
#'
#' @name bettr
#' @returns
#' @export
#'
#' @examples
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

