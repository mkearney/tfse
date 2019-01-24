
#' Open file
#'
#' Opens files.
#'
#' @param x File to open
#' @return File should open
#' @export
open_file <- function(x) {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    browseURL(x)
  }
  if (is_r_file(x) && is_rstudio()) {
    rstudioapi::navigateToFile(x)
  } else{
    browseURL(x)
  }
}

is_r_file <- function(x) grepl("\\.R$|\\.Rmd", x, ignore.case = TRUE)

is_rstudio <- function() identical(.Platform$GUI, "RStudio")

