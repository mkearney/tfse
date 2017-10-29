#' Silently load R packages
#'
#' Shortcut for library that is completely silent
#'
#' @param x Name of library. Accepts unquoted or quoted text.
#' @return Loads package namespace.
#' @examples
#' ## load the tidyverse without all the messages
#' lib(tidyverse)
#' @export
lib <- function(x) {
  if (is_char(x)) {
    x <- noquote(x)
  }
  suppressWarnings(
    suppressMessages(
      suppressPackageStartupMessages(
        eval(substitute(
          library(x, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)))
      )
    )
  )
}

is_char <- function(x) {
  tryCatch(is.character(x), error = function(e) return(FALSE))
}
