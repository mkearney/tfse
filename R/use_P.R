#' Use paste operators in a package
#'
#' Imports tfse's P and PP paste operators in a package
#'
#' @return Adds utils-P.R file and 'tfse' to Imports
#' @export
use_P <- function () {
  if (!requireNamespace("usethis", quietly = TRUE)) {
    stop("'use_P()' requires that you install {usethis}.", call. = FALSE)
  }
  usethis:::check_is_package("use_P()")
  if (!usethis:::uses_roxygen()) {
    stop("'use_P()' requires use of {roxygen}.", call. = FALSE)
  }
  usethis:::use_dependency("tfse", "Imports")
  new <- usethis:::use_template("P.R", "R/utils-P.R", package = "tfse")
  todo <- function(x) {
    paste0("* [ ] ", x)
  }
  todo("Run 'devtools::document()'")
  invisible(new)
}
