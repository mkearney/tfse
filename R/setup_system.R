#' Installs packages to my liking for new systems
#'
#' Installs a group of packages I like to setup my systems with.
#'
#' @export
setup_system <- function() {
  if (.Platform$OS.type == "unix") {
    warning("Make sure to install xml, curl, openssl, etc.")
  }
  ## install from CRAN
  install_if(c("tidyverse", "devtools", "rockchalk", "gbm",
    "rvest", "rtweet", "textfeatures", "roxygen2", "lavaan"))
  ## install from github
  devtools::install_github("mkearney/rtweet")
  devtools::install_github("mkearney/tfse")
  devtools::install_github("mkearney/textfeatures")
  devtools::install_github("mkearney/botrnot")
  devtools::install_github("klutometis/roxygen")
  devtools::install_github("yihui/xaringan")
  devtools::install_github("r-lib/usethis")
  devtools::install_github("r-lib/pkgdown")
  devtools::install_github("quanteda/quanteda")
  devtools::install_github("juliasilge/tidytext")
}

install_if_ <- function(x) {
  i <- !requireNamespace(x, quietly = TRUE)
  if (i) {
    install_pkg_verbose(x)
  }
  i
}

#' Install packages if not already
#'
#' Wrapper around install.packages
#'
#' @param pkg Character; one or more packages to install.
#' @return Returns data frame with pkg (chr) and required_install (lgl) columns.
#' @export
install_if <- function(pkg) {
  x <- vapply(pkg, install_if_, FUN.VALUE = logical(1))
  data.frame(pkg = names(x), required_install = x,
    row.names = NULL)
}
