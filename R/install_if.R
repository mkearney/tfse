

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
