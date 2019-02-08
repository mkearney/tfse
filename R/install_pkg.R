is_installed <- function(..., .stop = FALSE) {
  x <- c(...)
  ip <- utils::installed.packages()
  yn <- x %in% ip
  if (sum(!yn) > 0 && .stop) {
    msg <- pmsg(
      "This function requires the following packages: ",
      paste(x[!yn], collapse = ", "),
      print = FALSE
    )
    stop(msg, call. = FALSE)
  }
  invisible(set_names(yn, x))
}
