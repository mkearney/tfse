#' check_renv
#'
#' Checks whether one can safely append to .Renvir file.
#'
#'
#' @param path Path/to/.Renvir
#' @usage
#' check_renv(path)
#' @export
check_renv <- function(path) {
  if (!file.exists(path)) {
    return(invisible())
  }
  con <- file(path)
  x <- readLines(con, warn = FALSE)
  close(con)
  x <- clean_renv(x)
  x <- paste(x, collapse = "\n")
  cat(x, file = path, fill = TRUE)
  invisible()
}

clean_renv <- function(x) {
  stopifnot(is.character(x))
  ## remove incomplete vars
  x <- grep("=$", x, value = TRUE, invert = TRUE)
  ## split lines with double entries and fix into new vars
  xs <- strsplit(x, "=")
  vals <- sub("[^=]*=", "", x)
  kp <- !grepl("[[:upper:]]{1,}=", vals)
  if (sum(!kp) > 0L) {
    m <- regexpr("[[:upper:]_]{1,}(?==)", x[!kp], perl = TRUE)
    newlines <- paste0(regmatches(x[!kp], m), "=", sub(".*=", "", x[!kp]))
    x <- x[kp]
    x[(length(x) + 1):(length(x) + length(newlines))] <- newlines
  }
  ## remove double entries
  xs <- strsplit(x, "=")
  kp <- !duplicated(sapply(xs, "[[", 1))
  x <- x[kp]
  x
}

dots <- list(a = 1, b = 2)


#' set_renv
#'
#' Create and save new R environment variable
#'
#' @param ... Named values where names correspond to the name of the
#'   environment variables. Must include names for each value.
#' @param path Path to Renviron file, typcally .Renviron found in home
#'   directory.
#' @return Saves and reads into current session new environment variable(s).
#' @export
set_renv <- function(..., path = "~/.Renviron") {
  dots <- list(...)
  nms <- names(dots)
  stopifnot(length(nms) > 0L)
  stopifnot(length(dots) == length(nms))
  x <- paste0(nms, "=", dots, collapse = "\n")
  check_renv(path)
  cat(x, file = path, fill = TRUE, append = TRUE)
  readRenviron(path)
}
