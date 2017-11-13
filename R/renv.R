#' check_renv
#'
#' Checks whether one can safely append to .Renvir file.
#'
#' @usage
#' check_renv(path)
#' @export
check_renv <- function() {
  if (is_incomplete(.Renviron())) {
    append_lines("", file = .Renviron())
  }
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

#' set_renv
#'
#' Create and save new R environment variable
#'
#' @param ... Named values where names correspond to the name of the
#'   environment variables. Must include names for each value.
#' @return Saves and reads into current session new environment variable(s).
#' @export
set_renv <- function(...) {
  dots <- list(...)
  stopifnot(are_named(dots))
  x <- paste0(names(dots), "=", dots)
  x <- paste(x, collapse = "\n")
  check_renv()
  append_lines(x, file = .Renviron())
  readRenviron(.Renviron())
}

.Renviron <- function() {
  if (file.exists(".Renviron")) {
    ".Renviron"
  } else if (!identical(Sys.getenv("HOME"), "")) {
    file.path(Sys.getenv("HOME"), ".Renviron")
  } else {
    file.path(normalizePath("~"), ".Renviron")
  }
}

home <- function() {
  if (!identical(Sys.getenv("HOME"), "")) {
    file.path(Sys.getenv("HOME"))
  } else {
    file.path(normalizePath("~"))
  }
}
