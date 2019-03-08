#' Split URL
#'
#' Splits URL(s) such that each URL is represented with a character vector split
#' at all single forward slashes and ampersand (forward slashes and ampersands
#' are included in the return object)
#'
#' @param x Input character vector
#' @return If input is one string, then a character vector. If multiple URLs,
#' then a list of character vectors
#' @export
split_url <- function(x) {
  UseMethod("split_url")
}

#' @export
split_url.character <- function(x) {
  if (length(x) == 1) {
    return(strsplit(x, "(?<!/)\\b(?=/)|\\b(?=&)", perl = TRUE)[[1]])
  }
  strsplit(x, "(?<!/)\\b(?=/)|\\b(?=&)", perl = TRUE)
}

#' @export
split_url.default <- function(x) {
  stopifnot(
    is.character(x)
  )
}

#' Wrap URL
#'
#' Wraps long URL with the %P% operator
#'
#' @param x Input URL
#' @param width Width defaulting to 80 (or current width if smaller)
#' @return Text copied to clipboard and printed
#' @export
wrap_url <- function(x, width = NULL) {
  if (is.null(width)) {
    width <- getOption("width", 80)
    if (width > 80) width <- 80
  }
  x <- paste0(split_url(x), collapse = " ")
  x <- strwrap(x, width - 6, indent = 0, exdent = 2)
  x <- gsub("(?<=\\S) (?=(/|&))", "", x, perl = TRUE)
  x <- sub("^(?=\\S)|\\s(?=\\S)", '"', x, perl = TRUE)
  x <- paste0(paste0(x, '"'), collapse = " %P%\n")
  pbcopy(x)
  cat(x, fill = TRUE)
}
