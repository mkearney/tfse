
#' print big numbers with commas
#'
#' @param x Numeric or character vector.
#' @return String with commas in the right places.
#' @export
add_commas <- function(x) {
  if (is.numeric(x)) {
    op <- options()
    on.exit(options(op))
    options(scipen = 5)
    x <- as.character(round(x, 0))
  }
  stopifnot(is.character(x))
  x <- strsplit(x, "")
  x <- sapply(x, function(i) paste(rev(i), collapse = ""))
  new <- x
  old <- rep("", length(x))
  while (!identical(new, old)) {
    old <- new
    new <- regex_swap(old, "(?<=\\d{3})(?=\\d)", "subcomma")
  }
  x <- strsplit(new, "")
  sapply(x, function(i) paste(rev(i), collapse = ""))
}

regex_extract <- function(x, pat, ignore.case = FALSE, invert = FALSE, ...) {
  m <- gregexpr(pat, x, ignore.case = ignore.case, ...)
  regmatches(x, m, invert = invert)
}

regex_swap <- function(x, pat, f, ...) {
  f <- match.fun(f)
  if (grepl("\\(\\?", pat)) {
    perl <- TRUE
  } else {
    perl <- FALSE
  }
  m <- regexpr(pat, x, perl = perl)
  if (m[1] > 0L) {
    regmatches(x, m) <- f(regmatches(x, m))
  }
  x
}

subcomma <- function(x) {
  paste0(substr(x, 1, 3), ",", substr(x, 4, nchar(x)))
}
