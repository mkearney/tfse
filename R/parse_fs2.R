parse_fs2 <- function(x, n = NULL, as_double = FALSE) {
  x <- rawToChar(x$content)
  if (grepl("errors", x)) {
    x <- NA_real_
    next_cursor <- NULL
  } else {
    x <- strsplit(x, "\\]|\\[")[[1]]
    if (length(x) > 2) {
      next_cursor <- strsplit(x[3], "\\,|\\:")[[1]][[3]]
    } else {
      next_cursor <- NULL
    }
    if (as_double) {
      x <- as.double(x[[1]][["ids"]])
    } else {
      x <- as.character(x[[1]][["ids"]])
    }
  }
  x <- data.frame(x, stringsAsFactors = FALSE)
  names(x) <- "ids"

  attr(x, "next_cursor") <- next_cursor
  x
}