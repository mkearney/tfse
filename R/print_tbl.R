
#' print_tbl
#'
#' Custom tweaks to tibble printing.
#'
#' @param x Tibble
#' @param ... Passed to print
#' @param title Name of data frame, defaults to "A tibble..."
#' @return Print
#' @export
print_tbl <- function(x, ..., title = TRUE) {
  if ("width" %in% names(list(...))) {
    width <- list(...)$width
    op <- getOption("width")
    on.exit(options(width = op), add = TRUE)
    options(width = width)
  } else {
    width <- getOption("width")
  }
  p <- capture.output(print(x, ...))
  overfill <- grep("# \\.\\.\\. with", p)
  if (length(overfill) > 0) {
    p[overfill] <- gsub(" <\\S+>", "", p[overfill])
    x <- gsub("^#\\s+| <\\S+>,", "", p[(overfill + 1L):length(p)])
    x <- unlist(strsplit(x, " "))
    x <- gsub("`", "", x)
    h <- (width - 8) %/% 4
    long_names <- nchar(x) > (h * 2)
    if (any(long_names)) {
      x[long_names] <- paste0(
      substr(x[long_names], 1, h), ".*",
      substr(x[long_names],
        nchar(x[long_names]) - h, nchar(x[long_names])))
    }
    x <- paste(x, collapse = ", ")
    x <- strsplit(stringr::str_wrap(x, width), "\n")[[1]]
    if (length(x) > 5) {
      x <- x[1:5]
      x[5] <- paste0(sub("\\s{0,}\\,$", "", x[5]), "...")
    }
    x <- paste0("#   ", x)
    p <- c(p[1:overfill], x)
  }
  if (is.character(title)) {
    p[1] <- paste0("# ", title)
  } else if (!title) {
    p <- p[-1]
  }
  cat(p, fill = TRUE)
}
