
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
  stopifnot(is.data.frame(x))
  if (!inherits(x, "tbl_df")) {
    x <- tibble::as_tibble(x)
  }
  d <- x
  vars <- names(x)
  if ("width" %in% names(list(...))) {
    width <- list(...)$width
    op <- getOption("width")
    on.exit(options(width = op), add = TRUE)
    options(width = width)
  } else {
    width <- getOption("width")
  }
  p <- utils::capture.output(print(x, ...))
  overfill <- grep("# \\.\\.\\. with", p)
  if (length(overfill) > 0 && length(p) > overfill) {
    p[overfill] <- gsub(" <\\S+>\\,?", "", p[overfill])
    p[length(p)] <- sub("\\>\\W+$", ">", p[length(p)])
    x <- gsub("^#\\s+| <\\S+>\\,?", "", p[(overfill + 1L):length(p)])
    x <- unlist(strsplit(x, " "))
    x <- gsub("`", "", x)
    vars1 <- vars[!vars %in% x]
    vars <- c(vars1, x)
    h <- (width - 8) %/% 4
    long_names <- nchar(x) > (h * 2)
    if (any(long_names)) {
      x[long_names] <- paste0(
      substr(x[long_names], 1, h), "__",
      substr(x[long_names],
        nchar(x[long_names]) - h, nchar(x[long_names])))
      wvars <- which(long_names) + length(vars1)
      vars2 <- vars
      vars2[wvars] <- x[long_names]
    } else {
      vars2 <- vars
    }
    x <- paste(x, collapse = ", ")
    x <- strsplit(strtrim(x, width), "\n")[[1]]
    if (length(x) > 5) {
      x <- x[1:5]
      x[5] <- paste0(sub("\\s{0,}\\,$", "", x[5]), "...")
      vars <- vars[1:(length(vars1) + 5)]
      vars2 <- vars2[1:(length(vars1) + 5)]
    }
    x <- paste0("#   ", x)
    p <- c(p[1:overfill], x)
    d <- d[vars]
    names(d) <- vars2
  }
  #if (is.character(title)) {
  #  p[1] <- paste0("# ", title)
  #} else if (!title) {
  #  p <- p[-1]
  #}
  #cat(paste(p, collapse = "\n"), fill = TRUE)
  d
}
