fill_space <- function(x) {
  lns <- nchar(x) + 2L
  mln <- max(lns, na.rm = TRUE)
  make_space <- function(n) {
    make_space_ <- function(n) paste(rep(" ", n), collapse = "")
    sapply(n, make_space_)
  }
  sps <- make_space(mln - nchar(x))
  x <- strsplit(x, "\\t")
  unlist(Map(paste, x, collapse = sps))
}