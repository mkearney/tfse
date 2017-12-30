#' break_lines
#'
#' Truncates lines to a supplied width value
#'
#' @param x Input text
#' @param n Number of characters (width) on which to truncate given text
#' @param sep Separator used for subunits.
#' @param collapse When text is recombined into single string, this value is used
#'   as the sepator. Defaults to three line breaks.
#' @return Output with line breaks optimized for n width.
#' @export
break_lines <- function(x, n = 80, sep = "\\. ", collapse = "\n") {
  stopifnot(is.character(x))
  map_chr_("break_lines_", x,
          MoreArgs = list(n = n, sep = sep, collapse = collapse))
}


break_line <- function(x, n) {
  x <- trim_ws(x)
  if (length(x) == 0L) return("")
  x <- strsplit(x, " ")[[1]]
  cs <- cumsum(nchar(x) + 1L)
  out <- paste(x[cs <= n], collapse = " ")
  continue <- TRUE
  x <- x[cs > n]
  while (continue) {
    if (length(x) == 0) return(out)
    out <- paste0(out, "\n")
    cs <- cumsum(nchar(x) + 1L)
    out <- paste0(
      out, paste(x[cs <= n], collapse = " ")
    )
    x <- x[cs > n]
    if (length(x) == 1L && nchar(x) >= n) break
  }
  paste0(out, "\n", x)
}

break_lines_ <- function(x, n, sep = "\\. ", collapse = "\n") {
  if (!is.null(sep)) {
    x <- strsplit(x, sep)[[1]]
  }
  x <- map_chr_("break_line", x, MoreArgs = list(n = n))
  if (!is.null(collapse)) {
    x <- paste(x, collapse = collapse)
  }
  x
}
