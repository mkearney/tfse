#' break_lines
#' 
#' Truncates lines to a supplied width value
#' 
#' @param x Input text
#' @param n Number of characters (width) on which to truncate given text
#' @return Output with line breaks optimized for n width.
#' @export
break_lines <- function(x, n = 80) {
  stopifnot(is.character(x))
  sapply(x, break_lines_, n = n)
}


#' rm_amp
#' 
#' Remove ampersands as they show up in tweet data
#' 
#' @param x Input character vector
#' @param y Replacement text
#' @return Output character vector
#' @export
rm_amp <- function(x, y = "&") {
  if (is.null(y)) y <- ""
  gsub("&amp;", y, x)
}


#' rm_linebreaks
#' 
#' Remove linebreaks
#' 
#' @param x Input character vector
#' @param y Replacement text
#' @return Output character vector
#' @export
rm_linebreaks <- function(x, y = " ") {
  x <- gsub("\n", y, x)
}

#' rm_retweets
#' 
#' Remove retweets from data.
#' 
#' @param x Input, should be either data frame (or list) or character.
#'   If list/data frame, the text of the tweet is assumed to be named
#'   "text" while the logical is_retweet is assuemd to be named "is_retweet"
#' @return Output
#' @export
rm_retweets <- function(x, ...) {
  UseMethod("rm_retweets")
}

#' @export
rm_retweets.default <- function(x, ...) {
  grep("^RT ", x, ignore.case = TRUE, invert = TRUE, value = TRUE)
}

#' @export
rm_retweets.data.frame <- function(x,
                                   text = "text",
                                   is_retweet = "is_retweet") {
  stopifnot(is.character(x[[text]]), is.logical(x[[is_retweet]]))
  x[x[[!is_retweet]], ]
}

#' @export
rm_retweets.list <- function(x,
                             text = "text",
                             is_retweet = "is_retweet") {
  stopifnot(is.character(x[[text]]), is.logical(x[[is_retweet]]))
  x[[text]] <- x[[text]][x[[is_retweet]]]
  x
}

#' @export
rm_retweets.character <- function(x,
                                  is_retweet = NULL) {
  if (is.null(is_retweet)) {
    grep("^RT ", x, ignore.case = TRUE, invert = TRUE, value = TRUE)
  } else {
    stopifnot(!identical(length(x), length(is_retweet)))
    x[!is_retweet]
  }
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


break_lines_ <- function(x, n) {
  x <- strsplit(x, "\\n\\n")
  x <- sapply(x, break_line, n = n)
  paste(x, collapse = "\n\n")
}
