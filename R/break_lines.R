#' break_lines
#' 
#' Truncates lines to a supplied width value
#' 
#' @param x Input text
#' @param n Number of characters (width) on which to truncate given text
#' @param sep Separator used for subunits.
#' @param y When text is recombined into single string, this value is used
#'   as the sepator. Defaults to three line breaks.
#' @return Output with line breaks optimized for n width.
#' @export
break_lines <- function(x, n = 80, sep = "\\. ", collapse = "\n") {
  stopifnot(is.character(x))
  map_chr("break_lines_", x,
          MoreArgs = list(n = n, sep = sep, collapse = collapse))
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
rm_retweets.data.frame <- function(x,
                                   text = "text",
                                   is_retweet = "is_retweet") {
  stopifnot(is.character(x[[text]]))
  if ("is_retweet" %in% names(x)) {
    stopifnot(is.logical(x[[is_retweet]]))
    is_retweet <- x[[is_retweet]]
  } else {
    is_retweet <- grep(
      "^RT ", x[[text]], ignore.case = TRUE, value = TRUE
    )
  }
  x[[text]][is_retweet] <- ""
  x
}

#' @export
rm_retweets.list <- function(x,
                             text = "text",
                             is_retweet = "is_retweet") {
  stopifnot(is.character(x[[text]]))
  if ("is_retweet" %in% names(x)) {
    stopifnot(is.logical(x[[is_retweet]]))
    is_retweet <- x[[is_retweet]]
  } else {
    is_retweet <- grep(
      "^RT ", x[[text]], ignore.case = TRUE, value = TRUE
    )
  }
  x[[text]][is_retweet] <- ""
  x
}

#' @export
rm_retweets.character <- function(x, is_retweet = NULL) {
  if (is.null(is_retweet)) {
    is_retweet <- grep("^RT ", x, ignore.case = TRUE,  value = TRUE)
  }
  stopifnot(!identical(length(x), length(is_retweet)))
  x[is_retweet] <- ""
  x
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
  x <- map_chr("break_line", x, MoreArgs = list(n = n))
  if (!is.null(collapse)) {
    x <- paste(x, collapse = collapse)
  }
  x
}
