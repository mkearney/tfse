
#' extract reg expr matches
#'
#' A wrapper around the base function combo of gregexpr and regmatches
#'
#' @param x Text data.
#' @param pat Reg ex pattern
#' @param drop Logical indicating whether to drop empty matches. Defaults to FALSE.
#' @param ... Other args (like ignore.case) passed to gregexpr
#' @return Matching expression from text.
#' @export
regmatches_ <- function(x, pat, drop = FALSE, ...) UseMethod("regmatches_")

#' @inheritParams regmatches_
#' @rdname regmatches_
#' @export
regmatches_first <- function(x, pat, drop = FALSE, ...) UseMethod("regmatches_first")


#' @export
regmatches_.default <- function(x, pat, ...) {
  if (length(x) == 0) return(NULL)
  stop("input must be character or list of character vectors", call. = FALSE)
}

#' @export
regmatches_.factor <- function(x, pat, drop = FALSE, ...) {
  x <- as.character(x)
  regmatches_(x, pat)
}

#' @export
regmatches_.character <- function(x, pat, drop = FALSE, ...) {
  m <- gregexpr_(x, pat, ...)
  args <- list(x = x, m = m)
  x <- do.call("regmatches", args)
  if (drop) {
    x <- unlist(x[lengths(x) > 0], use.names = FALSE)
  } else {
    x[lengths(x) == 0] <- ""
    if (sum(lengths(x) > 1) == 0L) {
      x <- unlist(x)
    }
  }
  x
}


#' @export
regmatches_.list <- function(x, pat, drop = FALSE, ...) {
  x <- chr2fct(x)
  if (!all(vapply(x, is.character,
    FUN.VALUE = logical(1), USE.NAMES = FALSE))) {
    stop("input must be character or list of character vectors", call. = FALSE)
  }
  m <- gregexpr_(x, pat, ...)
  args <- list(x = x, m = m)
  x <- do.call("regmatches", args)
  if (drop) {
    x <- x[lengths(x) > 0]
    if (sum(lengths(x) > 1) == 0L) {
      x <- unlist(x)
    }
  } else {
    x[lengths(x) == 0] <- ""
  }
  x
}

#' smart gregexpr wrapper
#'
#' @param x Input text
#' @param pat Reg ex pattern
#' @param ... Other args passed to base (g)regexpr
#' @return Pattern match positions
#' @export
gregexpr_ <- function(x, pat, ...) {
  args <- list(pattern = pat, text = x, ...)
  if ("perl" %in% names(args)) {
    args$perl <- args$perl
  } else {
    if (grepl("\\(\\?.*\\)", pat)) {
      args$perl <- TRUE
    } else {
      args$perl <- FALSE
    }
  }
  do.call("gregexpr", args)
}

#' @export
regmatches_first.default <- function(x, pat, ...) {
  stop("input must be character or list of character vectors", call. = FALSE)
}

#' @export
regmatches_first.factor <- function(x, pat, drop = FALSE, ...) {
  x <- as.character(x)
  regmatches_first(x, pat)
}

#' @export
regmatches_first.character <- function(x, pat, drop = FALSE, ...) {
  m <- regexpr_(x, pat, ...)
  args <- list(x = x, m = m)
  x <- do.call("regmatches", args)
  if (drop) {
    x <- unlist(x[lengths(x) > 0], use.names = FALSE)
  } else {
    x[lengths(x) == 0] <- ""
    if (sum(lengths(x) > 1) == 0L) {
      x <- unlist(x)
    }
  }
  x
}


#' @export
regmatches_first.list <- function(x, pat, drop = FALSE, ...) {
  x <- chr2fct(x)
  if (!all(vapply(x, is.character,
    FUN.VALUE = logical(1), USE.NAMES = FALSE))) {
    stop("input must be character or list of character vectors", call. = FALSE)
  }
  m <- regexpr_(x, pat, ...)
  args <- list(x = x, m = m)
  x <- do.call("regmatches", args)
  if (drop) {
    x <- x[lengths(x) > 0]
    if (sum(lengths(x) > 1) == 0L) {
      x <- unlist(x)
    }
  } else {
    x[lengths(x) == 0] <- ""
  }
  x
}

#' @inheritParams gregexpr_
#' @rdname gregexpr_
#' @export
regexpr_ <- function(x, pat, ...) {
  args <- list(pattern = pat, text = x, ...)
  if ("perl" %in% names(args)) {
    args$perl <- args$perl
  } else {
    if (grepl("\\(\\?.*\\)", pat)) {
      args$perl <- TRUE
    } else {
      args$perl <- FALSE
    }
  }
  do.call("regexpr", args)
}


chr2fct <- function(x) {
  if (is.data.frame(x)) {
    x[1:ncol(x)] <- lapply(x, chr2fct_)
  } else if (is.list(x)) {
    x <- lapply(x, chr2fct_)
  } else {
    x <- chr2fct_(x)
  }
  x
}

chr2fct_ <- function(x) if (is.factor(x)) as.character(x) else x

