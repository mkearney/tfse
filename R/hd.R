#' Head data
#'
#' Returns first i elements of data object.
#'
#' @param x Input
#' @param i Number of elements to return from first, defaults to 10.
#' @return Data with first i elements.
#' @export
hd <- function(x, i = 10) UseMethod("hd")

#' @export
hd.default <- function(x, i = 10) {
  if (length(x) > i) {
    x <- x[seq_len(i)]
  }
  x
}

#' @export
hd.data.frame <- function(x, i = 10) {
  if (nrow(x) > i) {
    rn <- row.names(x)[seq_len(i)]
    x <- x[seq_len(i), ]
    row.names(x) <- rn
  }
  x
}

#' @export
hd.tbl <- function(x, i = 10) {
  if (nrow(x) > i) {
    x <- x[seq_len(i), ]
  }
  x
}


#' @export
hd.matrix <- function(x, i = 10) {
  if (nrow(x) > i) {
    rn <- row.names(x)[seq_len(i)]
    x <- x[seq_len(i), ]
    row.names(x) <- rn
  }
  x
}


#' Tail data
#'
#' Returns last i elements of data object.
#'
#' @param x Input
#' @param i Number of elements to return from last, defaults to 10.
#' @return Data with last i elements.
#' @export
tl <- function(x, i = 10) UseMethod("tl")

#' @export
tl.default <- function(x, i = 10) {
  if (length(x) > i) {
    x <- x[length(x) - seq_len(i) + 1L]
  }
  x
}

#' @export
tl.data.frame <- function(x, i = 10) {
  if (nrow(x) > i) {
    sq <- rev(nrow(x) - seq_len(i) + 1L)
    rn <- row.names(x)[sq]
    x <- x[sq, ]
    row.names(x) <- rn
  }
  x
}

#' @export
tl.tbl <- function(x, i = 10) {
  if (nrow(x) > i) {
    x <- x[nrow(x) - seq_len(i) + 1L, ]
  }
  x
}

#' @export
tl.tbl_df <- function(x, i = 10) {
  if (nrow(x) > i) {
    x <- x[nrow(x) - seq_len(i) + 1L, ]
  }
  x
}


#' @export
tl.matrix <- function(x, i = 10) {
  if (nrow(x) > i) {
    sq <- rev(nrow(x) - seq_len(i) + 1L)
    rn <- row.names(x)[sq]
    x <- x[sq, ]
    row.names(x) <- rn
  }
  x
}
