

#' Number of unique elements
#'
#' Estimates number of unique elements in a data object
#'
#' @param x Input data
#' @return Integer number of unique elements
#' @export
n_uq <- function(x) NROW(unique(x))


#' Create data set
#'
#' Simple way to make data frames
#'
#' @param ... Data to be converted to data frame
#' @return A tibble data frame
#' @export
data_set <- function(...) {
  ## gather input as list
  x <- list(...)

  ## if data frame
  if (length(x) == 1L && is.data.frame(x[[1]])) {
    x <- x[[1]]
  }

  ## stretch single-value vectors to match row number
  lens <- lengths(x)
  if (n_uq(lens) == 2L && 1L %in% lens) {
    x[lens == 1L] <- lapply(x[lens == 1L], rep, max(lens))
  }

  ## var names
  nms <- names(x)
  no_nms <- !nzchar(nms)
  nms[no_nms] <- paste0("x", seq_len(sum(no_nms)))

  ## output
  structure(
    x,
    names = nms,
    row.names = seq_along(x[[1]]),
    class = c("tbl_df", "tbl", "data.frame")
  )
}


#' Print data frame as a single column
#'
#' Prints first row of data frame
#'
#' @param x Input data frame
#' @return Prints first row and variable names
#' @export
print_as_col <- function(x) {
  x <- data_set(name = names(x), value = row2vec(x[1, ]))
  nms <- names(x)
  x <- as.list(x)
  for (i in seq_along(x)) {
    if (is.numeric(x[[i]])) {
      x[[i]] <- round(x[[i]], 4)
    }
    x[[i]] <- c(nms[i], x[[i]])
  }
  x <- c(list(c("", seq_along(x[[1]][-1]))), x)
  x[[1]] <- paste0("#", x[[1]])
  x[[1]][1] <- ""
  x <- lapply(x, format_col)
  x[[1]] <- crayon::make_style("#aaaaaa")(x[[1]])
  for (i in seq_len(length(x[[1]]))) {
    l <- lapply(x, function(j) j[i])
    #l <- lapply(l, function(x) paste(x, collapse = " "))
    l <- paste(l, collapse = " ")
    cat(l, fill = TRUE)
  }
}

format_col <- function(x) {
  col1 <- max(nchar(x)) + 1
  sp <- col1 - nchar(x)
  sp <- sapply(sp, function(i) paste0(rep(" ", i), collapse = ""))
  x <- paste0(sp, x)
  x
}


row2vec <- function(x) unlist(x, use.names = FALSE)

