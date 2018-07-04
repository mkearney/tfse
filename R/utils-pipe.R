#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' tibble
#'
#' See \code{tibble::\link[tibble]{as_tibble}} for details.
#'
#' @name tibble
#' @rdname tibble
#' @keywords internal
#' @export
#' @importFrom tibble as_tibble
as_tbl <- function(x, row.names = FALSE) {
  if (row.names && !identical(as.character(seq_len(nrow(x))), row.names(x))) {
    x$row_names <- row.names(x)
    repos_front(x, row_names)
  } else {
    tibble::as_tibble(x, validate = FALSE)
  }
}

#' @param ... Data
#' @export
as_tbl_smart <- function(...) {
  if (length(x) == 1 && inherits(rlang::eval_tidy(x[[1]]), "table")) {
    x <- rlang::eval_tidy(x[[1]])
    names(x) <- expr_names(x)
  } else if (length(x) == 1 && is.list(rlang::eval_tidy(x[[1]])[[1]]) &&
      length(rlang::eval_tidy(x[[1]])) == 1 &
      length(unique(lengths(rlang::eval_tidy(x[[1]])[[1]]))) == 1) {
    x <- rlang::eval_tidy(x[[1]])[[1]]
  } else if (length(x) == 1 && is.list(rlang::eval_tidy(x[[1]])[[1]])) {
    x <- rlang::eval_tidy(x[[1]])
  } else if (length(x) == 1 && is.list(rlang::eval_tidy(x[[1]]))) {
    nms <- as.character(x[[1]][[2]])[-1]
    x <- rlang::eval_tidy(x[[1]])
    if (is.null(names(x))) {
      names(x) <- nms
    } else {
      names(x)[names(x) == ""] <- nms[names(x) == ""]
    }
  } else {
    x <- purrr::map(x, rlang::eval_tidy)
    names(x) <- expr_names(x)
  }
  tibble::as_tibble(x, validate = FALSE)
}

#' @param data Data frame
#' @param ... Variables to make table from
#' @return tbl freq table
#' @export
ntbl <- function(data, ...) {
  data <- rlang::with_env(data, tidyselector(data, ...))
  as_tbl(table(data))
}

#' move vars to front
#'
#' @param data data frame
#' @param ... columns to move to front
#' @rdname repos
#' @export
repos_front <- function(data, ...) {
  re <- rlang::with_env(data, tidyselector(data, ...))
  as_tbl(cbind(re, data[!names(data) %in% names(re)]))
}

#' move vars to front
#'
#' @param data data frame
#' @param ... columns to move to front
#' @rdname repos
#' @export
repos_back <- function(data, ...) {
  re <- rlang::with_env(data, tidyselector(data, ...))
  #re <- re[rev(seq_len(ncol(re)))]
  as_tbl(cbind(data[!names(data) %in% names(re)], re))
}

#' @param data data frame
#' @param ... vars to select
#' @export
tidyselector <- function(data, ...) {
  vars <- tidyselect::vars_select(names(data), ...)
  if (length(vars) > 0) {
    data <- data[vars]
  }
  data
}

expr_names <- function(x) {
  exprnames <- purrr::map_chr(x, ~ sub("~", "", rlang::expr_text(.x)))
  ifelse(names(x) == "", exprnames, names(x))
}

is_recursive <- function(x) vapply(x, is.recursive, logical(1))

#' tibble
#'
#' See \code{tibble::\link[tibble]{as_tibble}} for details.
#'
#' @name tibble
#' @rdname tibble
#' @keywords internal
#' @export
#' @importFrom tibble data_frame
tbl_frame <- tibble::data_frame
