#' tbl
#'
#' See \code{tibble::\link[tibble]{as_tibble}} for details.
#'
#' @export
#' @param x Data
#' @param row.names Logical indicating whether to convert non-null row names
#'   into the first column.
#' @rdname tbl
#' @importFrom tibble as_tibble
tbl <- function(x, row.names = FALSE) {
  if (row.names && !identical(as.character(seq_len(nrow(x))), row.names(x))) {
    x$row_names <- row.names(x)
    repos_front(x, row_names)
  } else {
    tibble::as_tibble(x, validate = FALSE)
  }
}


#' as_tbl
#'
#' See \code{tibble::\link[tibble]{as_tibble}} for details.
#'
#' @inheritParams tbl
#' @rdname tbl
#' @importFrom tibble as_tibble
#' @export
as_tbl <- function(x, row.names = FALSE) {
  if (row.names && !identical(as.character(seq_len(nrow(x))), row.names(x))) {
    x$row_names <- row.names(x)
    repos_front(x, row_names)
  } else {
    tibble::as_tibble(x, validate = FALSE)
  }
}



#' tbl_frame
#'
#' @rdname tbl
#' @export
#' @importFrom tibble data_frame
tbl_frame <- tibble::data_frame


#' @inheritParams tbl
#' @rdname tbl
#' @export
as_tbl_smart <- function(x) {
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

#' @export
#' @rdname tbl
#' @inheritParams tbl
tblframe <- function(x) tbl_frame(!!rlang::quo_text(rlang::enquo(x)) := x)


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

#' tidyselector
#'
#' Select columns using tidy eval
#'
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
