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
as_tbl <- function(...) {
  x <- rlang::enquos(...)
  if (length(x) == 1 && is.list(rlang::eval_tidy(x[[1]])[[1]]) &&
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
tbl_df <- tibble::data_frame
