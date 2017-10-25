#' tbl
#'
#' Tibble functions
#'
#' @param ... Arguments passed to \code{\link{as_tibble}}, \code{\link{tbl_df}},
#'   or \code{\link{data_frame}}.
#' @param validate Logical indicating whether to validate tibble. Defaults to
#'   FALSE because it's slightly faster.
#' @return A tibble data frame
#' @examples
#' as_tbl(data.frame(x = rnorm(10), y = rnorm(10)))
#' @importFrom tibble as_tibble
#' @rdname tbl
#' @export
as_tbl <- function(...) {
  args <- list(...)
  if (length(args) == 0L) {
    args <- list(data.frame())
  }
  if ("validate" %!in% names(args)) {
    args[["validate"]] <- FALSE
  }
  do.call("as_tibble", args)
}

#' Pipe
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @name tbl_df
#' @rdname tbl
#' @inheritParams tbl
#' @examples
#' tbl_df(mtcars)
#' @export
#' @importFrom dplyr tbl_df
NULL


#' @name data_frame
#' @rdname tbl
#' @inheritParams tbl
#' @examples
#' data_frame(x = rnorm(10), y = rnorm(10)))
#' @export
#' @importFrom tibble data_frame
NULL


#' @name as_tibble
#' @rdname tbl
#' @inheritParams tbl
#' @examples
#' as_tibble(mtcars)
#' @export
#' @importFrom tibble as_tibble
NULL
