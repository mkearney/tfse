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


#' Filter rows
#'
#' @name filter
#' @rdname filter
#' @keywords internal
#' @export
#' @importFrom dplyr filter
#' @usage filter(.data, ...)
NULL


#' Select columns
#'
#' @name select
#' @rdname select
#' @keywords internal
#' @export
#' @importFrom dplyr select
#' @usage select(.data, ...)
NULL


#' Group observations by column
#'
#' @name group_by
#' @rdname group_by
#' @keywords internal
#' @export
#' @importFrom dplyr group_by
#' @usage group_by(.data, ..., add = FALSE)
NULL


#' Join data frames
#'
#' @name full_join
#' @rdname join
#' @keywords internal
#' @export
#' @importFrom dplyr full_join
#' @usage full_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
NULL

#' @name left_join
#' @rdname join
#' @keywords internal
#' @export
#' @importFrom dplyr left_join
#' @usage left_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
NULL

#' @name right_join
#' @rdname join
#' @keywords internal
#' @export
#' @importFrom dplyr right_join
#' @usage right_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
NULL

#' Summarise data frame
#'
#' @name summarise
#' @rdname summarise
#' @keywords internal
#' @export
#' @importFrom dplyr summarise
#' @usage summarise(.data, ...)
NULL

#' @name summarize
#' @rdname summarise
#' @keywords internal
#' @export
#' @importFrom dplyr summarize
#' @usage summarize(.data, ...)
NULL

#' Mutate data frame
#'
#' @name mutate
#' @rdname mutate
#' @keywords internal
#' @export
#' @importFrom dplyr mutate
#' @usage mutate(.data, ...)
NULL

#' Convert data from wide to long
#'
#' @name gather
#' @rdname gather
#' @keywords internal
#' @export
#' @importFrom tidyr gather
#' @usage gather(data, key = "key", value = "value", ..., na.rm = FALSE,
#'   convert = FALSE, factor_key = FALSE)
NULL
