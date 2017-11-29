
#' Base version of map over function
#'
#' Base version of map over function equivalent of lapply
#'
#' @param .f function
#' @param .x first param value
#' @param ... additional args
#' @return Data returned as list vector
#' @export
#' @examples
#'
#' map(is.numeric, mtcars)
map <- function(.f, .x, ...) {
  lapply(.x, .f, ...)
}

#' Adds names to data object
#'
#' Adds names to data object wrapper of names<-
#'
#' @param x Data
#' @param names Names.
#' @return (Re)named data.
#' @export
#' @examples
#' add_names(mtcars, paste0("mtcars", seq_len(ncol(mtcars))))
add_names <- function(x, names) `names<-`(x, names)

map_mold <- function(.f, .x, .mold, ...) {
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  add_names(out, names(.x))
}

#' @inheritParams map
#' @export
#' @rdname map
map_lgl <- function(.f, .x, ...) {
  map_mold(.f, .x, logical(1), ...)
}
#' @inheritParams map
#' @export
#' @rdname map
map_int <- function(.f, .x, ...) {
  map_mold(.f, .x, integer(1), ...)
}
#' @inheritParams map
#' @export
#' @rdname map
map_dbl <- function(.f, .x, ...) {
  map_mold(.f, .x, double(1), ...)
}
#' @inheritParams map
#' @export
#' @rdname map
map_chr <- function(.f, .x, ...) {
  map_mold(.f, .x, character(1), ...)
}
#' @inheritParams map
#' @export
#' @rdname map
map_cpl <- function(.f, .x, ...) {
  map_mold(.f, .x, complex(1), ...)
}

#' Pluck elements from list
#'
#' Extracts from list named or integer position data.
#'
#' @param .x Data
#' @param .f Named data object or integer position number
#' @return Data plucked from input
#' @export
#' @examples
#' pluck(list(mtcars), "cyl")
#'
pluck <- function(.x, .f) {
  map(`[[`, .x, .f)
}

#' @inheritParams pluck
#' @export
#' @rdname pluck
pluck_lgl <- function(.x, .f) {
  map_lgl(`[[`, .x, .f)
}
#' @inheritParams pluck
#' @export
#' @rdname pluck
pluck_int <- function(.x, .f) {
  map_int(`[[`, .x, .f)
}
#' @inheritParams pluck
#' @export
#' @rdname pluck
pluck_dbl <- function(.x, .f) {
  map_dbl(`[[`, .x, .f)
}
#' @inheritParams pluck
#' @export
#' @rdname pluck
pluck_chr <- function(.x, .f) {
  map_chr(`[[`, .x, .f)
}
#' @inheritParams pluck
#' @export
#' @rdname pluck
pluck_cpl <- function(.x, .f) {
  map_cpl(`[[`, .x, .f)
}
