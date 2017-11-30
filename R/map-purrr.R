## this whole file was straight ripped off from "tidyverse/hms":
## Source: https://github.com/tidyverse/hms/blob/master/R/compat-purrr.R
#### mwk

# nocov start - compat-purrr (last updated: rlang 0.1.9000)

# This file serves as a reference for compatibility functions for
# purrr. They are not drop-in replacements but allow a similar style
# of programming. This is useful in cases where purrr is too heavy a
# package to depend on. Please find the most recent version in rlang's
# repository.


#' Apply a function to each element of a vector
#'
#' The map functions transform their input by applying a function to
#' each element and returning a vector the same length as the input.
#'
#' @param .x A list or atomic vector.
#' @param .f A function, formula, or atomic vector.
#' @param ... Additional arguments passed on to '.f'.
#' @return All functions return a vector the same length as '.x'.
#' @examples
#' 1:10 %>%
#'   map(rnorm, n = 10) %>%
#'   map_dbl(mean)
#'
#' # Or use an anonymous function
#' 1:10 %>%
#'   map(function(x) rnorm(10, x))
#'
#' # Or a formula
#' #' 1:10 %>%
#'   map(~ rnorm(10, .x))
#'
#' # Extract by name or position
#' # .default specifies value for elements that are missing or NULL
#' l1 <- list(list(a = 1L), list(a = NULL, b = 2L), list(b = 3L))
#' l1 %>% map("a", .default = "???")
#' l1 %>% map_int("b", .default = NA)
#' l1 %>% map_int(2, .default = NA)
#'
#' # Supply multiple values to index deeply into a list
#' l2 <- list(
#'   list(num = 1:3,#' letters[1:3]),
#'   list(num = 101:103, letters[4:6]),
#'   list()
#' )
#' l2 %>% map(c(2, 2))
#'
#' # Use a list to build an extractor that mixes numeric indices and names,
#' # and .default to provide a default value if the element does not exist
#' l2 %>% map(list("num", 3))
#' l2 %>% map_int(list("num", 3), .default = NA)
#'
#' # A more realistic example: split a data frame into pieces, fit a
#' # model to each piece, summarise and extract R^2
#' mtcars %>%
#'   split(.$cyl) %>%
#'   map(~ lm(mpg ~ wt, data = .x)) %>%
#'   map(summary) %>%
#'   map_dbl("r.squared")
#'
#' # Use map_lgl(), map_dbl(), etc to reduce to a vector.
#' # * list
#' mtcars %>% map(sum)
#' # * vector
#' mtcars %>% map_dbl(sum)
#'
#' # If each element of the output is a data frame, use
#' # map_dfr to row-bind them together:
#' mtcars %>%
#'   split(.$cyl) %>%
#'   map(~ lm(mpg ~ wt, data = .x)) %>%
#'   map_dfr(~ as.data.frame(t(as.matrix(coef(.)))))
#' # (if you also want to preserve the variable names see
#' # the broom package)
#' @export
map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

#' @param .p A single predicate function, a formula describing such a
#'   predicate function, or a logical vector of the same length as
#'   '.x'. Alternatively, if the elements of '.x' are themselves lists
#'   of objects, a string indicating the name of a logical element in
#'   the inner lists. Only those elements where '.p' evaluates to
#'   'TRUE' will be modified.
#' @inheritParams map
#' @rdname map
#' @export
map_if <- function(.x, .p, .f, ...) {
  matches <- probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x
}

map_mold <- function(.x, .f, .mold, ...) {
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}

#' @inheritParams map
#' @rdname map
#' @export
map_lgl <- function(.x, .f, ...) {
  map_mold(.x, .f, logical(1), ...)
}

#' @inheritParams map
#' @rdname map
#' @export
map_int <- function(.x, .f, ...) {
  map_mold(.x, .f, integer(1), ...)
}

#' @inheritParams map
#' @rdname map
#' @export
map_dbl <- function(.x, .f, ...) {
  map_mold(.x, .f, double(1), ...)
}

#' @inheritParams map
#' @rdname map
#' @export
map_chr <- function(.x, .f, ...) {
  map_mold(.x, .f, character(1), ...)
}

#' @inheritParams map
#' @rdname map
#' @export
map_cpl <- function(.x, .f, ...) {
  map_mold(.x, .f, complex(1), ...)
}

?purrr::pluck
#' Pluck out a single an element from a vector or environment
#'
#' @param .x A vector or environment
#' @param .f Accessors for indexing into the object. Can be an integer position, a string name, or an accessor function.
#' @export
pluck <- function(.x, .f) {
  map(.x, `[[`, .f)
}
pluck_lgl <- function(.x, .f) {
  map_lgl(.x, `[[`, .f)
}
pluck_int <- function(.x, .f) {
  map_int(.x, `[[`, .f)
}
pluck_dbl <- function(.x, .f) {
  map_dbl(.x, `[[`, .f)
}
pluck_chr <- function(.x, .f) {
  map_chr(.x, `[[`, .f)
}
pluck_cpl <- function(.x, .f) {
  map_cpl(.x, `[[`, .f)
}

map2 <- function(.x, .y, .f, ...) {
  Map(.f, .x, .y, ...)
}
map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}
map2_int <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "integer")
}
map2_dbl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "double")
}
map2_chr <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "character")
}
map2_cpl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "complex")
}

args_recycle <- function(args) {
  lengths <- map_int(args, length)
  n <- max(lengths)

  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- map(args[to_recycle], function(x) rep.int(x, n))

  args
}
pmap <- function(.l, .f, ...) {
  args <- args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}

probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    map_lgl(.x, .p, ...)
  }
}

keep <- function(.x, .f, ...) {
  .x[probe(.x, .f, ...)]
}
discard <- function(.x, .p, ...) {
  sel <- probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}

compact <- function(.x) {
  Filter(length, .x)
}

#' transpose a list
#'
#' Transpose turns a list-of-lists "inside-out"; it turns a pair of
#' lists into a list of pairs, or a list of pairs into pair of
#' lists. For example, if you had a list of length n where each
#' component had values 'a' and 'b', 'transpose()' would make a list
#' with elements 'a' and 'b' that contained lists of length n. It's
#' called transpose because 'x[[1]][[2]]' is equivalent to
#' 'transpose(x)[[2]][[1]]'.
#'
#' @param .l A list of vectors to zip. The first element is used as
#'   the template; you'll get a warning if a sub-list is not the same
#'   length as the first element.
#' @param .names For efficiency, 'transpose()' usually inspects the
#'   first component of '.l' to determine the structure. Use '.names'
#'   if you want to override this default.
#' @return A list with indexing transposed compared to '.l'.
#' @examples
#' x <- rerun(5, x = runif(1), y = runif(5))
#' x %>% str()
#' x %>% transpose() %>% str()
#' # Back to where we started
#' x %>% transpose() %>% transpose() %>% str()
#'
#' # transpose() is useful in conjunction with safely() & quietly()
#' x <- list("a", 1, 2)
#' y <- x %>% map(safely(log))
#' y %>% str()
#' y %>% transpose() %>% str()
#'
#' # Use simplify_all() to reduce to atomic vectors where possible
#' x <- list(list(a = 1, b = 2), list(a = 3, b = 4), list(a = 5, b = 6))
#' x %>% transpose()
#' x %>% transpose() %>% simplify_all()
#'
#' # Provide explicit component names to prevent loss of those that don't
#' # appear in first component
#' ll <- list(
#'   list(x = 1, y = "one"),
#'   list(z = "deux", x = 2)
#' )
#' ll %>% transpose()
#' nms <- ll %>% map(names) %>% reduce(union)
#' ll %>% transpose(.names = nms)
#' @export
transpose <- function(.l) {
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  } else {
    fields <- set_names(inner_names)
  }

  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}

every <- function(.x, .p, ...) {
  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) return(FALSE)
  }
  TRUE
}
some <- function(.x, .p, ...) {
  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) return(TRUE)
  }
  FALSE
}
negate <- function(.p) {
  function(...) !.p(...)
}

reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}
reduce_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}
accumulate <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}
accumulate_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}

# nocov end
