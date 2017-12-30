blank2na <- function(x) {
  y <- trim_ws(x)
  x[y == "" | y == "NA"] <- NA_character_
  x
}

#' Calculate and sort by API use
#'
#' Counts number of requests used for each API (observation) and
#' arranges in descending order
#'
#' @param x Parsed token data
#' @return Arrange data frame with calculated used column.
#' @export
token_used <- function(x) {
  x$used <- x$limit - x$remaining
  x <- x[order(x$used, decreasing = TRUE), ]
  x[, c("query", "limit", "remaining", "reset", "used")]
}
