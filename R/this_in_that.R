#' Where is this in that?
#'
#' Looks up (matches) the position of \code{this} in \code{that} (table)
#'
#' @param this Values to look up in that
#' @param that Value positions matched to this
#' @param value Optional, values to be returned rather than the default, which
#'   returns positions (integers)
#' @export
this_in_that <- function(this, that, value = NULL) {
  m <- match(this, that)
  if (is.null(value)) {
    return(m)
  }
  if (length(that) != length(value)) {
    stop("'value' must be same length as 'that' (table)", call. = FALSE)
  }
  value[m]
}
