
#' match arg to choices
#'
#' Wrapper around \link[base]{match.arg} that defaults to ignoring case and
#'   trimming white space
#'
#' @param arg a character vector (of length one unless several.ok is TRUE) or
#'   NULL
#' @param choices a character vector of candidate values
#' @param multiple logical specifying if arg should be allowed to have more than
#'   one element. Defaults to FALSE
#' @param ignore_case logical indicating whether to ignore capitalization.
#'   Defaults to TRUE
#' @param trim_ws logical indicating whether to trim surrounding white space.
#'   Defaults to TRUE
#' @return Value(s) matched via partial matching.
#' @export
match_arg <- function(arg, choices,
                      multiple = FALSE,
                      ignore_case = TRUE,
                      trim_ws = TRUE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]],
      envir = sys.frame(sysP))
  }
  if (is.null(arg))
    return(choices[1L])
  else if (!is.character(arg))
    stop("'arg' must be NULL or a character vector")
  if (!multiple) {
    if (identical(arg, choices))
      return(arg[1L])
    if (length(arg) > 1L)
      stop("'arg' must be of length 1")
  }
  else if (length(arg) == 0L)
    stop("'arg' must be of length >= 1")
  if (trim_ws) {
    arg <- tfse::trim_ws(arg)
  }
  if (ignore_case) {
    arg <- tolower(arg)
  }
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(i == 0L))
    stop(gettextf("'arg' should be one of %s",
      paste(dQuote(choices), collapse = ", ")),
      domain = NA)
  i <- i[i > 0L]
  if (!multiple && length(i) > 1)
    stop("there is more than one match in 'match.arg'")
  choices[i]
}
