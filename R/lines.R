
##----------------------------------------------------------------------------##
##                               READ/WRITE FUNS                              ##
##----------------------------------------------------------------------------##

#' readlines
#'
#' Read lines of file
#'
#' Simple wrapper around \link[base]{readLines} that automates opening and
#'   closing of connection file.
#'
#' @param x Input
#' @param ... Other args passed to \code{readLines}.
#' @return Output
#' @export
readlines <- function(x, ...) {
  dots <- list(...)
  dots <- add_arg_if(dots, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
  if (is_url(x)) {
    con <- url(x, encoding = dots$encoding)
  } else {
    con <- file(x, encoding = dots$encoding)
  }
  on.exit(close(con))
  dots$con <- con
  do.call("readLines", dots)
}



##----------------------------------------------------------------------------##
##                           INTERACTIVE READ/WRITE                           ##
##----------------------------------------------------------------------------##

#' menuline
#'
#' Creates interactive multiple choice question.
#'
#'
#' @param q Question to be asked in interactive session.
#' @param a Answer choices.
#' @return Selection provided by user in interactive session.
#' @export
menuline <- function(q, a) {
  message(q)
  utils::menu(a)
}


#' readline_
#'
#' Worry free way to read lines from interactive sessions.
#'
#' @param ... Character string or vector to be used as prompt during
#'   interactive R session. Ultimately, this function only sends a
#'   single string to the user, but it will accept a vector if you're
#'   picky about not creating strings of a certain width.
#' @return Input entered during interactive session without extra quotes.
#' @export
readline_ <- function(...) {
  input <- readline(paste(unlist(c(...)), collapse = ""))
  gsub("^\"|^'|\"$|'$", "", input)
}

#' pbcopy
#'
#' Adds input to clipboard for pasting
#'
#' @param x Input passed to cat function.
#' @return Prints x to clipboard.
#' @examples
#' ## alphabet as string
#' pbcopy(paste(letters, collapse = ""))
#' ## paste e.g., C-v
#' @export
pbcopy <- function(x) {
  if (.Platform$OS.type == "unix" &&
    !identical(Sys.which("pbcopy"), "")) {
    con <- pipe("pbcopy", "w")
  } else if (.Platform$OS.type == "windows") {
    con <- file("clipboard", "w")
  } else {
    stop("Please install 'pbcopy'", call. = FALSE)
  }
  close(con)
}
