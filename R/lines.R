
##----------------------------------------------------------------------------##
##                               READ/WRITE FUNS                              ##
##----------------------------------------------------------------------------##

#' readlines
#'
#' Verbs object
#'
#' @param x Input
#' @param ... Other args passed to \code{readLines}.
#' @return Output
#' @export
readlines <- function(x, ...) {
  con <- file(x)
  x <- readLines(con, warn = FALSE, ...)
  close(con)
  x
}

#' writelines
#'
#' Writes lines to file
#'
#' @param x Text to output.
#' @param file File name
#' @param ... Other args passed to \code{writeLines}.
#' @return Saves/overwrites to file.
#' @export
writelines <- function(x, file, ...) {
  con <- file(file)
  writeLines(x, con, ...)
  close(con)
  message("Wrote lines to ", file)
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
  menu(a)
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
