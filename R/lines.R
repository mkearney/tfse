
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
  dots <- list(...)
  if (has_name_(dots, "append") && dots[["append"]]) {
    dots[["x"]] <- paste(x, collapse = "\n")
    dots[["file"]] <- file
    do.call("append_lines", dots)
  } else {
    con <- file(file)
    writeLines(x, con, ...)
    close(con)
  }
}

is_named <- function(x) UseMethod("is_named")
is_named.default <- function(x) !is.null(names(x))

are_named <- function(x) UseMethod("are_named")
are_named.default <- function(x) is_named(x) & "" %nin% names(x)

readlines <- function(x, ...) {
  con <- file(x)
  x <- readLines(con, warn = FALSE, ...)
  close(con)
  x
}

define_args <- function(args, ...) {
  dots <- list(...)
  nms <- names(dots)
  for (i in nms) {
    if (!has_name_(args, i)) {
      args[[i]] <- dots[[i]]
    }
  }
  args
}

append_lines <- function(x, ...) {
  args <- define_args(
    c(x, list(...)),
    append = TRUE,
    fill = TRUE
  )
  do.call("cat", args)
}

is_incomplete <- function(x) {
  if (!file.exists(x)) return(FALSE)
  con <- file(x)
  x <- tryCatch(readLines(con), warning = function(w) return(TRUE))
  close(con)
  ifelse(isTRUE(x), TRUE, FALSE)
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
  con <- pipe("pbcopy", "w")
  cat(x, file = con)
  close(con)
}
