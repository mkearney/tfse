
#' Print message about starting a task
#'
#' Prints a next-arrow bulleted message presumably about task completion
#'
#' @param ... Strings collapsed (with no additional space added) into gray
#'   message prefixed with a next-arrow emoji (color and emoji print may
#'   appear differently depending on your system/UI configuration)
#' @return A printed message
#' @export
print_start <- function(...) {
  x <- paste(c(...), collapse = "")
  gray <- crayon::make_style("#888888")
  x <- gray(paste0("\U21AA", " ", x))
  cat(x, fill = TRUE)
}

#' Print message about completing a task
#'
#' Prints a check-mark bulleted message presumably about task completion
#'
#' @param ... Strings collapsed (with no additional space added) into black
#'   message prefixed with a heavy-check emoji (color and emoji print may
#'   appear differently depending on your system/UI configuration)
#' @return A printed message
#' @export
print_complete <- function(...) {
  x <- paste(c(...), collapse = "")
  x <- crayon::black(paste0("\U2714", " ", x))
  cat(crayon::bold(crayon::black(x)), fill = TRUE)
}

