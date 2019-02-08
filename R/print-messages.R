
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
  graycol <- function(x) {
    sprintf("\033[38;5;244m%s\033[39m", as.character(x))
  }
  x <- paste0(green_col("\U21AA"), " ", graycol(x))
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
  x <- paste0(green_col("\U2714"), " ", x)
  cat(x, fill = TRUE)
}

bold_txt <- function(x) {
  sprintf("\033[1m%s\033[22m", as.character(x))
}

green_col <- function(x) {
  sprintf("\033[32m%s\033[39m", as.character(x))
}
