#' square_section
#'
#' @description Load clipboard with code chunk square. Paste to
#'   insert square into R script file at cursor location.
#'
#' @export
square_section <- function() {
  data <- paste_text()
  clip <- pipe("pbcopy", "w")
  cat(data, file = clip)
  close(clip)
}

paste_text <- function() {
  '##------------------------------------------------------------------##\n##                            header                                ##\n##------------------------------------------------------------------##'
}
