
#' boxcode
#' 
#' Load clipboard with code chunk square. Paste to insert square into R script 
#'   file at cursor location.
#'   
#' @param label Name of section/block
#' @return Text for code box saved into clipboard. Paste to use at cursor.
#' 
#' @export
boxcode <- function(label = "") {
  n <- nchar(label)
  ws <- 76L - n
  if (ws %% 2L > 0L) {
    ws <- c(ceiling(ws / 2L), floor(ws / 2L))
  } else {
    ws <- c(ws / 2L, ws / 2L)
  }
  f <- function(n) paste(rep(" ", n), collapse = "")
  ws <- unlist(Map("f", ws))
  label <- paste0("##", ws[1], label, ws[2], "##")
  txt <- paste(
    "##----------------------------------------------------------------------------##",
    label, sep = "\n",
    "##----------------------------------------------------------------------------##"
  )
  con <- pipe("pbcopy", "w")
  cat(txt, file = con)
  close(con)
}

#' @export
#' @noRd
codebox <- function(label = "") boxcode(label)
#' @export
#' @noRd
box_code <- function(label = "") boxcode(label)
#' @export
#' @noRd
code_box <- function(label = "") boxcode(label)

