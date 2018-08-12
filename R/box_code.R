
#' boxcode
#'
#' Load clipboard with code chunk square. Paste to insert square into R script
#'   file at cursor location.
#'
#' @param ... Name of section/block
#' @return Text for code box saved into clipboard. Paste to use at cursor.
#'
#' @export
boxcode <- function(...) {
  txt <- paste0(paste(vapply(c(...), boxcode_, FUN.VALUE = character(1)), collapse = "\n\n"), "\n")
  pbcopy(txt)
}

boxcode_ <- function(label = "") {
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
  paste(
    "##----------------------------------------------------------------------------##",
    label, sep = "\n",
    "##----------------------------------------------------------------------------##"
  )

  #con <- pipe("pbcopy", "w")
  #cat(txt, file = con)
  #close(con)
}

#' @export
#' @inheritParams boxcode
#' @rdname boxcode
codebox <- function(...) boxcode(...)

#' @export
#' @inheritParams boxcode
#' @rdname boxcode
box_code <- function(...) boxcode(...)

#' @export
#' @inheritParams boxcode
#' @rdname boxcode
code_box <- function(...) boxcode(...)

