#' funs
#'
#' Pastes fun skeletons into clipboard
#'
#' @param names Names of functions.
#' @return Skeleton function code copied to clipboard.
#' @export
pfuns <- function(names) {
  fun <- function(name = "foo") {
    paste0(
    "\n#\' ", name,
    "\n#\' ",
    "\n#\' Verbs object",
    "\n#\' ",
    "\n#\' @param x Input",
    "\n#\' @return Output\n",
    name,
    " <- function(x) {\n  x\n}\n"
    )
  }
  txt <- vapply(names, fun, character(1))
  con <- pipe("pbcopy", "w")
  cat(txt, file = con)
  close(con)
}
