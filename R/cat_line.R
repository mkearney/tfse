

#' Combine and print text
#'
#' \code{cat_lines}: concatenate/print \strong{multiple} line
#'
#' @param ... Text to print and/or named arguments such as sep, collapse, fill,
#'   indent, postdent.
#' @return Prints text and returns NULL similar to the base cat function
#' @export
cat_lines <- function(...) UseMethod("cat_lines")

#' @export
cat_lines.default <- function(...) {
  args <- as.list(c(
    dots_unnamed(...),
    add_arg_if(
      dots_named(...),
      sep = "",
      collapse = "\n",
      fill = TRUE,
      indent = "",
      postdent = "")
  ))
  do.call(catlines_, args)
}

#' Combine and print text
#'
#' \code{cat_line}: concatenate/print \strong{single} line
#'
#' @rdname cat_lines
#' @inheritParams cat_lines
#' @export
cat_line <- function(...) UseMethod("cat_line")

#' @export
cat_line.default <- function(...) {
  args <- as.list(c(
    dots_unnamed(...),
    add_arg_if(
      dots_named(...),
      sep = "",
      collapse = "",
      fill = TRUE,
      indent = "",
      postdent = "")
  ))
  do.call(catlines_, args)
}

dots_named <- function(...) {
  dots <- as.list(list(...))
  if (is.null(names(dots))) {
    return(list())
  }
  dots[names(dots) != ""]
}
dots_unnamed <- function(...) {
  dots <- as.list(list(...))
  if (!is.null(names(dots))) {
    dots <- dots[names(dots) == ""]
  }
  lapply(dots, unlist)
}

catline_ <- function(..., sep, collapse, fill, indent, postdent) {
  x <- paste(dots_unnamed(...), sep = sep)
  x <- paste0(indent, x, postdent)
  cat(paste(x, collapse = collapse), fill = fill)
}

catlines_ <- function(..., sep, collapse, fill, indent, postdent) {
  x <- lapply(dots_unnamed(...), function(.x)
    paste0(indent, .x, postdent, collapse = sep))
  cat(paste(x, collapse = collapse), fill = fill)
}
