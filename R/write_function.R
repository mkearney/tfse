#' Paste collapse
#'
#' Paste with sep and collapse set to empty.
#'
#' @param ... One or more character strings to paste together with paste0 and
#'   collapse equal to ""
#' @return A single string collapsed and separated with empty spaces
#' @export
paste_collapse <- function(...) {
  paste0(c(...), collapse = "")
}

paste_lines <- function(...) {
  paste0(unlist(list(...)), collapse = "\n")
}

capture_dots <- function(...) {
  eval(substitute(alist(...)), envir = parent.frame())
}

expr_names <- function(args) {
  vapply(
    args,
    deparse,
    USE.NAMES = FALSE,
    FUN.VALUE = character(1)
  )
}

pretty_dots <- function(...) {
  ## capture dots as arg list
  dots <- capture_dots(...)

  ## if none provided, return NULL
  if (length(dots) == 0) {
    return(NULL)
  }

  ## if no names, inherit expression text
  if (is.null(names(dots))) {
    names(dots) <- expr_names(dots)
  }

  ## dots names
  nms <- names(dots)

  ## if any names missing, assign expression text
  if ("" %in% nms) {
    names(dots)[nms == ""] <- expr_names(dots[nms == ""])
  }

  ## return dots
  lapply(dots, eval, envir = parent.frame())
}


#' Write function to file
#'
#' Write the function code to a file and open the file
#'
#' @param ... Functions to write to file. It's best to name these.
#' @return Writes to temporary file and opens that file.
#' @export
write_function <- function(...) {
  ## capture
  f <- pretty_dots(...)

  ## text of functions
  x <- mmap(text_of_function, f, names(f))

  ## collapse into single text string
  x <- paste_lines(paste0(x, "\n"))

  ## save to temporary .R file
  tmp <- tempfile(fileext = ".R")

  ## write/save file
  cat(
    x,
    file = tmp,
    fill = TRUE
  )

  ## open file for editing
  if (interactive()) {
    file_edit(tmp)
  } else {
    tmp
  }
}


#' Copy function to clipboard
#'
#' Copy the function code to clipboard (ready to paste)
#'
#' @param ... Functions to write to clipboard. It's best to name these.
#' @return Writes to clipboard (ready to paste)
#' @export
copy_function <- function(...) {
  ## capture
  f <- pretty_dots(...)

  ## text of functions
  x <- mmap(text_of_function, f, names(f))

  ## collapse into single text string
  x <- paste_lines(paste0(x, "\n"))

  ## save to temporary .R file
  pbcopy(x)
}

mmap <- function(f, ...) {
  f <- match.fun(f)
  mapply(FUN = f, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

text_of_function <- function(f, name) {
  ## name of function
  fun_name <- paste0(
    sub(".*\\:\\:", "", name),
    " <- "
  )

  ## function body text
  x <- deparse(f)

  ## reformat first line of function
  x[1] <- sub("function ", "function", x[1])
  open_bracket <- grep("^\\{$", x)
  if (length(open_bracket) > 0) {
    x[seq_len(open_bracket)] <- gsub(
      "\\,\\s",
      ",\n",
      x[seq_len(open_bracket)],
      perl = TRUE
    )
    x[seq_len(open_bracket)] <- sub("\n$", "", x[seq_len(open_bracket)])
    x[open_bracket - 1] <- paste0(x[open_bracket - 1], " {")
    x[open_bracket] <- ""
  }

  ## add some space before if {lines}
  x <- trim_ws(x)
  x <- gsub("^if ", "\nif ", x)

  ## collapse into single character
  x <- paste_lines(x)

  ## fix line spacing
  x <- gsub("\\}\\s+else", "} else", x)
  x <- gsub("\\}\n", "}\n\n", x)

  ## combine name and body
  x <- paste0(fun_name, x)

  ## style code
  x <- x[!(x == "" & c(FALSE, x[-length(x)] == ""))]
  x <- paste_lines(x)
  sub("\\{\n", "{\n\n", x)
}


#' Open file in text editor
#'
#' Opens file in current text editor
#'
#' @param file Name of file(s) to open.
#' @details Should open using system default or current text editor.
#' @return Opens file and returns invisible file name.
#' @export
file_edit <- function(file) {
  utils::file.edit(file, title = file, fileEncoding = "UTF-8")
  invisible(file)
}
