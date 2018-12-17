paste_lines <- function(...) {
  paste0(unlist(list(...)), collapse = "\n")
}

#' Write function to file
#'
#' Write the function code to a file and open the file
#'
#' @param f Function
#' @return Writes to temporary file and opens that file.
#' @export
write_function <- function(f) {
  ## name of function
  fun_name <- paste0(
    as.character(deparse(substitute(f))),
    " <- "
  )
  fun_name <- sub(".*\\:\\:", "", fun_name)

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

  ## save to temporary .R file
  tmp <- tempfile(fileext = ".R")

  ## style code
  x <- as.character(styler::style_text(x))
  x <- x[!(x == "" & c(FALSE, x[-length(x)] == ""))]
  x <- paste_lines(x)
  x <- sub("\\{\n", "{\n\n", x)

  ## write/save file
  cat(
    x,
    file = tmp,
    fill = TRUE
  )

  ## style via tidyverse code style
  #styler::style_file(tmp)

  ## open file for editing
  file_edit(tmp)
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
  file.edit(file, title = file, editor = "rstudio", fileEncoding = "UTF-8")
  invisible(file)
}
