
#' search_files
#'
#' Returns matching files and line numbers of given string pattern.
#'
#' @param x Pattern.
#' @param path Path on which to restrict search. Defaults to current working
#'   directory.
#' @param recursive logical
#' @param invert logical
#' @param file_names only show matching file names
#' @param all.files default false excludes dot files
#' @return Output from terminal - file name, line number, and preview of
#'   matching text
#' @export
search_files <- function(x, path = ".", recursive = TRUE, invert = FALSE,
                         file_names = FALSE, all.files = FALSE) {
  ignore.case <- TRUE
  args <- "-n"
  if (recursive) {
    args <- paste0(args, "r")
  }
  if (invert) {
    args <- paste0(args, "v")
  }
  if (file_names) {
    args <- paste0(args, "l")
  }
  if (!all.files) {
    dot_dirs <- grep("/\\.[^\\.]", list.dirs(recursive = recursive), value = TRUE)
    dot_dirs <- unique(regmatches(dot_dirs, regexpr("(?<=/)\\.[^/]+", dot_dirs, perl = TRUE)))
    dot_dirs <- paste0("{", paste(dot_dirs, collapse = ","), "}")
    args <- paste0(args, " --exclude-dir=", dot_dirs)
  }
  #cmd <- paste0("grep ", args, " ", shQuote(x), " ", path, " -s")
  #o <- tryCatch(system(cmd, intern = intern), error = function(e) "search_files-error")
  #if (identical(o, "search_files-error")) {
  f <- list.files(path, all.files = all.files, full.names = TRUE, recursive = recursive)
  i <- file.info(f)
  i <- i[i$size < 25000, ]
  f <- row.names(i)
  f <- grep("\\.png$|\\.jpeg$|\\.jpg$|\\.rda$|\\.rds$|\\.tiff$|\\.pdf", f,
    invert = TRUE, value = TRUE)
  s <- lapply(f, extra_text_search, pat = "readlines", ignore.case = ignore.case)
  names(s) <- f
  s <- s[lengths(s) > 0]
  nms <- names(s)
  s <- lapply(s, paste, collapse = "\n")
  for (i in seq_along(s)) {
    cat("\n# File:", nms[i], fill = TRUE)
    txt <- paste0("  ", s[[i]])
    txt <- gsub("\n", "\n  ", txt)
    cat(txt, fill = TRUE)
  }
}


extra_text_search <- function(file, pat, ignore.case = TRUE) {
  x <- tryCatch(readlines(file), error = function(e) NULL, warning = function(w) NULL)
  if (is.null(x)) return(invisible())
  term <- pat
  pat <- paste0("\\b.{0,32}", pat, ".{0,32}\\b")
  if (!any(grepl(pat, x, ignore.case = ignore.case))) {
    return(invisible())
  }
  m <- regmatches_(x, pat, drop = TRUE, ignore.case = ignore.case)
  m <- paste0(#"# ", seq_along(m), ". ",
    m, collapse = "\n")
  m <- gsub("( \\{\\{\\{(?!\\{))|((?<!\\})\\}\\}\\} )", "", m, perl = TRUE)
  if (rstudioapi::isAvailable()) {
    m <- gsub(term, paste0("\033[39m", term, "\033[38;5;242m"), m, ignore.case = ignore.case)
    m <- paste0("\033[38;5;242m", m, "\033[39m")
    m <- gsub(term, paste0("\033[31m", term, "\033[39m"), m)
  }
  m
}

