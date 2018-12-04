
#' APA citation of R package
#'
#' Returns an APA-formatting citation of an R package
#'
#' @param pkg Name of package (quoted string)
#' @return A character vector of the APA citation. If on Mac or PC, then also
#'   it's stored to the system's clipboard.
#' @examples
#'
#' ## cite this pacakge
#' apa_citation("tfse")
#'
#' @export
apa_citation <- function(pkg) {
  r <- utils::citation(pkg)
  r <- unclass(r[[1]])[[1]]
  au <- sub("\\s+[[:punct:]]\\S.*", "", r$author)
  au <- strsplit(au, " ")
  auf <- function(x) {
    auf_ <- function(x) {
      last <- x[length(x)]
      x <- x[-length(x)]
      x <- paste0(substr(x, 1, 1), ".")
      paste0(last, ", ", paste(x, collapse = " "))
    }
    x <- vapply(x, auf_, character(1))
    if (length(x) > 1) {
      x1 <- paste(x[-length(x)], collapse = ", ")
      x <- paste0(x1, ", & ", x[length(x)])
    }
    x
  }
  m <- regexpr("(?<=: [A-Z]{1}).*", r$title, perl = TRUE)
  if (m[[1]] > 0) regmatches(r$title, m) <- tolower(regmatches(r$title, m))
  x <- paste0(
    auf(au), " (", r$year, "). ",
    r$title, " (", r$note, ") [Computer software]. The Comprehensive R Archive Network. ",
    "Available from ", r$url
  )
  x <- paste(trim_ws(x), collapse = " ")
  width <- getOption("width", 80)
  if (width > 80) width <- 80
  x <- strwrap(x, width)
  x[1] <- paste0("\n", x[1])
  print_start("Adding APA citation of {", pkg, "} to clipboard!")
  print_complete("Ready to paste!")
  x <- paste(x, collapse = "\n    ")
  x <- paste0(x, "\n")
  p <- gsub("\n", " ", x)
  p <- gsub("[ ]{2,}", " ", p)
  pbcopy(p)
  cat(x, fill = TRUE)
  invisible(x)
}


