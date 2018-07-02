
#' read as xml
#'
#' @param x input
#' @return xml_document
#' @export
read_as_xml <- function(x) {
  ## if connection
  if (inherits(x, "connection") && inherits(x, "file")) {
    txt <- readLines(x, warn = FALSE)
    close(x)
    x <- txt
    rm(txt)
  } else if (inherits(x, "connection")) {
    x <- tryCatch(xml2::read_html(x),
      error = function(e) return(NULL))
  }
  ## read lines if file
  if (is.character(x) && length(x) == 1 && file.exists(x)) {
    if (grepl("\\.webarchive$|\\.html?$", x)) {
      x <- tryCatch(xml2::read_html(x),
        error = function(e) return(NULL))
        ## if null, report error on read_html
      if (is.null(x)) {
        warning("`read_html()` could not convert `x` into an xml_document.")
        return(NULL)
      }
    } else {
      x <- readr::read_lines(x, progress = FALSE)
    }
  }

  ## if null/empty return null
  if (length(x) == 0) return(NULL)

  ## convert multiple lines into single string
  if (is.character(x) && length(x) > 1) {
    x <- paste(x, collapse = "\n")
  }

  ## read if URL and convert to xml_document
  if (is.character(x) && grepl("^http", x)) {
    x <- tryCatch(xml2::read_html(url(x)),
      error = function(e) return(NULL))
  } else if (is.character(x)) {
    x <- tryCatch(xml2::read_html(x),
      error = function(e) return(NULL))
  }

  ## if null, report error on read_html
  if (is.null(x)) {
    warning("`read_html()` could not convert `x` into an xml_document.")
    return(NULL)
  }

  ## validate data
  if (!inherits(x, "xml_document")) {
    warning("Failed to convert `x` into xml_document")
    return(NULL)
  }

  ## return xml_document
  x
}

#' grab json
#'
#' @param x input
#' @return if json found then parsed list
#' @examples
#' \dontrun{
#' ## scrape espn
#' e <- grab_json("http:/www.espn.com/")
#' str(e, 3)
#'
#' ## convert html text
#' grab_json('<html>{\"mpg\":21,\"cyl\":6,\"disp\":160,\"lgl\":false}</html>')
#' }
#' @export
grab_json <- function(x) {
  ## if json
  if (grepl("\\[?\\{", x)) {
    j <- tryCatch(jsonlite::fromJSON(x), error = function(e) return(NULL))
    if (!is.null(j)) return(j)
  }

  ## read as xml
  x <- tryCatch(read_as_xml(x), error = function(e) return(x))

  ## convert to char
  x <- as.character(x)

  ## if null/empty return null
  if (length(x) == 0) return(NULL)

  ## convert multiple lines into single string
  if (is.character(x) && length(x) > 1) {
    x <- paste(x, collapse = "\n")
  }

  ## validate x
  stopifnot(is.character(x))

  ## initialize output vector
  o <- list()

  ## swing for fences
  o[[length(o) + 1L]] <- safely_parse_json(x, "\\[\\{\".*\\}\\](?!\\,)")
  o[[length(o) + 1L]] <- safely_parse_json(x, "(?<!\\[)\\{\".*\\}(?!\\,|\\]|\\})")
  o[[length(o) + 1L]] <- safely_parse_json(x, "\\{\".*\\}(?=;</script>)")
  o[[length(o) + 1L]] <- safely_parse_json(x, "\\{\".*\\}(?=(;|\\s))")
  o[[length(o) + 1L]] <- safely_parse_json(x, "\\{\".*\\}(?!\\,)")

  ## return o
  unique(o)
}


safely_parse_json <- function(x, pat) {
  if (grepl("\\(\\?", pat)) {
    perl <- TRUE
  } else {
    perl <- FALSE
  }
  m <- gregexpr(pat, x, perl = perl)
  if (!identical(m[[1]], -1)) {
    x <- regmatches(x, m)[[1]]
  }
  safely_fromJSON(x)
}

safely_fromJSON <- function(x) {
  if (length(x) == 0) return(NULL)
  fj <- function(x) {
    o <- tryCatch(jsonlite::fromJSON(x), error = function(e) return(NULL))
    if (is.null(o)) {
      o <- tryCatch(jsonlite::fromJSON(paste0("[", x, "]")), error = function(e) return(NULL))
    }
    o
  }
  x <- purrr::map(x, fj)
  if (is.null(x) || all(lengths(x) == 0)) return(NULL)
  x <- x[lengths(x) > 0]
  names(x) <- paste0("j", seq_along(x))
  if (length(x) == 1 && is.atomic(x[[1]]) && length(x[[1]]) == 1L) return(NULL)
  if (length(x) == 1 && is.data.frame(x[[1]])) x <- x[[1]]
  if (length(x) == 1 && is.list(x[[1]]) && length(unique(lengths(x[[1]]))) == 1) {
    x <- as_tbl(x[[1]])
  }
  x
}
