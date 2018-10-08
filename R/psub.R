#' Paste sub
#'
#' Glue-like sub pasting of strings
#'
#' @param x Input string
#' @param ... Named strings with names being the values to replace and the strings
#'   being the desired new value.
#' @export
psub <- function(x, ...) {
  dots <- list(...)
  if (is.null(names(dots))) {
    nms <- rep("", length(dots))
  } else {
    nms <- names(dots)
  }
  m <- gregexpr("\\{[^\\}\\{]+\\}", x)
  p <- regmatches(x, m)[[1]]
  p <- grep("\\s", p, invert = TRUE, value = TRUE)
  p <- gsub("^\\{|\\}$", "", p)
  if (!"" %in% nms) {
    nms <- nms[match(p, nms)]
    if (length(nms) != length(p)) {
      stop("tfse::psub - number of {params} doesn't match number of replacement values", call. = FALSE)
    }
  } else if (length(nms) != length(p)) {
    stop("tfse::psub - number of {params} doesn't match number of replacement values", call. = FALSE)
  } else {
    nms[nms == ""] <- p[nms == ""]
  }
  dots <- unlist(dots)
  dots <- dots[match(p, names(dots))]
  p <- paste0("{", p, "}")
  p <- sub(".{1}$", "\\\\}", sub("^.{1}", "\\\\{", p))
  nms <- paste0("{", nms, "}")
  for (i in seq_along(nms)) {
    x <- sub(p[i], dots[i], x)
  }
  x
}

