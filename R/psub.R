#' Paste sub
#'
#' Glue-like sub paster
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
  s <- unlist(dots)
  nms <- paste0("{", nms, "}")
  m <- gregexpr("\\{[^\\}]+}", x)
  p <- regmatches(x, m)[[1]]
  if ("{}" %in% nms) {
    nms[nms == "{}"] <- p[!p %in% nms]
  }
  c("c", "a", "b")[match(c("a", "b", "c"), c("c", "a", "b"))]
  s <- s[match(p, nms)]
  p <- sub(".{1}$", "\\\\}", sub("^.{1}", "\\\\{", p))
  for (i in seq_along(s)) {
    x <- gsub(p[i], s[i], x)
  }
  x
}
