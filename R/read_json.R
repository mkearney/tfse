
#' read_json
#'
#' @param x Character vector with json strings
#' @return Parsed output from jsonlite fromJSON.
#' @export
read_json <- function(x) {
  read_json_ <- function(x) {
    x <- gsub("\\t|\\s{2,}", " ", x)
    x <- trimws(x)
    x <- sub(".*(?={\")", "", x, perl = TRUE)
    x <- sub("(?<=\"}).*$", "", x, perl = TRUE)
    tryCatch(jsonlite::fromJSON(x),
             error = function(e) return(NULL))
  }
  x <- lapply(x, read_json_)
  x[lengths(x) > 0L]
}
