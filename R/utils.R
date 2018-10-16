
is_url <- function(x) {
  is.character(x) && length(x) == 1 && grepl("^http", x)
}

is_recursive <- function(x) vapply(x, is.recursive, logical(1))

