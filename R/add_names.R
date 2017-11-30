#' Adds names to data object
#'
#' Adds names to data object wrapper of names<-
#'
#' @param x Data
#' @param names Names.
#' @return (Re)named data.
#' @export
#' @examples
#' add_names(mtcars, paste0("mtcars", seq_len(ncol(mtcars))))
add_names <- function(x, names) `names<-`(x, names)
