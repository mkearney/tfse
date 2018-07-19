#' @title tfse: The First Scriptorium Emporium
#'
#' @description Collection of useful functions.
#'
#' @author Michael W. Kearney
#' @docType package
#' @name tfse
#' @aliases tfse
#' @keywords package tfse-package
#' @seealso \code{rtweet}
NULL

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("rdrop2", quietly = TRUE)) {
    shhh(tryCatch(db_load_token(), error = function(e) return(NULL)))
  }
}
