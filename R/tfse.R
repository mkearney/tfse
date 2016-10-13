#' @title tfse: Twitter Follows and Selective Exposure
#'
#' @description Useful user functions and imported functions.
#'
#' @author Michael W Kearney
#' @docType package
#' @name tfse
#' @aliases tfse
#' @keywords package tfse-package
#' @seealso \code{httr}
NULL

.state <- new.env(parent = emptyenv())

.onAttach <- function(libname, pkgname) {
  days <- (as.Date("2017-04-01") - Sys.Date())[[1]]
  tbl_wide()
  packageStartupMessage(paste0("you have ", days,
    " days to complete your dissertation!"))
}



