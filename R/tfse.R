#' @title tfse: Twitter Follows and Selective Exposure
#'
#' @description An implementation of http calls and data wrangling
#' functions designed to interact with Twitter API's. GET and
#' POST requests are designed to return data from Twitter's
#' search and streaming API's. Returned [json] objects are
#' automatically parsed into data_frames or lists. Specific
#' consideration is given to functions to use of more
#' than one OAuth personal access token.
#'
#' @author Michael W Kearney
#' @docType package
#' @name tfse
#' @aliases tfse
#' @keywords package tfse-package
#' @examples
#' \dontrun{get.followers("HillaryClinton")}
#' @seealso \code{httr}
NULL

.state <- new.env(parent = emptyenv())
