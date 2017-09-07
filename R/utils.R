#' sn2id
#'
#' @param screen_name Twitter handle
#' @seealso See \url{https://dev.twitter.com/
#' overview/documentation} for more information on using
#' Twitter's API.
#' @return response Twitter account user id
#' @import rvest
#' @export
sn2id <- function(screen_name) {
  sn2id_ <- function(screen_name) {
    if (!requireNamespace("rvest", quietly = TRUE)) {
      stop("Rvest needed for this function to work.
           Please install it.",
           call. = FALSE)
    }
    user_id <- xml2::read_html(
      paste0("http://twitter.com/", screen_name)
    )
    html_attr("data-user-id", html_nodes(user_id, ".ProfileNav"))
  }
  out <- parallel::mclapply(screen_name, sn2id_, mc.cores = mc.cores)
  out[lengths(out) == 0L] <- NA_character_
  out <- unlist(out)
  names(out) <- screen_name
  out
}

#' id2sn
#'
#' Converts user ids to screen names.
#'
#' @param user_id Vector of user_id(s)
#' 
#' @return Named vector of screen names.
#' @importFrom httr GET
#' @importFrom rvest html_text html_nodes
#' @importFrom xml2 read_html
#' @export
id2sn <- function(user_id, mc.cores = 1L) {
  id2sn_ <- function(x) {
    h <- httr::GET(paste0("https://twitter.com/intent/user?user_id=", x))
    h <- httr::content(h)
    x <- gsub("@", "", html_text(html_nodes(h, "p span.nickname")))
    if (length(x) == 0L) return(NA_character_)
    if (length(x) > 1L) return(x[1])
    x
  }
  out <- parallel::mclapply(user_id, id2sn_, mc.cores = mc.cores)
  out[lengths(out) == 0L] <- NA_character_
  out <- unlist(out)
  names(out) <- user_id
  out
}

#' @importFrom grDevices rgb
alphacolor <- function(cols, a = .99) {
    cols <- t(col2rgb(cols, alpha = TRUE)) / 255
    rgb(cols, alpha = a)
}

#' @importFrom grDevices col2rgb
is.color <- function(x) {
    if (all(grepl("^#", x))) return(TRUE)
    x <- tryCatch(col2rgb(x),
        error = function(e) return(NULL))
    if (!is.null(x)) return(TRUE)
    FALSE
}

