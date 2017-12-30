#' sn2id
#'
#' @param screen_name Twitter handle
#' @param mc.cores Number of cores. If greater than 1, then mcMap is used.
#' @return response Twitter account user id
#'
#' @export
sn2id <- function(screen_name, mc.cores = 1L) {
  if (mc.cores > 1L) {
    if (!requireNamespace("parallel", quietly = FALSE)) {
      stop("must install parallel pkg", call. = FALSE)
    }
    out <- parallel::mcMap("sn2id_", screen_name, mc.cores = mc.cores)
  } else {
    out <- Map("sn2id_", screen_name)
  }
  out <- unlist(out)
  names(out) <- screen_name
  out
}

sn2id_ <- function(x) {
  if (!requireNamespace("rvest", quietly = FALSE)) {
    stop("must install rvest pkg", call. = FALSE)
  }
  x <- xml2::read_html(
    paste0("http://twitter.com/", x)
  )
  x <- rvest::html_attr(rvest::html_nodes(
    x, ".ProfileNav"), "data-user-id")
  if (length(x) == 0L) {
    return(NA_character_)
  } else if (length(x) > 1L) {
    x <- x[1]
  }
  if (length(x) == 0L) {
    return(NA_character_)
  }
  trim_ws(x)
}

#' id2sn
#'
#' Converts user ids to screen names.
#'
#' @param user_id Vector of user_id(s)
#' @param mc.cores Number of cores. If greater than 1, then mcMap is used.
#'
#' @return Named vector of screen names.
#' @export
id2sn <- function(user_id, mc.cores = 1L) {
  if (mc.cores > 1L) {
    if (!requireNamespace("parallel", quietly = FALSE)) {
      stop("must install parallel pkg", call. = FALSE)
    }
    out <- parallel::mcMap("id2sn_", user_id, mc.cores = mc.cores)
  } else {
    out <- Map("id2sn_", user_id)
  }
  out <- unlist(out)
  names(out) <- user_id
  out
}

id2sn_ <- function(x) {
  if (!requireNamespace("rvest", quietly = FALSE)) {
    stop("must install rvest pkg", call. = FALSE)
  }
  h <- xml2::read_html(paste0("https://twitter.com/intent/user?user_id=", x))
  x <- rvest::html_text(rvest::html_nodes(h, "p span.nickname"))
  if (length(x) == 0L) {
    return(NA_character_)
  } else if (length(x) > 1L) {
    x <- x[1]
  }
  if (length(x) == 0L) {
    return(NA_character_)
  }
  x <- gsub("@", "", x)
  trim_ws(x)
}
