
#' Returns integer values. Used for get_friends function.
#' @keywords internal
.which_ids <- function(n, max_users, token = NULL) {
  if (!is.null(token)) {
    total <- rate_limit(token, "friends/ids")
    if (total == 0) {
      return(invisible())
    }
  } else {
    total <- 15
  }
  remain <- total - 1

  n <- n * (remain + 1) - remain
  end <- n + remain

  if (!missing(max_users)) {
    if (end > max_users) {
      end <- max_users
    }
  }

  n:end
}
