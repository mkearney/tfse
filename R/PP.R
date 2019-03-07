
#' Paste with parameters
#'
#' Paste0 strings and sub brace-indicated params
#'
#' @param lhs String
#' @param rhs String
#' @return String
#' @export
`%PP%` <- function(lhs, rhs) {
  lp <- unique(regmatches_(
    lhs, "(?<=\\{)[[:alnum:]]+(?=\\})", drop = TRUE))
  if (length(lp) > 0) {
    lv <- dapr::lap(lp, get, envir = parent.frame())
    lp <- paste0("\\{", lp, "\\}")
    for (i in seq_along(lp)) {
      lhs <- gsub(lp[i], lv[[i]], lhs)
    }
  }
  rp <- unique(regmatches_(
    rhs, "(?<=\\{)[[:alnum:]]+(?=\\})", drop = TRUE))
  if (length(rp) > 0) {
    rv <- dapr::lap(rp, get, envir = parent.frame())
    rp <- paste0("\\{", rp, "\\}")
    for (i in seq_along(rp)) {
      rhs <- gsub(rp[i], rv[[i]], rhs)
    }
  }
  paste0(lhs, rhs)
}
