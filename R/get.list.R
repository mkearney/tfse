#' get_list
#'
#' @param slug Name of Twitter list
#' @param owner Screen name of list owner
#' @param token An OAuth token (1.0 or 2.0)
#' @param count Maximum number of users to return (cannot be higher than 5000).
#' @param cursor Select next or previous page or results using cursor value from
#'   returned json object
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return json response object as nested list
#' @import httr
#' @import jsonlite
#' @export
get_list <- function(slug, owner, token, count = 5000, cursor = "-1") {
  out <- TWIT(query = "lists/members",
              parameters = paste0("slug=", slug,
                                  "&owner_screen_name=", owner,
                                  "&count=", count,
                                  "&cursor=", cursor),
              token = token)
  return(out)
}
