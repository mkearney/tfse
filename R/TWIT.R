#' TWIT
#'
#' @param query Twitter API request type string. e.g, \code{"friends/ids"} calls
#'   Twitter API to return information about a user's friend network (i.e.,
#'   accounts followed by a user).
#' @param parameters Additional parameters passed along to API call
#' @param token An OAuth token (1.0 or 2.0)
#' @param parse logical indicating whether to parse json response object
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return json response object as nested list
#' @import httr
#' @import jsonlite
#' @export
TWIT <- function(query, parameters = NULL, token, parse = TRUE, version = "1.1", timeout = 120, file_name) {
  # POST and GET requests
  if (query == "lists/members") {
    req <- POST(paste0("https://api.twitter.com/",
                       version, "/",
                       query,
                       ".json?",
                       parameters),
                config = config(token = token))
  } else if (query == "statuses/filter") {

    if (foo_params(parameters)) {
      tryCatch(POST(paste0("https://stream.twitter.com/",
                                   version, "/",
                                   "statuses/filter.json?",
                                   parameters),
                    config = config(token = token),
                    timeout(timeout),
                    write_disk(file_name, overwrite = TRUE)),
               error = function(e) return(invisible()))

      return(invisible())
    } else {
      tryCatch(GET(paste0("https://stream.twitter.com/",
                                  version, "/",
                                  "statuses/filter.json?",
                                  parameters),
                   config = config(token = token),
                   timeout(timeout),
                   write_disk(file_name, overwrite = TRUE)),
               error = function(e) return(invisible()))

      return(invisible())
    }
  } else {
    req <- GET(paste0("https://api.twitter.com/",
                      version, "/",
                      query,
                      ".json?",
                      parameters),
               config = config(token = token))
  }

  if (http_error(req)) {
    return(invisible())
  }
  if(parse) {
    return(fromJS(req))
  }
  req
}

