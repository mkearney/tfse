TWIT <- function(query, parameters, token, version = "1.1") {
  if (query == "lists/members") {
    req <- POST(paste0("https://api.twitter.com/",
                       version, "/",
                       query,
                       ".json?",
                       parameters),
                config(token = token))
  }
  if (is.null(parameters)) {
    req <- GET(paste0("https://api.twitter.com/",
                      version, "/",
                      query,
                      ".json"),
               config(token = token))
  } else {
    req <- GET(paste0("https://api.twitter.com/",
                      version, "/",
                      query,
                      ".json?",
                      parameters),
               config(token = token))
  }
  if (http_error(req)) {
    return(NULL)
  }
  out <- fromJSON(content(req, as = "text"))
  return(out)
}
