
#' scrape URL links from a webpage
#'
#' Scrapes URL links from HTML source code
#'
#' @param rurl URL to desired page. Should include scheme (http/https)
#' @param qs Logical indicating whether to return query strings (part of URL starting with ?)
#' @param pat Pattern from which only matching results should be returned. Defaults to ".",
#'   which matches everything.
#' @return Character vector of URLs.
#' @export
scrape_links <- function(rurl, qs = TRUE, pat = ".") {
  r <- httr::GET(rurl)
  scheme <- ifelse(grepl("^https", rurl), "https", "http")
  base_url <- paste0(scheme, "://", dirname(gsub("^https?://", "", rurl)))
  if (grepl("https?://.", base_url)) base_url <- paste0(scheme, "://", gsub("^https?://", "", rurl))
  links <- httr::content(r) %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    ifelse(grepl("^//", .), paste0(scheme, ":", .), .) %>%
    ifelse(grepl("^/", .), paste0(base_url, .), .) %>%
    sub("#.*", "", .)
  if (identical(scheme, "https")) {
    links <- sub("http:", "https:", links)
  }
  if (!qs) {
    links <- sub("\\?.*", "", links)
  }
  links %>%
    unique() %>%
    sort() %>%
    grep(pat, ., value = TRUE)
}

