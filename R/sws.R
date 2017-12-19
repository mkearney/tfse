#' stop words by probability
#'
#' Return Twitter specific stopwords by expected probability of a term
#' being unique to a given sample of tweets.
#'
#' @param p Probability of word in any given sample of tweets.
#' @param lang One or more lang abbreviations to subset with.
#' @param drop Logical indicating whether to drop non-word elements of
#'   data frame (the default) or to return the subsetted [and ordered
#'   by desc p] data set
#' @param data Data set of stopwords. Defaults to stopwordslangs from
#'   rtweet package
#' @return Either vector of words or data frame with word, p, lang.
#' @export
swp <- function(p = .99999, lang = "en", drop = TRUE, data = NULL) {
  if (is.null(data)) {
    env <- new.env()
    data("stopwordslangs", package = "rtweet", envir = env)
    data <- get("stopwordslangs", envir = env)
  }
  stopifnot(
    is.data.frame(data),
    all(c("lang", "p") %in% names(data))
  )
  stopifnot(is.atomic(lang), is.numeric(p))
  if (is.null(lang) || lang == "all") {
    lang <- unique(data$lang)
  }
  sws <- data[data$lang %in% lang & data$p > p, ]
  if (drop) {
    sws$word[order(sws$p, decreasing = TRUE)]
  } else {
    sws[order(sws$p, decreasing = TRUE), ]
  }
}
