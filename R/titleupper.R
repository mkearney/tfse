
#' titleupper
#' 
#' Capitalize characters to title case
#' 
#' @param x Character vector
#' @return Character vector with non-article/preposition (unless at beginning of sentence of following colon) first letters capitalized
#' @examples 
#' titleupper("this is a title about something good: a witty zinger")
#' @export
titleupper <- function(x) sapply(x, titleupper_)

titleupper_ <- function(x) {
  x <- gsub("(?<=\\W)|(?=\\W)", "SPLITR", x, perl = TRUE)
  x <- strsplit(x, "SPLITR")[[1]]
  tocap <- !x %in% keeplower()
  tocap[1] <- TRUE
  if (any(x == ":")) {
    tocap[which(x == ":") + 2L] <- TRUE
  }
  x[tocap] <- toupper_first(x[tocap])
  paste(x, collapse = "")
}


toupper_first <- function(x) {
  toupper_ <- function(x) {
    toupper_word <- function(x) list(toupper(x[[1]]))
    m <- gregexpr("^[a-z]", x)
    regmatches(x, m) <- toupper_word(regmatches(x, m))
    x
  }
  lapply(x, toupper_)
}


keeplower <- function() {
  c("is", "are", "was", "were", "on", "about", "in", "around", "between",
    "with", "for", "to", "from", "the", "and", "a", "an", " ", "of",
    "but", "does", "or", "not", "by", "through", "within", "onto",
    "against", "across", "above", "below", "beyond", "at", "before",
    "after", "beneat", "during", "except", "into", "inside", "toward",
    "regarding", "concerning", "outside", "up", "upon", "without",
    "under", "off", "past", "since", "near", "beside", "besides", "beyond",
    "along", "among", "behind", "throughout", "until", "-", ":", ",", ".",
    "(", ")", "[", "]")
}
