#' get_var
#'
#' Looks for and returns element of given name
#'
#'
#' @param x Data object. Particularly useful for recursive lists.
#' @param ... Names at each successive level from which to dive and return.
#' @return Returns the last named element found in the data object.
#' @examples
#' ## generate nested list
#' lst <- list(
#'   I = 1,
#'   II = list(
#'     A = 1,
#'     B = list(
#'       a = 1,
#'       b = list(
#'         v1 = 1,
#'         v2 = 2
#'       )
#'     )
#'   )
#' )
#' lst <- replicate(2, list(lst))
#'
#' ## fetch and return all v2's
#' get_var(lst, "II", "B", "v2")
#' @export
get_var <- function(x, ...) {
  vars <- c(...)
  success <- FALSE
  for (i in vars) {
    if (!is.recursive(x)) break
    if (has_name_(x, i)) {
      x <- x[[i]]
      if (i == vars[length(vars)]) {
        success <- TRUE
      }
    } else if (any_recursive(x) && any(sapply(x, has_name_, i))) {
      kp <- sapply(x, has_name_, i)
      x <- x[kp]
      x <- lapply(x, "[[", i)
      if (i == vars[length(vars)]) {
        success <- TRUE
      }
    }
  }
  if (!success) return(NULL)
  if (length(x) == 0L) return(NA)
  if (any_recursive(x)) {
    return(x)
  }
  unlist(x)
}

#' n_uq
#'
#' Number of distinct elements.
#'
#' @param x Vector.
#' @return Integer number of distinct elements.
#' @examples
#' n_uq(sample(1:10, 5, replace = TRUE))
#' @export
n_uq <- function(x) {
  length(unique(x))
}

#' any_recursive
#'
#' Tests whether any element is recursive.
#'
#'
#' @param x Object to be tested
#' @return Logical
#' @examples
#' ## list containing data frame
#' lst <- list(
#'   a = c(1, 2, 3),
#'   b = "asdf",
#'   c = mtcars
#' )
#' any_recursive(lst)
#'
#' ## list with non-recursive columns
#' lst <- list(
#'   a = c(1, 2, 3),
#'   b = "asdf",
#'   c = c(TRUE, FALSE, TRUE, FALSE)
#' )
#' any_recursive(lst)
#'
#' ## data frame containing data frame
#' dat <- data.frame(
#'   a = c(1, 2, 3),
#'   b = c("a", "b", "c"),
#'   c = I(list(mtcars, NA, "blah"))
#' )
#' any_recursive(dat)
#'
#' ## data frame with non-recursive columns
#' dat <- data.frame(
#'   a = c(1, 2, 3),
#'   b = c("a", "b", "c"),
#'   c = c(TRUE, FALSE, TRUE)
#' )
#' any_recursive(dat)
#' @export
any_recursive <- function(x) {
  if (!is.recursive(x)) {
    return(FALSE)
  }
  any(vapply(x, is.recursive, logical(1)))
}

## hase name(s) accepts one or more names (looks for all == TRUE)


#' has_name_
#'
#' Tests whether object contains name(s)
#'
#'
#' @param x Data object. This function is not recursive, so must be applied to
#'   object at intended level of inspection.
#' @param ... Names to look for in data object.
#' @details If multiple names are provided, the function will only return TRUE if
#'   all names are present.
#' @return Logical reflecting whether data contains provided name(s)
#' @examples
#' ## data set
#' dat <- data.frame(a = 1, b = 2, c = 3)
#'
#' ## does dat contain named element "a"
#' has_name_(dat, "a")
#'
#' ## what about "q"?
#' has_name_(dat, "q")
#'
#' ## what about both "a" and "b"?
#' has_name_(dat, "a", "b")
#'
#' #' ## what about "a", "b", and "q"?
#' has_name_(dat, "a", "b", "q")
#' @export
has_name_ <- function(x, ...) {
  vars <- c(...)
  stopifnot(is.character(vars))
  if (!is.recursive(x)) {
    return(FALSE)
  }
  all(vars %in% names(x))
}

#' check_renv
#'
#' Checks whether one can safely append to .Renvir file.
#'
#'
#' @param path Path/to/.Renvir
#' @usage
#' check_renv(path)
#' @export
check_renv <- function(path) {
  if (!file.exists(path)) {
    return(invisible())
  }
  con <- file(path)
  x <- readLines(con, warn = FALSE)
  close(con)
  x <- clean_renv(x)
  x <- paste(x, collapse = "\n")
  cat(x, file = path, fill = TRUE)
  invisible()
}

clean_renv <- function(x) {
  stopifnot(is.character(x))
  ## remove incomplete vars
  x <- grep("=$", x, value = TRUE, invert = TRUE)
  ## split lines with double entries and fix into new vars
  xs <- strsplit(x, "=")
  vals <- sub("[^=]*=", "", x)
  kp <- !grepl("[[:upper:]]{1,}=", vals)
  if (sum(!kp) > 0L) {
    m <- regexpr("[[:upper:]_]{1,}(?==)", x[!kp], perl = TRUE)
    newlines <- paste0(regmatches(x[!kp], m), "=", sub(".*=", "", x[!kp]))
    x <- x[kp]
    x[(length(x) + 1):(length(x) + length(newlines))] <- newlines
  }
  ## remove double entries
  xs <- strsplit(x, "=")
  kp <- !duplicated(sapply(xs, "[[", 1))
  x <- x[kp]
  x
}

#' menuline
#'
#' Creates interactive multiple choice question.
#'
#'
#' @param q Question to be asked in interactive session.
#' @param a Answer choices.
#' @return Selection provided by user in interactive session.
#' @export
menuline <- function(q, a) {
  message(q)
  menu(a)
}


#' readline_
#'
#' Worry free way to read lines from interactive sessions.
#'
#' @param ... Character string or vector to be used as prompt during
#'   interactive R session. Ultimately, this function only sends a
#'   single string to the user, but it will accept a vector if you're
#'   picky about not creating strings of a certain width.
#' @return Input entered during interactive session without extra quotes.
#' @export
readline_ <- function(...) {
  input <- readline(paste(unlist(c(...)), collapse = ""))
  gsub("^\"|^'|\"$|'$", "", input)
}


#' trim_ws
#'
#' Returns character vector without extra spaces and trimmed of white space.
#'
#' @param x Character vector
#' @return Character vector without extra spaces
#' @export
trim_ws <- function(x) {
  x <- gsub("\\s{2,}", " ", x)
  gsub("^\\s|\\s$", "", x)
}

#' stopwords
#'
#' Vector of stopwords
#' @docType data
NULL

stopwords <- tidytext::stop_words$word[tidytext::stop_words$lexicon == "SMART"]


#' rm_links
#'
#' Removes URL links included in tweets.
#'
#' @param x Character vector of Twitter statuses.
#' @return Character vector of statuses without URLs.
#' @export
rm_links <- function(x) {
  x <- gsub("\\s{0,1}http\\S{1,}\\s{0,1}", "", x)
  gsub("\\s{0,1}\\S{1,}\\.com\\b\\s{0,1}", "", x)
}

#' rm_stopwords
#'
#' Returns statuses with stop words removed
#'
#' @param x Vector of text.
#' @param stopwords Optional, stop words to be removed from text. Defaults to
#'   SMART stop words provided by tidytext package.
#' @return Character vector with stopwords removed
#' @export
rm_stopwords <- function(x, stopwords = stopwords) {
  wordbreakor <- function(x) {
    x <- paste(x, collapse = "\\s{0,1}\\b|\\b\\s{0,1}")
    paste0("\\b", x, "\\b")
  }
  if (is.null(stopwords)) {
    stopwords <- c(
      tidytext::stop_words$word[tidytext::stop_words$lexicon == "SMART"],
      0:9
    )
  }
  stopwords <- wordbreakor(stopwords)
  x <- gsub(stopwords, " ", x, perl = TRUE, ignore.case = TRUE)
  trim_ws(x)
}


#' tabsort
#'
#' Returns a sorted (descending) frequence tbl
#'
#' @param x Character vector
#' @param V1 Optional, name of term variable. Defaults to "term".
#' @return Frequency tbl
#' @export
tabsort <- function(x, V1 = NULL) {
  x <- sort(table(x), decreasing = TRUE)
  x <- tibble::data_frame(term = names(x), n = as.integer(x))
  if (!is.null(V1)) {
    names(x)[1] <- V1
  }
  x
}

#' stripped_down_words
#'
#' Strip (clean) and tokenize text
#'
#' @param x CHaracter vector of text to clean.
#' @return Split into vectors of words for each character string.
#' @examples
#' x <- "Some sentencE with AWK. formatting
#' and \t a URL link to https://google.com   , maybe?"
#' stripped_down_words(x)
#' @export
stripped_down_words <- function(x) {
  x <- gsub("\\n|\\t", " ", x)
  x <- rm_links(x)
  x <- tolower(x)
  x <- iconv(x, "utf-8", "ascii", "")
  x <- trim_ws(x)
  strsplit(x, " ")
}
