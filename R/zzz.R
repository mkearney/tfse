
#' not in
#'
#' A more convenient way to say no lhs in rhs
#'
#' @param lhs Left hand side
#' @param rhs Right hand side
#' @return Logical indicating whether lhs are NOT in rhs. For more info on the output
#'   vector.
#' @usage lhs \%>\% rhs
#' @examples
#' ## is "a" in the alphabet?
#' "a" %!in% letters
#'
#' ## vector of "elephant" letters?
#' elephant <- strsplit("elephant", "")[[1]]
#'
#' ## letters a b and c
#' abc <- c("a", "b", "c")
#'
#' ## are  "a", "b", or "c" not in
#' abc %!in% elephant
#'
#' @details Number of returned logical values is equal to the length or number of
#'   columns of LHS (behaves the same as base in function).
#' @export
`%!in%` <- function(lhs, rhs) !lhs %in% rhs


#' go_get_var
#'
#' Looks for and returns element of given name
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
go_get_var <- function(x, ...) {
  vars <- c(...)
  success <- FALSE
  for (i in vars) {
    if (!is.recursive(x)) break
    if (has_name_(x, i)) {
      x <- x[[i]]
      if (i == vars[length(vars)]) {
        success <- TRUE
      }
    } else if (any_recursive(x) && any(map_chr("has_name_", x, i))) {
      kp <- map_lgl("has_name_", x, i)
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


#' has_name_
#'
#' Tests whether object contains name(s)
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
"stopwords"

stopwords <- tidytext::stop_words$word[tidytext::stop_words$lexicon == "SMART"]




#' tabsort
#'
#' Returns a sorted (descending) frequence tbl
#'
#' @param x Character vector
#' @param V1 Optional, name of term variable. Defaults to "term".
#' @param percent Logical indicating whether to include a percent of total
#'   column.
#' @param na_omit Logical indicating whether to exclude missing. If all
#'   responses are missing, a missing value is used as the single category.
#' @return Frequency tbl
#' @export
tabsort <- function(x, V1 = NULL, percent = TRUE, na_omit = TRUE) {
  if (is.atomic(x) && all(is.na(x))) {
    x <- table(x, useNA = "ifany")
  } else {
    x <- sort(table(x), decreasing = TRUE)
  }
  x <- tibble::data_frame(term = names(x), n = as.integer(x))
  if (percent) {
    x$percent <- x$n / sum(x$n, na.rm = TRUE)
  }
  if (!is.null(V1)) {
    names(x)[1] <- V1
  }
  x
}

#' emc2ascii
#'
#' Makes text to ascii friendly.
#'
#' @param x Text, a character vector.
#' @param y Replacement for non-ascii characters. Defaults to "" (blank).
#' @return ascii friendly text
#' @export
enc2ascii <- function(x, y = "") {
  stopifnot(is.character(x))
  iconv(x, to = "ascii", sub = y)
}


#' has_factor_potential
#'
#' Tests whether vector has factor potential
#'
#' @param x Vector
#' @param p_uq Proportion of cases that represent unique values. If this value
#'   is less than one, than ID vars will be returned FALSE, and so on.
#' @param max_chars Number of chars from which the maximum acceptable
#'   MEAN string size should be allowed.
#' @return Logical.
#' @export
has_factor_potential <- function(x, p_uq = .9, max_chars = 60) {
  if (!is.atomic(x)) {
    return(FALSE)
  }
  if (is.character(x)) {
    if (mean(nchar(x), na.rm = TRUE) > max_chars) {
      return(FALSE)
    }
  }
  x <- as.character(x)
  n1 <- length(x) - ceiling(length(x) * (1 - p_uq))
  if (n_uq(x) > n1) {
    return(FALSE)
  }
  TRUE
}
