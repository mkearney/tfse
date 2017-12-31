
can_plus <- function(x) {
  inherits(x, c("numeric", "integer", "matrix", "array", "Date", "POSIXt", "gg"))
}


can_eval <- function(.e) {
  x <- tryCatch(suppressWarnings(eval(.e)),
    error = function(e) return("errorcannoteval"))
  if (!is.null(x) && identical(x, "errorcannoteval")) return(FALSE)
  TRUE
}

#' Add (paste) together character vectors
#'
#' Paste or paste0 character vectors using the plus operator.
#'
#' @param e1 lhs
#' @param e2 rhs
#' @return Computed value
#' @examples
#' ## normal plus operator
#' 20 + 1
#' Sys.Date() + 5
#' matrix(1:4, 2, 2) + matrix(1:4, 2, 2)
#' array(1:8, c(2, 2, 2)) + array(1:8, c(2, 2, 2))
#'
#' ## normal errors
#' Sys.Date() + Sys.Date()
#'
#' ## paste together characters
#' "this" + "that"
#'
#' ## or characters and numbers
#' "this" + 2000
#' 2000 + "that"
#'
#' ## ignores missing
#' c(NA, "this") + c("that", NA) + c("other", "other")
#'
#' ## use paste0 instead of paste
#' "this" + "^that"
#' "this$" + "that"
#'
#' ## escape ^ or $ to use paste
#' "this" + "\\^that"
#' "this\\$" + "that"
#'
#' ## add new ^ or $ and escape other to still use paste0
#' "this" + "^\\^that"
#' "this\\$$" + "that"
#'
#' ## string together any number of character vectors
#' "this" + "that" + "other"
#' "ABC_$" + letters + "^_XYZ"
#' letters + "^$" + letters
#' "T$" + "his" + "that" + "or" + "the other" + "^."
#'
#'
#' @method + character
#' @export
"+.character" <- function(e1, e2) {
  unlist(Map("plus_character", e1, e2, USE.NAMES = FALSE))
}


`+` <- function(e1, e2) {
   if (is.character(e1) || is.character(e2)) {
     return(try(`+.character`(e1, e2)))
   }
   try(.Primitive("+")(e1, e2))
}

#' @export
"%+%" <- `+.character`

## `+` <- function(e1, e2) {
##   if (!(is.character(e1) || is.character(e2))) {
##     return(try(.Primitive("+")(e1, e2)))
##   }
##   ## if (can_eval(e1) && can_plus(e1) && can_eval(e2) && can_plus(e2)) {
##   ##   return(try(.Primitive("+")(e1, e2)))
##   ## }
##   ## if (!can_eval(e1) || (!can_plus(e1) && !is_alnum(e1))) {
##   ##   stop("object ", sQuote(substitute(e1)), " not found", call. = FALSE)
##   ## }
##   ## if (!can_eval(e2) || (!can_plus(e2) && !is_alnum(e2))) {
##   ##   stop("object ", sQuote(substitute(e2)), " not found", call. = FALSE)
##   ## }
##   if (is.numeric(e1) || is.integer(e1)) e1 <- as.character(e1)
##   if (is.numeric(e2) || is.integer(e2)) e2 <- as.character(e2)
##   try(`+.character`(e1, e2))
## }

## `+` <- function(e1, e2) {
##   if (!(is.character(e1) || is.character(e2))) {
##     return(try(.Primitive("+")(e1, e2)))
##   }
##   ## if (can_eval(e1) && can_plus(e1) && can_eval(e2) && can_plus(e2)) {
##   ##   return(try(.Primitive("+")(e1, e2)))
##   ## }
##   ## if (!can_eval(e1) || (!can_plus(e1) && !is_alnum(e1))) {
##   ##   stop("object ", sQuote(substitute(e1)), " not found", call. = FALSE)
##   ## }
##   ## if (!can_eval(e2) || (!can_plus(e2) && !is_alnum(e2))) {
##   ##   stop("object ", sQuote(substitute(e2)), " not found", call. = FALSE)
##   ## }
##   if (is.numeric(e1) || is.integer(e1)) e1 <- as.character(e1)
##   if (is.numeric(e2) || is.integer(e2)) e2 <- as.character(e2)
##   try(`+.character`(e1, e2))
## }

`plus_character` <- function(e1, e2) {
  if (is.na(e1)) e1 <- "$"
  if (is.na(e2)) e2 <- "^"
  if (grepl("^\\^", e2)) {
    return(paste0(e1, gsub("^\\^", "", e2)))
  } else if (grepl("^\\\\\\^", e2)) {
    e2 <- gsub("^\\\\\\^", "^", e2)
  } else if (grepl("^[^\\]?\\$$", e1)) {
    return(paste0(gsub("\\$$", "", e1), e2))
  } else if (grepl("\\\\\\$$", e1)) {
    e1 <- gsub("\\\\\\$$", "$", e1)
  }
  paste(e1, e2)
}

is_alnum <- function(x) inherits(x, c("character", "numeric", "integer", "logical"))
