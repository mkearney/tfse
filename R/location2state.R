#' Convert Twitter locations to U.S. state names
#'
#' Estimates U.S. state given Twitter user location data.
#'
#' @param x Character vector of Twitter user location data.
#' @return Character vector of same length with U.S. names (or a
#'   generic "United States") for values based on best guess for each
#'   user. Cases with insufficient information are returned as NAs.
#' @examples
#'
#' @export
location2state <- function(x) {
  ## create output vector
  states <- rep(NA_character_, length(x))
  ## trim white space
  x <- trim_ws(x)
  ## remove all periods
  x <- gsub("\\.", "", x)
  ## remove only periods between last two-letters end of string
  #dots <- grepl("[[:alpha:]]{1}\\.[[:alpha:]]\\.$", x)
  #if (sum(dots) > 0L) {
  #  x <- gsub("\\.(?=[[:alpha:]]{1}\\.)", "", x, perl = TRUE)
  #}
  ## identify trailing abbs or two-letter locations
  ## if identifies as US
  usas <- tolower(x) %in% c("us", "usa", "united states")
  states[usas] <- "United States"
  x[usas] <- ""
  endusas <- grepl("\\busa$|\\bus$", x, ignore.case = TRUE)
  states[endusas] <- "United States"
  x <- gsub("\\busa$|\\bus$", "", x)
  x[endusas] <- trim_ws(x[endusas])
  x[endusas] <- gsub("\\,$", "", x[endusas])
  ta <- grepl("[^\\,]{2,}(\\,|\\s{1})\\s{0,1}[[:alpha:]]{2}$", x)
  if (length(ta) > 0L) {
    xta <- substr(x[ta], nchar(x[ta]) - 1L, nchar(x[ta]))
    states[ta] <- state.abb2name(xta)
    x[ta] <- ""
  }
  ta <- grepl("[^\\,]{2,}\\,\\s{0,1}[^\\,]{2,}$", x)
  if (length(ta) > 0L) {
    xta <- gsub("[^\\,]{2,}\\,\\s{0,1}", "", x[ta])
    states[ta] <- state.name2name(xta)
    x[ta] <- ""
  }
  states[is.na(states)] <- state.any2name(x[is.na(states)])
  states
}



state.abb2name <- function(x) {
  state.name[match(toupper(x), state.abb)]
}

state.name2name <- function(x) {
  x <- tolower(trim_ws(x))
  state.name[match(x, tolower(state.name))]
}

state.any2name_ns <- function(x) {
  x <- tolower(trim_ws(x))
  x <- c(state.name, state.name)[match(
    x, gsub(" ", "", tolower(c(state.name, state.abb))))
  ]
  if (length(x) == 0L) return(NA_character_)
  x[1L]
}

state.any2name <- function(x) {
  x <- gsub("north ", "north", x, ignore.case = TRUE)
  x <- gsub("south ", "south", x, ignore.case = TRUE)
  x <- gsub("west ", "west", x, ignore.case = TRUE)
  x <- strsplit(x, "\\s|\\,")
  x[lengths(x) == 0L] <- NA_character_
  vapply(x, state.any2name_ns, FUN.VALUE = character(1))
}
