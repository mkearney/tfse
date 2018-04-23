
#' A smart function for reading data files
#'
#' Reads data based on file extension
#'
#' @param x Name of file to read.
#' @param ... Other args passed on to read-in function, see details for more info.
#' @details Here is the function logic \itemize{
#'   \item csv = readr::read_csv
#'   \item rds = readRDS
#'   \item rda = load
#'   \item xlsx = openxlsx::read.xlsx
#'   \item sav = haven::read_sav
#'   \item dta = haven::read_dta
#' }
#' @examples
#' ## save congress data as .rds, .csv, and .rda files
#' saveRDS(congress, "congress.rds")
#' save(congress, file = "congress.rda")
#' write.csv(congress, "congress.csv", row.names = FALSE)
#'
#' ## now read and view each file
#' (c1 <- read_smart("congress.rds"))
#' (c2 <- read_smart("congress.csv"))
#' (c3 <- read_smart("congress.rda"))
#'
#' @return Info from file returned as R data object.
#' @export
read_smart <- function(x, ...) {
  if (grepl("\\.csv", x)) {
    suppressMessages(readr::read_csv(x, ...))
  } else if (grepl("\\.rds$", x, ignore.case = TRUE)) {
    readRDS(x, ...)
  } else if (grepl("\\.rda$", x, ignore.case = TRUE)) {
    read_rda(x, ...)
  } else if (grepl("\\.xlsx$", x)) {
    openxlsx::read.xlsx(x, ...)
  } else if (grepl("\\.sav$", x)) {
    haven::read_sav(x, ...)
  } else if (grepl("\\.dta$", x)) {
    haven::read_dta(x, ...)
  } else {
    warning("file extension not recognized. reading as text.")
    readr::read_lines(x, ...)
  }
}

#' A more normal way to read-in .rda data/
#'
#' Reads in .rda files like any other data file would (outputs an object, which you can name whatever you want)
#'
#' @inheritParams read_smart
#' @rdname read_smart
#' @export
read_rda <- function(x) {
  ## validate input
  stopifnot(is.character(x) && length(x) == 1L)
  ## load into new environment
  rda_env <- new.env()
  load(x, envir = rda_env)
  ## fetch from environment and return
  get(ls(envir = rda_env), envir = rda_env)
}
