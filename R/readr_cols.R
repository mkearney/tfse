#' Easy readr column specification
#'
#' Shortcut-loaded convenience wrapper around \code{\link[readr]{cols}}.
#'
#' @param ... All arguments are passed to \code{\link[readr]{readr::cols}} with
#'  any character values first treated and processed as shortcuts for equivalent
#'   \pkg{readr} functions–i.e., \code{readr::col_*}, \code{readr::parse_*}.
#'   See details for a list of available shortcuts.
#' @details The following list of shortcuts can be supplied as character strings
#'   in place of \pkg{readr} \code{col_*} or \code{parse_*} functions:
#'
#' \itemize{
#'   \item l, lgl, logical
#'   \item i, int, integer
#'   \item d, dbl, double
#'   \item c, chr, character
#'   \item D, date
#'   \item T, dt, dttm, datetime, POSIXct
#'   \item f, fct, factor
#'   \item n, num, number
#'   \item ?, gss, guess
#'   \item _, -, skip
#' }
#' @return Column specification
#' @examples
#'
#' ## readr package must be installed
#' if (requireNamespace("readr"), quietly = TRUE) {
#'   ## create column specification
#'   readr_cols(
#'     .default = "?",
#'     id = "chr",
#'     is_good = "lgl",
#'     day = "date",
#'     timestamp = readr::col_datetime(format = "%m-%y-%d %H:%M:%S"),
#'     count = "integer",
#'     misc = "_"
#'   )
#' }
#' @export
readr_cols <- function(...) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("`readr_cols()` requires that you install {readr}.",
      call. = FALSE)
  }
  args <- lapply(capture_dots(...), chr2col)
  do.call(readr::cols, args)
}

chr2col <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  switch(x,
    ##------------------------------------------------------------------------##
    ##                    {x}|col_{x}: read column as {x}                     ##
    ##------------------------------------------------------------------------##
    l               = quote(readr::col_logical()),
    lgl             = quote(readr::col_logical()),
    logical         = quote(readr::col_logical()),
    col_logical     = quote(readr::col_logical()),

    i               = quote(readr::col_integer()),
    int             = quote(readr::col_integer()),
    integer         = quote(readr::col_integer()),
    col_integer     = quote(readr::col_integer()),

    d               = quote(readr::col_double()),
    dbl             = quote(readr::col_double()),
    double          = quote(readr::col_double()),
    col_double      = quote(readr::col_double()),

    c               = quote(readr::col_character()),
    chr             = quote(readr::col_character()),
    character       = quote(readr::col_character()),
    col_character   = quote(readr::col_character()),

    f               = quote(readr::col_factor()),
    fct             = quote(readr::col_factor()),
    factor          = quote(readr::col_factor()),
    col_factor      = quote(readr::col_factor()),

    D               = quote(readr::col_date()),
    date            = quote(readr::col_date()),
    col_date        = quote(readr::col_date()),

    t               = quote(readr::col_time()),
    time            = quote(readr::col_time()),
    col_time        = quote(readr::col_time()),

    T               = quote(readr::col_datetime()),
    dt              = quote(readr::col_datetime()),
    dttm            = quote(readr::col_datetime()),
    datetime        = quote(readr::col_datetime()),
    posix           = quote(readr::col_datetime()),
    POSIX           = quote(readr::col_datetime()),
    posixct         = quote(readr::col_datetime()),
    POSIXct         = quote(readr::col_datetime()),
    col_datetime    = quote(readr::col_datetime()),

    n               = quote(readr::col_number()),
    nmb             = quote(readr::col_number()),
    nmbr            = quote(readr::col_number()),
    num             = quote(readr::col_number()),
    number          = quote(readr::col_number()),
    col_number      = quote(readr::col_number()),

    `_`             = quote(readr::col_skip()),
    `-`             = quote(readr::col_skip()),
    skip            = quote(readr::col_skip()),
    col_skip        = quote(readr::col_skip()),

    `?`             = quote(readr::col_guess()),
    gss             = quote(readr::col_guess()),
    guess           = quote(readr::col_guess()),
    col_guess       = quote(readr::col_guess()),

    ##------------------------------------------------------------------------##
    ##                    as_{x}|parse_{x}: convert to {x}                    ##
    ##------------------------------------------------------------------------##
    as_l            = quote(readr::parse_logical()),
    as_lgl          = quote(readr::parse_logical()),
    as_logical      = quote(readr::parse_logical()),
    parse_l         = quote(readr::parse_logical()),
    parse_lgl       = quote(readr::parse_logical()),
    parse_logical   = quote(readr::parse_logical()),

    as_i            = quote(readr::parse_integer()),
    as_int          = quote(readr::parse_integer()),
    as_integer      = quote(readr::parse_integer()),
    parse_i         = quote(readr::parse_integer()),
    parse_int       = quote(readr::parse_integer()),
    parse_integer   = quote(readr::parse_integer()),

    as_d            = quote(readr::parse_double()),
    as_dbl          = quote(readr::parse_double()),
    as_double       = quote(readr::parse_double()),
    parse_d         = quote(readr::parse_double()),
    parse_dbl       = quote(readr::parse_double()),
    parse_double    = quote(readr::parse_double()),
    parse_double    = quote(readr::parse_double()),

    as_c            = quote(readr::parse_character()),
    as_chr          = quote(readr::parse_character()),
    as_character    = quote(readr::parse_character()),
    parse_c         = quote(readr::parse_character()),
    parse_chr       = quote(readr::parse_character()),
    parse_character = quote(readr::parse_character()),
    parse_character = quote(readr::parse_character()),

    as_f            = quote(readr::parse_factor()),
    as_fct          = quote(readr::parse_factor()),
    as_factor       = quote(readr::parse_factor()),
    parse_f         = quote(readr::parse_factor()),
    parse_fct       = quote(readr::parse_factor()),
    parse_factor    = quote(readr::parse_factor()),
    parse_factor    = quote(readr::parse_factor()),

    as_D            = quote(readr::parse_date()),
    as_date         = quote(readr::parse_date()),
    parse_D         = quote(readr::parse_date()),
    parse_date      = quote(readr::parse_date()),
    parse_date      = quote(readr::parse_date()),

    as_t            = quote(readr::parse_time()),
    as_time         = quote(readr::parse_time()),
    parse_t         = quote(readr::parse_time()),
    parse_time      = quote(readr::parse_time()),
    parse_time      = quote(readr::parse_time()),

    as_T            = quote(readr::parse_datetime()),
    as_dt           = quote(readr::parse_datetime()),
    as_dttm         = quote(readr::parse_datetime()),
    as_datetime     = quote(readr::parse_datetime()),
    as_posix        = quote(readr::parse_datetime()),
    as_POSIX        = quote(readr::parse_datetime()),
    as_posixct      = quote(readr::parse_datetime()),
    as_POSIXct      = quote(readr::parse_datetime()),
    parse_T         = quote(readr::parse_datetime()),
    parse_dt        = quote(readr::parse_datetime()),
    parse_dttm      = quote(readr::parse_datetime()),
    parse_datetime  = quote(readr::parse_datetime()),
    parse_posix     = quote(readr::parse_datetime()),
    parse_POSIX     = quote(readr::parse_datetime()),
    parse_posixct   = quote(readr::parse_datetime()),
    parse_POSIXct   = quote(readr::parse_datetime()),
    parse_datetime  = quote(readr::parse_datetime()),

    as_n            = quote(readr::parse_number()),
    as_nmb          = quote(readr::parse_number()),
    as_nmbr         = quote(readr::parse_number()),
    as_num          = quote(readr::parse_number()),
    as_number       = quote(readr::parse_number()),
    parse_n         = quote(readr::parse_number()),
    parse_nmb       = quote(readr::parse_number()),
    parse_nmbr      = quote(readr::parse_number()),
    parse_num       = quote(readr::parse_number()),
    parse_number    = quote(readr::parse_number()),
    parse_number    = quote(readr::parse_number()),

    `as__`          = quote(readr::parse_skip()),
    `as_-`          = quote(readr::parse_skip()),
    as_skip         = quote(readr::parse_skip()),
    `parse__`       = quote(readr::parse_skip()),
    `parse_-`       = quote(readr::parse_skip()),
    parse_skip      = quote(readr::parse_skip()),
    parse_skip      = quote(readr::parse_skip()),

    `as_?`          = quote(readr::parse_guess()),
    as_gss          = quote(readr::parse_guess()),
    as_guess        = quote(readr::parse_guess()),
    `parse_?`       = quote(readr::parse_guess()),
    parse_gss       = quote(readr::parse_guess()),
    parse_guess     = quote(readr::parse_guess()),
    parse_guess     = quote(readr::parse_guess())
  )
}

