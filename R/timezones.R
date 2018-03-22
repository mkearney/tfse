
#' convert date-time time zone to UTC
#'
#' @param x data
#'
#' @return Object with date-times in UTC timezone
#' @export
#'
#' @examples
#' time2utc(Sys.time())
time2utc <- function(x) {
  UseMethod("time2utc")
}

#' @export
time2utc.POSIXt <- function(x) {
  utc_time(x)
}

#' @export
time2utc.data.frame <- function(x) {
  if (any(vply_lgl(inherits, x, "POSIXt"))) {
    psx <- vply_lgl(inherits, x, "POSIXt")
    x[psx] <- lapply(x[psx], time2utc)
  }
  x
}


get_rl <- function(tokens = get_tokens(), new = FALSE) {
  if (!new && exists(".rate_limit")) {
    rl <- get("rl", envir = .rate_limit)
  } else {
    rl <- rate_limit(tokens)
    rl$timestamp <- utc_time()
    rl$reset_at <- utc_time(rl$reset_at)
    .rate_limit <- new.env()
    assign(".rate_limit", .rate_limit, envir = .GlobalEnv)
    assign("rl", rl, envir = .rate_limit)
  }
  rl
}

#' UTC time zone
#'
#' @param x
#'
#' @return Date time in UTC time zone
#' @export
#' @rdname tz_time
#'
#' @examples
#' utc_time(Sys.time())
#' ets_time(Sys.time())
#' cst_time(Sys.time())
#' mst_time(Sys.time())
#' pst_time(Sys.time())
utc_time <- function(x = Sys.time()) {
  if (is.numeric(x)) x <- try(as.POSIXct(x, origin = "1970-01-01"))
  stopifnot(inherits(x, "POSIXt"))
  as.POSIXct(
    format(x, tz = "UTC"),
    tz = "UTC")
}

#' CST time zone
#'
#' @export
#' @rdname tz_time
cst_time <- function(x = Sys.time()) {
  if (is.numeric(x)) x <- try(as.POSIXct(x, origin = "1970-01-01"))
  stopifnot(inherits(x, "POSIXt"))
  as.POSIXct(
    format(x, tz = "America/Chicago"),
    tz = "America/Chicago")
}

#' EST time zone
#'
#' @export
#' @rdname tz_time
est_time <- function(x = Sys.time()) {
  if (is.numeric(x)) x <- try(as.POSIXct(x, origin = "1970-01-01"))
  stopifnot(inherits(x, "POSIXt"))
  as.POSIXct(
    format(x, tz = "America/New_York"),
    tz = "America/New_York")
}

#' MST time zone
#'
#' @export
#' @rdname tz_time
mst_time <- function(x = Sys.time()) {
  if (is.numeric(x)) x <- try(as.POSIXct(x, origin = "1970-01-01"))
  stopifnot(inherits(x, "POSIXt"))
  as.POSIXct(
    format(x, tz = "America/Denver"),
    tz = "America/Denver")
}

#' PST time zone
#'
#' @export
#' @rdname tz_time
pst_time <- function(x = Sys.time()) {
  if (is.numeric(x)) x <- try(as.POSIXct(x, origin = "1970-01-01"))
  stopifnot(inherits(x, "POSIXt"))
  as.POSIXct(
    format(x, tz = "America/Los_Angeles"),
    tz = "America/Los_Angeles")
}


update_rl <- function(fun = NULL, token = 1L, n = 1L, refresh = FALSE) {
  rl <- get_rl(new = refresh)
  if (!is.null(fun)) {
    twapis <- unlist(rtweet:::funs_and_apis())
    if (any(grepl(fun, twapis))) {
      kp <- names(twapis)[grep(fun, twapis)]
    } else {
      kp <- grep(fun, names(twapis), value = TRUE)
    }
    if (length(kp) == 0L) stop("couldn't match API path")
    rl$remaining[
      rl$app == unique(rl$app)[token] & rl$query %in% kp] <- rl$remaining[
      rl$app == unique(rl$app)[token] & rl$query %in% kp] - n
  }
  timestamp <- utc_time()
  rl$remaining <- ifelse(
    rl$reset_at <= timestamp, rl$limit, rl$remaining)
  rl$reset_at <- utc_time(
    ifelse(rl$reset_at <= timestamp,
           rl$reset_at + 60 * 15,
           rl$reset_at)
  )
  rl$reset <- difftime(rl$reset_at, utc_time(), units = "mins")
  assign("rl", rl, envir = get(".rate_limit", envir = .GlobalEnv))
  if (sum(rl$remaining))
  rl <- subset(rl, grepl(paste(kp, collapse = "|"), query),
               select = c(limit, remaining, reset, app))
  if (sum(rl$remaining > 0L) == 0L) {
    slp <- as.numeric(min(rl$reset), "secs")
    app <- rl$app[rl$reset == min(rl$reset)][1]
    message("Sleeping for " + slp + " seconds")
    Sys.sleep(slp)
  } else {
    app <- rl$app[order(rl$remaining, decreasing = TRUE)][1]
  }
  which(unique(rl$app) == app)
}



#' Normalize scale(s)
#'
#' Transform numeric objects into z-scores
#'
#' @param x Data
#' @return Numeric objects transformed via standard normal distribution.
#' @export
scale_normal <- function(x) UseMethod("scale_normal")

#' @export
scale_normal.default <- function(x) {
  scale(x)[, 1]
}

#' @export
scale_normal.matrix <- function(x) {
  scale(x)
}

#' @export
scale_normal.data.frame <- function(x) {
  tibble::as_tibble(scale(x), validate = FALSE)
}




#' Standardize scale(s)
#'
#' Transform numeric objects into values on 0-1 scale
#'
#' @param x Data
#' @return Numeric objects transformed onto 0-1 scale
#' @export
scale_standard <- function(x) UseMethod("scale_standard")

#' @export
scale_standard.default <- function(x) {
  xmin <- min(x, na.rm = TRUE)
  (x - xmin) / (max(x, na.rm = TRUE) - xmin)
}

#' @export
scale_standard.matrix <- function(x) {
  x[vply_lgl(x, is.numeric)] <- lply(x[vply_lgl(x, is.numeric)], scale_standard)
  x
}

#' @export
scale_standard.data.frame <- function(x) {
  x[vply_lgl(x, is.numeric)] <- lply(x[vply_lgl(x, is.numeric)], scale_standard)
  tibble::as_tibble(x, validate = FALSE)
}

