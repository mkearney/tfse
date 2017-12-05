#' SEM fit statistics
#'
#' Returns data frame of SEM fit statistics
#'
#' @param x Object of class lavaan.
#' @param digits Number of digits to round to, Defaults to 3.
#' @param scaled Logical indicating whether to return scaled fit indices if
#'   available.
#' @examples
#' ## specify model
#' m1 <- 'e =~ cyl + hp + wt\nmpg ~ e + gear'
#'
#' ## function to normalize data
#' normalize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
#'
#' ## fit model
#' f1 <- lavaan::sem(
#'   m1,
#'   data = tibble::as_tibble(dplyr::mutate_if(mtcars, is.numeric, normalize)),
#'   std.lv = TRUE,
#'   estimator = "mlr"
#' )
#'
#' ## view fit stats
#' sem_fit(f1)
#'
#' ## view par estimates
#' par_table(f1)
#'
#' @return Fit statistics tibble.
#' @importFrom lavaan fitMeasures
#' @export
#' @rdname lavaan
sem_fit <- function(x, digits = 3, scaled = TRUE) {
  stopifnot(inherits(x, "lavaan"))
  x <- lavaan::fitMeasures(x)
  x <- round(x, digits = digits)
  x <- as.list(x)
  if (scaled && sum(grepl("\\.scaled$", names(x))) > 0L) {
    df <- x$df.scaled
    chisq <- x$chisq.scaled
    cfi <- x$cfi.scaled
    tli <- x$tli.scaled
    rmsea <- x$rmsea.scaled
    l <- x$rmsea.ci.lower.scaled
    u <- x$rmsea.ci.upper.scaled
    srmr <- x$srmr_bentler
    message("Scaled fit statistics")
  } else {
    df <- x$df
    chisq <- x$chisq
    cfi <- x$cfi
    tli <- x$tli
    rmsea <- x$rmsea
    l <- x$rmsea.ci.lower
    u <- x$rmsea.ci.upper
    srmr <- x$srmr
    message("Fit statistics")
  }
  rmsea_ci <- paste0("[", l, ",", u, "]")
  tibble::as_tibble(data.frame(
    N = df,
    ChiSq = chisq,
    CFI = cfi,
    TLI = tli,
    RMSEA = rmsea,
    RMSEA_ci = rmsea_ci,
    SRMR = srmr
  ), validate = FALSE)
}

#' SEM parameter table
#'
#' @param x Object of class lavaan
#' @return Tibble of parameter estimates
#' @rdname lavaan
#' @export
par_table <- function(x) {
  stopifnot(inherits(x, "lavaan"))
  pt <- lavaan::parameterEstimates(x)
  cstr <- function(x) strsplit(x, ",")[[1]]
  pt <- pt[, names(pt) %in% cstr("group,free,lhs,op,rhs,est,se,z,pvalue,ci.lower,ci.upper")]
  names(pt)[names(pt) == "pvalue"] <- "p"
  tibble::as_tibble(pt, validate = FALSE)
}
