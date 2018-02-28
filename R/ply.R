lply <- function(.x, .f, ..., n.cores = NULL) {
  ## number of cores
  n.cores <- n_cores(n.cores)
  ## apply in parallel
  parallel::mclapply(X = .x, FUN = rlang::as_closure(.f), ..., mc.cores = n.cores)
}

ulply <- function(.x, .f, ..., n.cores = NULL) {
  unlist(lply(.x, .f, ..., n.cores = NULL), use.names = FALSE)
}

lcly <- function(.x, .f, ..., cl.cores = NULL) {
  ## number of cores
  cl.cores <- cl_cores(cl.cores)
  cl <- parallel::makeCluster(cl.cores)
  x <- parallel::parLapply(cl, X = .x, fun = rlang::as_closure(.f), ...)
  parallel::stopCluster(cl)
  x
}


n_cores <- function(x) {
  if (is.null(x) || !is.numeric(x)) {
    x <- getOption("mc.cores")
    if (length(x) == 0L) {
      x <- parallel::detectCores() - 1L
      options(mc.cores = x)
    }
  }
  x
}

cl_cores <- function(x) {
  if (is.null(x) || !is.numeric(x)) {
    x <- getOption("cl.cores")
    if (length(x) == 0L) {
      x <- parallel::detectCores(logical = FALSE)
      options(cl.cores = x)
    }
  }
  x
}
