

#' Read RDS
#'
#' Read serialized R data file
#'
#' @param path Name of .rds file
#' @return A data object
#' @export
#' @family readsave
read_RDS <- function(path) {
  con <- gzfile(path, "rb", encoding = "UTF-8")
  on.exit(close(con))
  readRDS(con)
}

#' Save RDS
#'
#' Save serialized R data file
#'
#' @param x Data object to be saved
#' @param path Name of the file to be saved
#' @param compress Logical indicating whether to compress data–it will take up
#'   less space but it will be slower. Defaults to FALSE.
#' @return Invisible data object
#' @export
#' @family readsave
save_RDS <- function(x, path, compress = FALSE) {
  if (compress) {
    con <- gzfile(path, "wb", encoding = "UTF-8")
  } else {
    con <- file(path, "wb", encoding = "UTF-8")
  }
  on.exit(close(con), add = TRUE)
  saveRDS(x, con)
  invisible(x)
}
