

#' write rds to dropbox
#'
#' @param x Data object to save
#' @param file Path/file name in dropbox folder to save data.
#' @return Saves x as rds file in specified dropbox folder
#' @export
db_saveRDS <- function(x, file) {
  if (!requireNamespace("rdrop2", quietly = TRUE)) {
    stop("must install rdrop2 pkg to use this function")
  }
  db_load_token()
  tmp <- file.path(tempdir(), file)
  saveRDS(x, tmp)
  rdrop2::drop_upload(tmp, dirname(file))
}

#' write csv to dropbox
#'
#' @param x Data object to save
#' @param file Path/file name in dropbox folder to save data.
#' @return Saves x as CSV file in specified dropbox folder
#' @export
db_write_csv <- function(x, file) {
  if (!requireNamespace("rdrop2", quietly = TRUE)) {
    stop("must install rdrop2 pkg to use this function")
  }
  db_load_token()
  tmp <- file.path(tempdir(), file)
  readr::write_csv(x, tmp)
  rdrop2::drop_upload(tmp, dirname(file))
}

#' load/set token for dropbox
#'
#' @export
db_load_token <- function() {
  if (!requireNamespace("rdrop2", quietly = TRUE)) {
    stop("must install rdrop2 pkg to use this function")
  }
  if (exists("token", envir = rdrop2:::.dstate)) return(invisible())
  ## load and auto-insert dropbox token
  token <- readRDS("~/Dropbox/.r2drop.rds")
  assign("token", token, envir = rdrop2:::.dstate)
  invisible()
}

#' read rds from dropbox
#'
#' @param file Path/file name in dropbox folder to read.
#' @return Reads rds file from dropbox.
#' @export
db_readRDS <- function(file) {
  db_load_token()
  tmp <- tempfile()
  rdrop2::drop_download(file, tmp, overwrite = TRUE)
  readRDS(tmp)
}

#' read CSV from dropbox
#'
#' @param file Path/file name in dropbox folder to read.
#' @param ... Other args passed to readr::read_csv
#' @return Reads CSV file from dropbox.
#' @export
db_read_csv <- function(file, ...) {
  db_load_token()
  tmp <- tempfile()
  rdrop2::drop_download(file, tmp, overwrite = TRUE)
  readr::read_csv(tmp, ...)
}
