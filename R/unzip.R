#' Unzip files into directory
#'
#' Unzip archive into similarly named directory
#'
#' @param path Name of zipfile. Must end in ".zip"
#' @return Creates a directory in the same folder with the same name (minus the zip part)
#' @export
un_zip <- function(path) {
  stopifnot(grepl("\\.zip$", path))
  out <- sub("\\.zip$", "", path)
  stopifnot(!dir.exists(out))
  dir.create(out)
  utils::unzip(path, exdir = out)
}
