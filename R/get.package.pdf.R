#' get_package_pdf
#'
#' @param package Name of target package.
#' @param update logical, indicating whether to replace old manual
#' @param open logical, indicating whether to automatically view preview of pdf
#' @export
get_package_pdf <- function(package, update = FALSE, open = TRUE) {
  if (paste0(package, ".pdf") %in% list.files()) {
    if (update){
      file.remove(paste0(package, ".pdf"))
    } else {
      stop("pdf already exists. use 'update = TRUE' to replace old manual.")
    }
  }

  if (open){
    preview <- NULL
  } else{
    preview <- "--no-preview"
  }

  path <- find.package(package)
  try(system(paste(shQuote(file.path(R.home("bin"), "R")),
                   "CMD",
                   "Rd2pdf",
                   shQuote(path),
                   preview),
             ignore.stdout = TRUE, ignore.stderr = TRUE))

  message <- paste(package, "pdf saved here:", path, sep = " ")
  return(print(message))
}
