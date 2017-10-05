#' make_package
#' 
#' Document and load R package (devtools wrapper)
#'
#' @param pkg Name of package. Defaults to current file name.
#' @param update Optional, indicating the size of change so as to automate
#'   updating of the package version number.
#' @param load_all Logical indicating whether or not to load all functions
#'   on exit.
#' @importFrom devtools install document load_all
#' @export 
make_package <- function(pkg = ".", update = NULL, load_all = TRUE) {
  if (!is.null(update)) {
    update_version_number(update, pkg)
  }
  devtools::document(pkg, roclets = c('rd', 'collate', 'namespace'))
  devtools::install(pkg)
  if (load_all) {
    devtools::load_all(pkg)
  }
  return(invisible())
}

## do the git stuff

#' add_to_git
#' 
#' Update github repository
#'
#' @param m Commit message
#' @param pkg Repo name. Defaults to working directory file name.
#' @param pull Logical indicating whether to pull the repo prior to
#'   pushing
#' 
#' @return Sends local repo to Github.
#' @export 
add_to_git <- function(m = "misc", pkg = ".", pull = TRUE) {
  msg <- paste0(
    "Sure you want to update your github repo for ", basename(pkg), "?"
  )
  r <- menuline(msg, c("Yes", "No"))
  if (r == 2L) {
    stop("cool cool cool no doubt no doubt")
  }
  if (!identical(pkg, ".")) {
    op <- getwd()
    setwd(pkg)
    on.exit(setwd(op))
  }
  if (pull) {
    system("git pull")
  }
  system("git add .")
  system(paste0("git commit -m \"", m, "\""))
  system("git push")
}

## pkg_website
#' 
#' Updates pkg website using pkgdown
#'
#' @details If custom css is supplied (must be saved as custom.css in the
#'   docs folder), then it will be used to supplement the default pkgdown
#'   theme.
#'
#' @importFrom pkgdown build_site
#' @export 
pkg_website  <- function() {
  pkgdown::build_site(preview = FALSE)
  if (file.exists("docs/custom.css")) {
    con <- file("docs/custom.css")
    x <- readLines(con, warn = FALSE)
    close(con)
    file.remove("docs/pkgdown.css")
    cat(
      paste(x, collapse = "\n"),
      file = "docs/pkgdown.css",
      fill = TRUE
    )
  }
  browseURL("docs/index.html")
}
