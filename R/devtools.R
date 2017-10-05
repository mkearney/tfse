#' make_package
#' 
#' Document and load R package (devtools wrapper)
#'
#' @param update Optional, indicating the size of change so as to automate
#'   updating of the package version number.
#' @param pkg Name of package. Defaults to current file name.
#' @param load_all Logical indicating whether or not to load all functions
#'   on exit.
#' @importFrom devtools install document load_all
#' @export 
make_package <- function(update = NULL, pkg = ".", load_all = TRUE) {
  pkg <- basename(normalizePath(pkg))
  if (!is.null(update)) {
    if (!update %in% c("minor", "major", "patch")) {
      stop("update must be one of patch, minor, or major", call. = FALSE)
    }
    update_version_number(update)
  }
  devtools::document(roclets = c('rd', 'collate', 'namespace'))
  devtools::install()
  if (load_all) {
    devtools::load_all()
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
  pkg <- basename(normalizePath(pkg))
  msg <- paste0(
    "Sure you want to update your github repo for ", pkg, "?"
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
#' @param pkg Name of package. Defaults to current file name.
#' 
#' @details If custom css is supplied (must be saved as custom.css in the
#'   docs folder), then it will be used to supplement the default pkgdown
#'   theme.
#'
#' @importFrom pkgdown build_site
#' @export 
pkg_website  <- function(pkg = ".") {
  pkg <- basename(normalizePath(pkg))
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

#' make_citation
#' 
#' Makes (or updates) CITATION file.
#' 
#' @param pkg Name of package. Defaults to current file name.
#' @return Saves updated CITATION file.
#' @export 
make_citation <- function(pkg = ".") {
  ## read description
  if (!file.exists("DESCRIPTION")) {
    stop("DESCRIPTION file not found", call. = FALSE)
  }
  x <- readlines("DESCRIPTION")
  ## convert to pkg info vars
  x <- strsplit(x, "\\:(?!\\/)", perl = TRUE)
  x <- x[lengths(x) == 2L]
  d <- as.list(structure(
    trim_ws(map_chr("[[", x, 2L)),
    names = map_chr("[[", x, 1L),
    class = "list"
  ))
  year <- format(Sys.Date(), "%Y")
  d$Title <- paste0(d$Package, ": ", d$Title)

  ## create citation text
  ce <- paste0('
citHeader("To cite ', d$Package, ' use:")\n
citEntry(
    entry        = "Manual",
    title        = "', d$Title, '"
    author       = as.person("Michael W. Kearney"),
    year         = ', year, ',
    note         = "R package version ', d$Version, '",
    url          = "', d$URL, '",
    key          = "', d$Package, '-package",
    textVersion  = paste(
        "Kearney, M. W. (', year, '). ', d$Title, '.",
        "R package version ', d$Version, '. Retrieved from",
        "', d$URL, '"
    )
)
'
)

  ## create inst dir if it doesn't already exist
  if (!dir.exists("inst")) {
    dir.create("inst")
  }
  ## save updated citation
  cat(ce, file = "inst/CITATION", fill = TRUE)
}

