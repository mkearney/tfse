#' set class
#'
#' Set class with a parenthetical function.
#'
#' @param x Object to assign new class to.
#' @param value Class value to assign to x
#' @return Object x as class value.
#' @export
set_class <- function(x, value) `class<-`(x, value)

## do the git stuff

#' add_to_git
#'
#' Update github repository
#'
#' @param m Commit message
#' @param pkg Repo name. Defaults to working directory file name.
#' @param pull Logical indicating whether to pull the repo prior to
#'   pushing
#' @param interactive Logical indicating whether to ask prior to
#'   committing to Github repo.
#'
#' @return Sends local repo to Github.
#' @export
add_to_git <- function(m = "misc", pkg = ".", pull = TRUE, interactive = TRUE) {
  msg <- paste0(
    "Sure you want to update your github repo for ",
    basename(normalizePath(pkg)), "?"
  )
  if (interactive) {
    r <- menuline(msg, c("Yes", "No"))
    if (r == 2L) {
      stop("cool cool cool no doubt no doubt")
    }
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
    trim_ws(map_chr_("[[", x, 2L)),
    names = map_chr_("[[", x, 1L),
    class = "list"
  ))
  year <- format(Sys.Date(), "%Y")
  d$Title <- paste0(d$Package, ": ", d$Title)

  ## create citation text
  ce <- paste0('
citHeader("To cite ', d$Package, ' use:")\n
citEntry(
    entry        = "Manual",
    title        = "', d$Title, '",
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
