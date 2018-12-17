
#' Install package
#'
#' A wrapper function to simplify devtools install
#'
#' @param pkg Name/path to package defaults to current directory
#' @param ... Passed to package build
#' @return Installs locally
#' @export
install_pkg <- function(pkg = ".", ...) {

  pkg <- as_package(pkg)

  if (is_loaded(pkg)) {
    eapply(pkgload::ns_env(pkg$package), force, all.names = TRUE)
  }
  build_opts <- c("--no-resave-data", "--no-manual", "--no-build-vignettes")
  opts <- "--install-tests"

  remotes::install_deps(pkg$path,
    build = TRUE, build_opts = build_opts,
    INSTALL_opts = opts, dependencies = TRUE, quiet = TRUE,
    force = FALSE, upgrade = TRUE, ...
  )

  install_path <- pkgbuild::build(pkg$path,
    dest_path = tempdir(),
    args = build_opts, quiet = FALSE
  )
  on.exit(unlink(install_path), add = TRUE)

  callr::rcmd("INSTALL",
    c(install_path, opts),
    echo = TRUE,
    show = TRUE
  )

  invisible(TRUE)
}



strip_slashes <- function(x) sub("/*$", "", x)

is_loaded <- function(pkg = ".") {
  pkg <- as_package(pkg)
  pkg$package %in% loadedNamespaces()
}


is_root <- function(path) identical(path, dirname(path))

is_package <- function(x) inherits(x, "package")

has_description <- function(path) file.exists(file.path(path, "DESCRIPTION"))

as_package <- function(x = NULL) {

  if (is_package(x)) {
    return(x)
  }
  x <- package_file(path = x)
  load_pkg_description(x)
}





package_file <- function(..., path = ".") {

  if (!is.character(path) || length(path) != 1) {
    stop("`path` must be a string.", call. = FALSE)
  }

  path <- strip_slashes(normalizePath(path, mustWork = FALSE))

  if (!file.exists(path)) {
    stop("Can't find '", path, "'.", call. = FALSE)
  }

  if (!file.info(path)$isdir) {
    stop("'", path, "' is not a directory.", call. = FALSE)
  }

  while (!has_description(path)) {
    path <- dirname(path)

    if (is_root(path)) {
      stop("Could not find package root.", call. = FALSE)
    }
  }

  file.path(path, ...)
}



load_pkg_description <- function(path) {

  path_desc <- file.path(path, "DESCRIPTION")

  desc <- as.list(read.dcf(path_desc)[1, ])
  names(desc) <- tolower(names(desc))
  desc$path <- path
  structure(desc, class = "package")
}
