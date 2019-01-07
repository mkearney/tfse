
#' Install package
#'
#' A wrapper function to simplify devtools install
#'
#' @param pkg Name/path to package defaults to current directory
#' @param ... Passed to package build
#' @return Installs locally
#' @export
install_pkg <- function(pkg = ".", ...) {

  ## check package
  pkg <- as_package(pkg)

  ## update documentation
  document_pkg(pkg)

  ## unload if necessary
  if (is_loaded(pkg)) {
    eapply(pkgload::ns_env(pkg$package), force, all.names = TRUE)
  }

  ## options
  build_opts <- c("--no-resave-data", "--no-manual", "--no-build-vignettes")
  opts <- "--install-tests"

  ## dependencies
  print_start("Checking dependencies...")
  remotes::install_deps(
    pkg$path,
    build = TRUE,
    build_opts = build_opts,
    INSTALL_opts = opts,
    dependencies = NA,
    quiet = FALSE,
    force = FALSE,
    upgrade = "ask",
    ...
  )

  install_path <- pkgbuild::build(
    pkg$path,
    dest_path = tempdir(),
    args = build_opts,
    quiet = FALSE
  )

  on.exit(unlink(install_path), add = TRUE)

  print_start("Installing {", pkg$package, "}...")
  callr::rcmd(
    "INSTALL",
    c(install_path, opts),
    echo = FALSE,
    show = FALSE
  )
  print_complete("Installation complete!")

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

save_all <- function() {
  if (rstudioapi::hasFun("documentSaveAll")) {
    rstudioapi::documentSaveAll()
  }
}


r_env_vars <- function() {
  vars <- c(
    R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep),
    CYGWIN = "nodosfilewarning",
    R_TESTS = "",
    R_BROWSER = "false",
    R_PDFVIEWER = "false"
  )
  if (is.na(Sys.getenv("NOT_CRAN", unset = NA))) {
    vars[["NOT_CRAN"]] <- "true"
  }
  vars
}


with_envvar <- function(new, code, action = "replace") {
  old <- set_envvar(envs = new, action = action)
  on.exit(set_envvar(old))
  force(code)
}

document_pkg <- function(pkg = ".", roclets = NULL) {
  #check_suggested("roxygen2")
  pkg <- as_package(pkg)
  save_all()
  sh <- utils::capture.output(with_envvar(
    r_env_vars(),
    roxygen2::roxygenise(pkg$path, roclets)
  ))
  pkgload::dev_topic_index_reset(pkg$package)
  print_complete("Updated ", pkg$package, " documentation...")
  invisible()
}

check_suggested <- function(package) {
  pkgload::check_suggested(
    package = package,
    version = NULL,
    compare = NA,
    path = inst()
  )
}

inst <- function(name = "utils") {
  paths <- file.path(.libPaths(), name)
  paths <- paths[dir.exists(paths)]
  if (length(paths) > 0) {
    return(normalizePath(paths[1]))
  } else {
    return(NULL)
  }
}



is.named <- function(x) {
  !is.null(names(x)) && all(names(x) != "")
}

set_envvar <- function(envs, action = "replace") {

  if (length(envs) == 0) {
    return()
  }
  stopifnot(is.named(envs))
  stopifnot(is.character(action), length(action) == 1)
  action <- match.arg(action, c("replace", "prefix", "suffix"))
  envs <- envs[!duplicated(names(envs), fromLast = TRUE)]
  old <- Sys.getenv(names(envs), names = TRUE, unset = NA)
  set <- !is.na(envs)
  both_set <- set & !is.na(old)

  if (any(both_set)) {
    if (action == "prefix") {
      envs[both_set] <- paste(envs[both_set], old[both_set])
    } else if (action == "suffix") {
      envs[both_set] <- paste(old[both_set], envs[both_set])
    }
  }

  if (any(set)) {
    do.call("Sys.setenv", as.list(envs[set]))
  }

  if (any(!set)) {
    Sys.unsetenv(names(envs)[!set])
  }
  invisible(old)
}
