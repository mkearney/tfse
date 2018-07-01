#' Install package from Github
#'
#' Installs an R package from Github
#'
#' @param repo Name of repo e.g., 'mkearney/warcraft'
#' @return Installs package to default library
#' @export
ghinstall <- function(repo) {
  ## store workding dir, reset on exit
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  ## set wd to temp dir
  tmp <- tempdir()
  setwd(tmp)
  ## clone from git
  sh <- git_clone(repo)
  ## parse pkg name
  pkg <- sub(".*\\/", "", repo)
  ## install
  sh <- rcmd_install(pkg)
  ## remove source files
  unlink(pkg, recursive = TRUE)
  ## return sh invisibly
  invisible(sh)
}


rcmd_install <- function(pkg) {
  deps <- tools::package_dependencies(pkg)[[1]]
  ip <- installed.packages()
  if (any(!deps %in% ip)) {
    message("Installing dependencies...")
    deps <- deps[!deps %in% ip]
    for (i in deps) {
      install_pkg_verbose(i)
    }
  }
  w <- getOption("width")
  if (w > 80) {
    w <- 80
  }
  r <- (w - nchar(paste0("Installing {", pkg, "} +")) - 5) %/% 2
  cat(paste0("Installing ", pkg, " +"))
  for (j in seq_len(r)) {
    Sys.sleep(.025)
    cat("+")
  }
  for (k in seq_len(r)) {
    Sys.sleep(.025)
    if (k == r) {
      cat(" 100%", fill = TRUE)
    } else {
      cat("+")
    }
  }
  if (.Platform$OS.type == "windows") {
    sh <- suppressMessages(system2(file.path(R.home("bin"), "Rcmd.exe"), c("INSTALL", pkg),
      stdout = NULL))
  } else {
    sh <- suppressMessages(system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", pkg),
      stdout = NULL))
  }
  invisible(sh)
}


install_pkg_verbose <- function(pkg) {
  w <- getOption("width")
  if (w > 80) {
    w <- 80
  }
  r <- (w - nchar(paste0("Installing {", pkg, "} +")) - 5) %/% 2
  cat(paste0("Installing ", pkg, " +"))
  for (j in seq_len(r)) {
    Sys.sleep(.025)
    cat("+")
  }
  sh <- suppressMessages(install.packages(pkg, verbose = FALSE, quiet = TRUE))
  for (k in seq_len(r)) {
    Sys.sleep(.025)
    if (k == r) {
      cat(" 100%", fill = TRUE)
    } else {
      cat("+")
    }
  }
}

git_clone <- function(repo) {
  repo <- sprintf("git@github.com:%s.git", repo)
  sh <- suppressMessages(system2("git", c("clone -sq", repo), stdout = NULL))
  invisible(sh)
}
