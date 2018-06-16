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
  rcmd_install(pkg)
  ## remove source files
  unlink(pkg, recursive = TRUE)
}


rcmd_install <- function(pkg) {
  if (.Platform$OS.type == "windows") {
    system2(file.path(R.home("bin"), "Rcmd.exe"), c("INSTALL", pkg))
  } else {
    system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", pkg))
  }
}

git_clone <- function(repo) {
  repo <- sprintf("git@github.com:%s.git", repo)
  system2("git", c("clone", repo))
}
