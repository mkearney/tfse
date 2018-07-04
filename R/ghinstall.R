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
  op <- old.packages()
  if (any(!deps %in% ip)) {
    message("Installing dependencies...")
    deps <- deps[deps %in% ip]
    newdeps <- deps[!deps %in% ip]
    for (i in newdeps) {
      task <- rlang::quo(suppressMessages(install.packages(newdeps[i],
        verbose = FALSE, quiet = TRUE)))
      task_progress_bar(task, sprintf("Installing %s", newdeps[i]))
    }
  }
  if (any(deps %in% op)) {
    olddeps <- deps[deps %in% op]
    for (i in seq_along(olddeps)) {
      task <- rlang::quo(suppressMessages(update.packages(olddeps[i],
        ask = FALSE)))
      task_progress_bar(task, sprintf("Installing %s", olddeps[i]))
    }
  }
  if (.Platform$OS.type == "windows") {
    task <- rlang::quo(system3(file.path(R.home("bin"), "Rcmd.exe"),
      "INSTALL", pkg))
  } else {
    task <- rlang::quo(system3(file.path(R.home("bin"), "R"),
      "CMD", "INSTALL", pkg))
  }
  task_progress_bar(task, sprintf("Installing %s", pkg))
  invisible()
}



task_progress_bar <- function(task, msg) {
  w <- getOption("width", 100)
  ## b/c only monsters set width > 100
  if (w > 100)
    w <- 100
  r <- (w - nchar(msg) - 7) %/% 2
  cat(paste0(msg, " +"))
  for (j in seq_len(r)) {
    Sys.sleep(.025)
    cat("+")
  }
  rlang::eval_tidy(task)
  for (k in seq_len(r)) {
    Sys.sleep(.025)
    if (k == r) {
      cat(" 100%", fill = TRUE)
    } else {
      cat("+")
    }
  }
  invisible(TRUE)
}

system3 <- function(cmd, ..., capture = FALSE) {
  args <- list(...)
  if (length(args) == 0)
    args <- ""
  args <- paste(args, collapse = " ")
  cmd <- paste(shQuote(cmd), args)
  if (!capture) {
    cmd <- paste(cmd, ">/dev/null 2>/dev/null")
  }
  invisible(tryCatch(.Internal(system(cmd, TRUE, 0)),
    error = function(e) return(e)))
}


install_pkg_verbose <- function(pkg) {
  sh <- install.packages(pkg, quiet = TRUE)
  task_progress_bar(rlang::quo(invisible()), sprintf("Installing %s", pkg))
}

git_clone <- function(repo, silent = FALSE) {
  repo <- sprintf("git@github.com:%s.git", repo)
  if (silent) {
    sh <- system3("git", "clone -sq", repo)
    return(invisible())
  } else {
    system(sprintf("git clone %s", repo))
  }
}
