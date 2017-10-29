#' Update all out of date packages
#'
#' @return Updates all packages.
#' @export
update_packages <- function() {
  pkgs <- old.packages()
  pkgs <- unname(pkgs[, 1])
  op <- options()
  on.exit(options(op))
  options(install.packages.check.source = "no")
  options(install.packages.compile.from.source = "never")
  message("Updating ", length(pkgs), " packages...\n")
  update.packages(oldPkgs = pkgs, ask = FALSE)
}
