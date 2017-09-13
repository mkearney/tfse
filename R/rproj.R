

rproj_settings <- function(pkg = TRUE,
                           version = "1.0",
                           restore_workspace = "Default",
                           save_workspace = "Default",
                           always_save_history = "Default",
                           quit_child_processes_on_exit = "Default",
                           enable_code_indexing = "Yes",
                           use_spaces_for_tab = "Yes",
                           num_spaces_for_tab = "2",
                           encoding = "UTF-8",
                           rnw_weave = "knitr",
                           latex = "XeLaTeX",
                           auto_append_newline = "Yes",
                           strip_trailing_whitespace = "Yes",
                           build_type = "Package",
                           package_use_devtools = "Yes",
                           package_install_args = "--no-multiarch --with-keep.source --install-tests",
                           package_build_args = "--no-build-vignettes",
                           package_roxygenize = "rd,collate,namespace") {

    params <- list(
      Version = version,
      RestoreWorkspace = restore_workspace,
      SaveWorkspace = save_workspace,
      AlwaysSaveHistory = always_save_history,
      QuitChildProcessesOnExit = quit_child_processes_on_exit,
      EnableCodeIndexing = enable_code_indexing,
      UseSpacesForTab = use_spaces_for_tab,
      NumSpacesForTab = num_spaces_for_tab,
      Encoding = encoding,
      RnwWeave = rnw_weave,
      LaTeX = latex,
      AutoAppendNewline = auto_append_newline,
      StripTrailingWhitespace = strip_trailing_whitespace,
      BuildType = build_type,
      PackageUseDevtools = package_use_devtools,
      PackageInstallArgs = package_install_args,
      PackageBuildArgs = package_build_args,
      PackageRoxygenize = package_roxygenize
    )
    params[c(1, 5, 9, 11, 13)] <- paste0(params[c(1, 5, 9, 11, 13)], "\n")
    if (!pkg) {
      params <- params[1:13]
    }
    params <- paste0(names(params), ": ", params)
    paste(params, collapse = "\n")
}

#' rproj
#'
#' Creates new rProf file.
#'
#' @param name Name of project. Defaults to basename of working directory.
#' @param pkg Logical indicating whether the project is an R package. Defaults
#'   to true.
#' @param ... Other args passed to rproj settings.
#' @return Saved .Rproj file in current working directory.
#' @export
rproj <- function(name = NULL, pkg = TRUE, ...) {
  if (is.null(name)) {
    name <- basename(getwd())
  }
  rproj <- rproj_settings(pkg = pkg, ...)
  cat(rproj, file = paste0(name, ".Rproj"), fill = TRUE)
}
