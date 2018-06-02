

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
#' Creates new .Rproj file.
#'
#' @param path Project name (path).
#' @return Saved .Rproj file in path.
#' @rdname rproj
#' @export
rproj_pkg <- function(path = NULL) {
  ## set path to current wd if null
  if (is.null(path)) {
    path <- "."
  }
  ## create path if doesn't exist
  if (!dir.exists(path)) {
    dir.create(path)
  }
  ## expand path if current wd otherwise set wd
  if (identical(path, ".")) {
    path <- getwd()
  } else {
    op <- getwd()
    on.exit(setwd(op))
    setwd(path)
  }
  ## proj name
  pkg <- basename(path)
  ## create project
  usethis::create_package(path, open = FALSE)
  ## overwrite proj file
  x <- as.character(rproj_new_pkg())
  writeLines(x, paste0(pkg, ".Rproj"))
}

#' @rdname rproj
#' @export
rproj_site <- function(path = NULL) {
  ## set path to current wd if null
  if (is.null(path)) {
    path <- "."
  }
  ## create path if doesn't exist
  if (!dir.exists(path)) {
    dir.create(path)
  }
  ## expand path if current wd otherwise set wd
  if (identical(path, ".")) {
    path <- getwd()
  } else {
    op <- getwd()
    on.exit(setwd(op))
    setwd(path)
  }
  ## proj name
  site <- basename(path)
  ## create project
  usethis::create_project(path, open = FALSE)
  ## overwrite proj file
  x <- as.character(rproj_new_site())
  writeLines(x, paste0(site, ".Rproj"))
}


rproj_new_site <- function() {
  structure(list(
    Version                 = "1.0",
    BuildType               = "Website",

    `\nRestoreWorkspace`    = FALSE,
    SaveWorkspace           = FALSE,
    AlwaysSaveHistory       = FALSE,
    EnableCodeIndexing      = TRUE,
    UseSpacesForTab         = TRUE,
    NumSpacesForTab         = 2,
    Encoding                = "UTF-8",

    `\nRnwWeave`            = "knitr",
    LaTeX                   = "XeLaTeX",

    `\nAutoAppendNewline`   = TRUE,
    StripTrailingWhitespace = TRUE
  ), class = "rproj")
}

as.character.rproj <- function(x) {
  nms <- names(x)
  x <- unlist(unclass(x))
  x <- dplyr::case_when(
    x == TRUE ~ "Yes",
    x == FALSE ~ "No",
    TRUE ~ x
  )
  x <- paste0(nms, ":\t", x)
  x <- unlist(strsplit(x, "\\n"))
  tfse:::fill_space(x)
}

rproj_new_pkg <- function() {
  structure(list(
    Version                 = "1.0",
    `\nBuildType`           = "Package",
    PackageUseDevtools      = TRUE,
    PackageInstallArgs      = "--no-multiarch --with-keep.source",
    PackageRoxygenize       = "rd,collate,namespace",

    `\nRestoreWorkspace`    = FALSE,
    SaveWorkspace           = FALSE,
    AlwaysSaveHistory       = FALSE,
    EnableCodeIndexing      = TRUE,
    UseSpacesForTab         = TRUE,
    NumSpacesForTab         = 2,
    Encoding                = "UTF-8",

    `\nRnwWeave`            = "knitr",
    LaTeX                   = "XeLaTeX",

    `\nAutoAppendNewline`   = TRUE,
    StripTrailingWhitespace = TRUE
  ), class = "rproj")
}
