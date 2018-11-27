

#' Description variables
#'
#' Get values from package DESCRIPTION
#'
#' @param pkg Character string, name of package
#' @param field Character string, name of field/key
#' @return Data frame
#' @export
desc_get_var <- function(pkg, field = NULL) {
  Map(desc_get_pkg, pkg, field = field)
}

#' @inheritParams desc_get_var
#' @rdname desc_get_var
#' @export
desc_get_url <- function(pkg) {
  Map(desc_get_pkg, pkg, "URL")
}

desc_get_pkg <- function(pkg, field = NULL) {
  d <- set_class(utils::packageDescription(pkg), "list")
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  if (!is.null(field)) {
    choices <- tolower(names(d))
    if (!any(grepl("url", choices, ignore.case = TRUE))) {
      return(NULL)
    }
    kp <- which(choices == tolower(field))[1]
    d <- d[1, kp, drop = TRUE]
    names(d) <- field
  }
  d
}

#' @inheritParams desc_get_var
#' @rdname desc_get_var
#' @export
desc_gh_repo <- function(pkg) {
  u <- desc_get_url(pkg)
  if (!isTRUE(grepl("github\\.com", u))) {
    d <- desc_get_pkg(pkg)
    u <- d$BugReports
  }
  if (isTRUE(grepl("github\\.com", u))) {
    u <- regmatches_(u, "(?<=com/)[^/]+/[^/]+",
      drop = TRUE)
    p <- regmatches_(u, "[[:punct:]]", drop = TRUE)
    if (length(p) > 0) {
      p <- p[!p %in% c("-", "/")]
      pat <- paste(paste0("\\", p, ".*"),
        collapse = "|")
      u <- sub(pat, "", u)
    }
    u <- grep(pkg, u, ignore.case = TRUE, value = TRUE)
  } else {
    u <- character()
  }

  if (length(u) == 0) {
    u <- find_gh_url(pkg)
  }
  if (length(u) > 1) u <- u[1]
  unname(u)
}


find_gh_url <- function(pkg) {
  ds <- list.files(system.file(package = pkg),
    full.names = TRUE, include.dirs = FALSE,
    recursive = TRUE)
  txt <- ""
  i <- 1
  while (!any(grepl("github\\.com/\\S+",
    txt, ignore.case = TRUE))) {
    if (i > length(ds)) break
    if (grepl("\\.rds$", ds[i], ignore.case = TRUE)) {
      txt <- as.character(readRDS(ds[i]))
    } else if (grepl("\\.Rdata$", ds[i],
      ignore.case = TRUE)) {
      e <- new.env()
      load(ds[i], e)
      obj <- ls(envir = e)
      if (length(obj) != 1) {
        txt <- tryreadlines(ds[i])
      } else {
        txt <- as.character(get(obj, envir = e))
      }
    } else {
      txt <- tryreadlines(ds[i])
    }
    i <- i + 1
  }

  if (length(txt) == 0 ||
      !any(grepl("github\\.com/\\S+",
    txt, ignore.case = TRUE))) {
    return(NULL)
  }
  u <- regmatches_(txt, "(?<=com/)[^/]+/[^/]+",
    drop = TRUE)
  if (length(u) == 0 || (is.na(u[1]))) {
    return(NULL)
  }
  p <- regmatches_(u, "[[:punct:]]", drop = TRUE)
  if (length(p) > 0) {
    p <- p[!p %in% c("-", "/")]
    pat <- paste(paste0("\\", p, ".*"),
      collapse = "|")
    u <- sub(pat, "", u)
  }
  u <- grep(pkg, u, ignore.case = TRUE, value = TRUE)
  u[1]
}


tryreadlines <- function(...) {
  suppressWarnings(tryCatch(readlines(...),
    error = function(e) NULL))
}
