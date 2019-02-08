includes_username <- function(x) {
  grepl("^\\S+/\\S+", x)
}

r_dir <- function() {
  home_dir <- normalizePath("~")
  meta <- list()
  r_dir <- list_dirs(home_dir)
  for (i in seq_along(r_dir)) {
    meta[[length(meta) + 1L]] <-  count_r_pkgs(r_dir[i])
  }
  d <- do.call("rbind", meta, quote = TRUE)
  d$path[order(d$r_pkg, decreasing = TRUE)][1]
}

list_dirs <- function(path = ".", full.names = TRUE, recursive = FALSE,
                      all.names = FALSE) {
  dirs <- list.dirs(path, full.names, recursive)
  if (all.names) {
    return(dirs)
  }
  grep("\\.[^/]+$", dirs, value = TRUE, invert = TRUE)
}


matches_previous <- function(x, include_previous = TRUE) {
  m <- x == c(NA, x[-length(x)])
  m[1] <- FALSE
  if (!include_previous) {
    return(m)
  }
  m | c(m[-1], FALSE)
}

matches_next <- function(x, include_next = TRUE) {
  m <- x == c(x[-1], NA)
  m[length(m)] <- FALSE
  if (!include_next) {
    return(m)
  }
  m | c(FALSE, m[-length(m)])
}

is_r_package <- function(path) {
  file.exists(file.path(path, "DESCRIPTION")) &&
    file.exists(file.path(path, "NAMESPACE")) &&
    dir.exists(file.path(path, "R"))
}


count_r_pkgs <- function(path) {
  dirs <- list.dirs(path, full.names = TRUE, recursive = FALSE)
  tf <- dapr::vap_lgl(dirs, is_r_package)
  tfse::data_set(
    path = path,
    r_pkg = sum(tf),
    total = length(tf),
    prop = if (length(tf) > 0) sum(tf) / length(tf) * 100 else 0
  )
}
