
#' update_version_number
#' 
#' Updates version number in R package documentation
#'
#' @param update_type 
#' @param path Path to base directory of given R package.
#' @return Updates and saves DESCRIPTION file.
#' @export 
update_version_number <- function(update_type = "patch", pkg = getwd()) {
  pkg <- gsub("DESCRIPTION$", "", pkg)
  if (!file.exists(file.path(pkg, "DESCRIPTION"))) {
    stop(
      "No file named \"DESCRIPTION\" found in pkg directory."
    )
  }
  x <- readlines(file.path(pkg, "DESCRIPTION"))
  v <- grep("^Version:", x)
  nv <- gsub("Version:\\s{0,1}", "", x[v], perl = TRUE)
  nv <- as.version_number(nv)
  if (update_type == "patch") {
    update <- 1
  } else if (update_type == "minor") {
    update <- 100 - nv[3]
  } else if (update_type == "major") {
    update <- 100 - nv[3]
    update <- (9 - nv[2]) * 100 + update
  } else {
    stop(
      "update_type must be equal to one of \"patch\", \"minor\", \"major\"",
      call. = FALSE
    )
  }
  nv <- nv + update
  x[v] <- gsub(
    "(?<=Version: )([[:digit:]]|.){1,}",
    paste.version_number(nv),
    x[v], perl = TRUE
  )
  writelines(x, "DESCRIPTION")
}


#' version_number
#'
#' Class for version numbers.
#' 
#' @param major Increment major, e.g. 1.0.0, for a major release. This
#'   is best reserved for changes that are not backward compatible and
#'   that are likely to affect many users. Going from 0.b.c to 1.0.0
#'   typically indicates that your package is feature complete with a
#'   stable API.
#' @param minor Increment minor, e.g. 0.9.0, for a minor release. A
#'   minor release can include bug fixes, new features and changes in
#'   backward compatibility. This is the most common type of
#'   release. It’s perfectly fine to have so many minor releases that
#'   you need to use two (or even three!) digits, e.g. 1.17.0.
#' @param major Increment patch, e.g. 0.8.2 for a patch: you’ve fixed
#'   bugs without adding any significant new features. I’ll often do a
#'   patch release if, after release, I discover a show-stopping bug
#'   that needs to be fixed ASAP. Most releases will have a patch
#'   number of 0.
#' @export
version_number <- function(major, minor, patch) {
  stopifnot(map_lgl(is.numeric, list(major, minor, patch)))
  major <- as.integer(major)
  if (minor == 0) {
    minor <- 0L
  } else {
    minor <- as.integer(
      substr(format(minor / 10, digits = 1), 3, 3)
    )
  }
  if (patch == 0) {
    patch <- 0L
  } else {
    patch <- patch
  }
  structure(
    c(major = major, minor = minor, patch = patch),
    class = c("version_number")
  )
}

#' @export
as.version_number <- function(x) {
  stopifnot(length(x) == 1L)
  x <- strsplit(x, "\\.")[[1]]
  if (nchar(x[[3]]) == 1) {
    x[[3]] <- paste0(x[[3]], "0")
  }
  do.call(version_number, as.list(as.integer(x)))
}

#' @export
print.version_number <- function(x) {
  if (x[3] < 10L & x[3] > 0L) {
    x[3] <- paste0("0", x[3])
  } else if (x[3] %% 10L == 0) {
    x[3] <- substr(x[3], 1, 1)
  }
  x <- paste0(x[1], ".", x[2], ".", x[3])  
  print(x)
}

#' @export
paste.version_number <- function(x) {
  if (x[3] < 10L & x[3] > 0L) {
    x[3] <- paste0("0", x[3])
  } else if (x[3] %% 10L == 0) {
    x[3] <- substr(x[3], 1, 1)
  }
  paste0(x[1], ".", x[2], ".", x[3])
}


#' @export
cutnum <- function(x, s1, s2) {
  as.numeric(substr(x, s1, s2))
}

#' @export
`+.version_number` <- function(e1, e2) {
  if (length(e2) == 1L && is.character(e2)) {
    e2 <- as.version_number(e2)
  } else if (length(e2) == 1L && is.numeric(e2)) {
    if (e2 < 100) {
      e2 <- version_number(0, 0, cutnum(e2, 1, 2)) 
    } else if (e2 < 1000) {
      e2 <- version_number(0, cutnum(e2, 1, 1), cutnum(e2, 2, 3)) 
    } else {
      e2 <- version_number(cutnum(e2, 1, 1), cutnum(e2, 2, 2), cutnum(e2, 3, 4)) 
    }
  }
  x <- as.matrix(e1) + as.matrix(e2)
  x <- as.list(x)
  if (x[[3]] == 100L) {
    x[[2]] <- x[[2]] + 1L
    x[[3]] <- x[[3]] <- 0L
  }
  if (x[[2]] == 10L) {
    x[[1]] <- x[[1]] + 1L
    x[[2]] <- x[[2]] <- 0L
  }
  do.call("version_number", x)
}

