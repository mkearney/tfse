devtools::document()
devtools::install()

devtools::install("~/R/tfse")

library(purrr)
library(tfse)

find_funfile <- function(x) {
  x <- search_files(gsub("\\.", "\\\\.", x), "R")
  x <- grep("R/\\S+\\.R", x, value = TRUE)
  unique(gsub("^R/|\\.R.*", "", x))
}
find_funfiles <- function(x) {
  f <- map(x, find_funfile)
  names(f) <- x
  f
}

move_file <- function(x) {
  file.copy(sprintf("R/%s.R", x), sprintf("~/Desktop/tfse-old/%s.R", x))
  file.remove(sprintf("R/%s.R", x))
}
devtools::install()
devtools::document()

move_file("zzz")
map("funs", move_file)

devtools
usethis::use_build_ignore(sprintf("R/%s.R", fd))
