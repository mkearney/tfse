
devtools::install("~/R/tfse")
setwd("~/R/tfse")
usethis::use_build_ignore("make.R")
usethis::use_build_ignore("tfse.sublime-workspace")
usethis::use_build_ignore("tfse.sublime-project")
usethis::use_git_ignore("tfse.sublime-workspace")
usethis::use_git_ignore("tfse.sublime-project")
add_to_git()