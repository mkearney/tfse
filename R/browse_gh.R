#' Open Github repo
#'
#' A github.com-specific wrapper around browseURL
#'
#' @param repo Name of github repo. If null (the default), function opens to
#'   github.com. For several popular repos, simply supply the name of the pkg/
#'   repo and the user/org will be imputed. Otherwise, supply the user/repo.
#' @return Opens browser to appropriate Github.com page
#' @export
browse_gh <- function(repo = NULL) {
  if (tryCatch(is.null(repo), error = function(e) FALSE)) {
    return("")
  }
  repo <- substitute(repo)
  repo <- as.character(repo)
  if (repo %in% cc("rtweet,botrnot,tfse,textfeatures")) {
    repo <- sprintf("mkearney/%s", repo)
  } else if (repo %in% cc(
    "dbplyr,dplyr,tidyr,httr,readr,purrr,tibble,stringr,forcats,tidyverse,ggplot2,modelr")) {
    repo <- sprintf("tidyverse/%s", repo)
  } else if (repo %in% cc(
    "devtools,usethis,pkgdown,stylr,testthat,httr,xml2,rlang,fs,lobstr")) {
    repo <- sprintf("r-lib/%s", repo)
  } else if (repo %in% cc("rvest,r4ds,adv-r,r-pkgs,emo")) {
    repo <- sprintf("hadley/%s", repo)
  } else if (grepl("roxygen", repo)) {
    repo <- "klutometis/roxygen"
  } else if (repo %in% cc("quanteda,readtext")) {
    repo <- sprintf("quanteda/%s", repo)
  } else if (grepl("fivethirtyeight|538", repo)) {
    repo <- "rudeboybert/fivethirtyeight"
  } else if (repo %in% cc("blogdown,sparklyr,shiny,rmarkdown")) {
    repo <- sprintf("rstudio/%s", repo)
  } else if (repo %in% cc("knitr,xaringan,tinytex,servr")) {
    repo <- sprintf("yihui/%s", repo)
  } else if (repo %in% cc("patchwork,lime,tidygraph")) {
    repo <- sprintf("thomasp85/%s", repo)
  } else if (repo %in% cc("tidytext")) {
    repo <- sprintf("juliasilge/%s")
  } else if (repo %in% cc("tidy-text-mining")) {
    repo <- sprintf("dgrtwo/%s")
  } else if (repo %in% cc("janitor")) {
    repo <- sprintf("sfirke/%s")
  } else if (repo %in% cc("tidyquant,timetk,tibbletime")) {
    repo <- sprintf("business-science/%s")
  } else if (repo %in% cc("Rfacebook,twitter_ideology,streamr")) {
    repo <- sprintf("pablobarbera/%s")
  } else if (repo %in% cc("brms")) {
    repo <- sprintf("paul-buerkner/%s")
  } else if (repo %in% cc("cowsay")) {
    repo <- sprintf("sckott/%s")
  } else if (repo %in% cc(
    paste0("plotly,drake,pdftools,git2r,fulltext,bikedata,RefManageR,tabulizer,",
      "magick,rplos,assertr,elastic,rrrpkg,Rselenium,googleLanguageR"))) {
    repo <- sprintf("ropensci/%s")
  } else {
    repo <- sprintf("search?q=%s", repo)
  }
  browseURL(sprintf("https://github.com/%s", repo))
}
