# rtweet
## Package make file

## update/document package
devtools::document(roclets = c('rd', 'collate', 'namespace'))

## install
devtools::install()

## check
if (FALSE) {
  devtools::check(
    document = FALSE,
    check_dir = ".check"
  )
}

## pkgdown
devtools::install_github("hadley/pkgdown")
pkgdown::build_site()


