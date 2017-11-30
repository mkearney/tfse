# tfse
## Package make file

## unload tfse
#unloadNamespace("tfse")
#devtools::document(roclets = c('rd', 'collate', 'namespace'))
#devtools::install()

## build package / update version number
make_package(load_all = FALSE, update = "patch")

## add to git
add_to_git("now contains map_ (mine) and map (purrr compat) functions")
1

## check
if (FALSE) {
  devtools::check(
    document = FALSE,
    check_dir = ".check"
  )
}

## pkgdown
devtools::install_github("hadley/pkgdown")

## build site
pkgdown::build_site()
