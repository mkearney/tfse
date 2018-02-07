# tfse
## Package make file

## unload tfse
#unloadNamespace("tfse")

## build via devtools
#devtools::document(roclets = c('rd', 'collate', 'namespace'))
#devtools::install()

"this " + "that"

## build package / update version number
make_package(load_all = FALSE, update = NULL)

## rm annoying .ds_store files
rm_.DS_Store()

## add to git
add_to_git("fixed +.character")
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
