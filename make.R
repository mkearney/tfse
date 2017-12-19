# tfse
## Package make file

## unload tfse

#unloadNamespace("tfse")
#devtools::document(roclets = c('rd', 'collate', 'namespace'))
#devtools::install()

## build package / update version number
make_package(load_all = FALSE, update = "patch")

## rm annoying .ds_store files
rm_.DS_Store()

swp(p = 1 - .0001, lang = "en")

## add to git
add_to_git("added swp (stop words by probability) function")
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
