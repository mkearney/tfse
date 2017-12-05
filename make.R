# tfse
## Package make file

## unload tfse
#unloadNamespace("tfse")
#devtools::document(roclets = c('rd', 'collate', 'namespace'))
#devtools::install()

## build package / update version number
make_package(load_all = FALSE, update = "patch")

## add to git
rm_.DS_Store()
add_to_git("added sem_fit and par_table (lavaan functions)")
1

## check
if (FALSE) {
  devtools::check(
    document = FALSE,
    check_dir = ".check"
  )
}

rm_.DS_Store()

## pkgdown
devtools::install_github("hadley/pkgdown")

## build site
pkgdown::build_site()
