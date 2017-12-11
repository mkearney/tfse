# tfse
## Package make file

## unload tfse
#unloadNamespace("tfse")
#devtools::document(roclets = c('rd', 'collate', 'namespace'))
#devtools::install()

## build package / update version number
make_package(load_all = FALSE, update = "patch")

cols <- c("#2C4980", "#B63247", "#4DA52D", "#642780")

## rm annoying .ds_store files
rm_.DS_Store()

## add to git
add_to_git("added nin and yin functions")
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
