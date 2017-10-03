# rtweet
## Package make file

## update/document package
devtools::document(roclets = c('rd', 'collate', 'namespace'))

## install
devtools::install()

x <- rm_links(rt$text[1:15])
x <- rm_linebreaks(x)
x <- rm_amp(x)
x <- enc2ascii(x)
x <- trim_ws(x)
x <- rm_retweets(x)
cat(break_lines(x, n = 30), fill = TRUE)

tabsort(unlist(rt$mentions_screen_name))

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


