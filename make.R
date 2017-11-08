# tfse
## Package make file

## update/document package
unloadNamespace("tfse")
devtools::document(roclets = c('rd', 'collate', 'namespace'))
#devtools::load_all()

## update version number
make_package()

?tidytext::stop_words

search_files("tidytext")

## add to git
add_to_git("fixed pkg dependencies")
1

## get some tweets
rt <- rtweet::search_tweets("lang:en")

## format the text
x <- rm_links(rt$text[1:15])
x <- rm_linebreaks(x)
x <- rm_amp(x)
x <- rm_retweets(x)
x <- enc2ascii(x)
x <- trim_ws(x)
x <- break_lines(x, n = 60)

## view print out
cat(paste(x, collapse = "\n"), fill = TRUE)

## view original
rt$text[1:15]

## tabsort
tabsort(unlist(rt$mentions_screen_name))

## has_factor_potential
map_lgl(has_factor_potential, rt)

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
