dtm_ <- function(x) set_class(x, c("dtm", "DocumentTermMatrix", "simple_triplet_matrix"))



print.dtm <- function(x) {
  cat("##----------------------------------------------", fill = TRUE)
  cat("##  **Document term matrix (dtm)**", fill = TRUE)
  cat("##----------------------------------------------", fill = TRUE)
  cat(sprintf(
    "##  Documents           : %d", nrow(x)), fill = TRUE)
  cat(sprintf(
    "##  Terms               : %d", ncol(x)), fill = TRUE)
  cat(sprintf(
    "##  Non-sparse/sparse   : %d/%.0f", length(x$v),
    prod(dim(x)) - length(x$v)), fill = TRUE)
  sparsity <- if (!prod(dim(x))) 100 else round((1 - length(x$v)/prod(dim(x))) * 100)
  cat(sprintf(
    "##  Sparsity            : %s%%", sparsity), fill = TRUE)
  cat(sprintf(
    "##  Max term length     : %s",
    max(nchar(tm::Terms(x), type = "chars"), 0)), fill = TRUE)
  cat(sprintf(
    "##  Weighting           : %s (%s)",
    attr(x, "weighting")[1L], attr(x, "weighting")[2L]), fill = TRUE)
  invisible(x)
}


as_dtm <- function(x) UseMethod("as_dtm")
as_dtm.DocumentTermMatrix <- function(x) dtm_(x)



as_dtm.character <- function(x) {
  x <- gsub("https?[[:graph:]]+", "", x)
  x <- gsub("#\\S+", "", x)
  x <- gsub("@\\S+", "", x)
  x <- chr::chr_replace_nonascii(x)
  sws <- dplyr::pull(dplyr::filter(
    tidytext::stop_words, lexicon == "SMART"), "word")
  sws <- paste0(
    "\\b",
    paste(sws, collapse = "\\s{0,}\\b|\\b\\s{0,}"),
    "\\b")
  x <- gsub(sws, " ", x, ignore.case = TRUE)
  x <- tm::DocumentTermMatrix(
    tm::Corpus(tm::VectorSource(x)),
    control = list(
      removeNumbers = TRUE, removePunctuation = TRUE))
  term_tfidf <- tapply(x$v / slam::row_sums(x)[x$i],
    x$j, mean) * log2(tm::nDocs(x) / slam::col_sums(x > 0))
  x <- x[,term_tfidf >= 0.1]
  x <- x[slam::row_sums(x) > 0,]
  as_dtm(x)
}

