devtools::install_github("mkearney/rtweet")
library(rtweet)
setwd("/Users/mwk/r/tfse")

smp_ids <- read.csv("study/smp_ids")

library(rtweet)
rtweet::lookup_users(smp_ids$user_id[1:18000])

tokens <- get_tokens()

list.files("/Users/mwk")

lusr <- function(x, token) {
  tryCatch(lookup_users(x, token = token),
    error = function(e) return(data.frame()))
}

usR <- c(smp_ids$user_id[1:18000],
  smp_ids$user_id[18001:36000], smp_ids$user_id[36001:54000])

x <- mapply(lusr, usR, c(tokens[[1]], tokens[[2]], tokens[[3]]))

x
