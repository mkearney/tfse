# load tfse package
library(tfse)

# calculate wave number
wave <- round(as.numeric(Sys.Date() - as.Date("2016-05-19")) / 7, 0) + 1

# run data collection script
get_wave_data(wave)

# wave 2
data("d2")
data("d1")
data("ids")

load_tokens()
gfm_rest <- get_friends_max(ids, tokens, start = (nrow(gfm) + 1))
gfm <- rbind(gfm, gfm_rest)

# messing around
d <- d1[d1$id %in% gfm$id, ]

names(d)[4] <- "f1"
names(gfm)[3] <- "f2"

d <- merge(d[, c(1, 2, 4)], gfm[, c(1, 3)], all = TRUE)

friends_added <- function(friends) {
  x <- as.character(unlist(friends[[1]]))
  y <- as.character(unlist(friends[[2]]))
  out <- length(y[!y %in% x])
  return(out)
}
friends_dropped <- function(friends) {
  x <- as.character(unlist(friends[[2]]))
  y <- as.character(unlist(friends[[1]]))
  out <- length(y[!y %in% x])
  return(out)
}

d$adds <- apply(d[, 3:4], 1, friends_added)
d$drops <- apply(d[, 3:4], 1, friends_dropped)




#get_package_pdf("tfse", update = TRUE, open = TRUE)
