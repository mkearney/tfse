# load tfse package
library(tfse)

# calculate wave number
wave <- round(as.numeric(Sys.Date() - as.Date("2016-05-19")) / 7, 0) + 1

# run data collection script
collect_diss_data(wave)

gfm_rest <- get_friends_max(ids, tokens, start = (nrow(gfm) + 1))
gfm <- rbind(gfm, gfm_rest)
