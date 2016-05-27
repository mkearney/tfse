# load tfse package
library(tfse)

# calculate wave number
wave <- round(as.numeric(Sys.Date() - as.Date("2016-05-19")) / 7, 0) + 1

# run data collection script
get_wave_data(wave)
