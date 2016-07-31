##------------------------------------------------------------------##
##                     data collection script                       ##
##------------------------------------------------------------------##

# install rtweet
# devtools::install_github("mkearney/rtweet")

# load rtweet
library(rtweet)

# load twitter oauth tokens
tokens <- get_tokens()

# get sample user ids
o <- readr::read_csv("/Users/mwk/r/tfse/study/data/users_3000")
user_ids <- o$user_id
rm(o)

# for loop function
tokeN <- function(n) {
  nset <- unlist(lapply(c(
    1:length(tokens), 1:(200 - length(tokens))),
    function(x) rep(x, 15)))
  nset[n]
}

##------------------------------------------------------------------##
##                         collect data                             ##
##------------------------------------------------------------------##

# create list vector
d <- vector("list", 3000)
max_tkn <- (length(tokens) * 15)

# run loop(s) to collect data
for (i in 1:max_tkn) {
  d[[i]] <- get_friends(user_ids[i], tokens[[tokeN(i)]])
}

# wait if necessary
rate_limit(tokens[[1]], "friends/ids")

# final data collection loop
for (i in (max_tkn + 1):3000) {
  d[[i]] <- get_friends(
    user_ids[i],
    tokens[[tokeN(i)]])
}

# create data frame
d <- dplyr::data_frame(
  user_id = user_ids,
  date = Sys.Date(),
  friends = d)

##------------------------------------------------------------------##
##                         inspect data                             ##
##------------------------------------------------------------------##

# first 10 rows
d

# friends list length
hist(unlist(lapply(d$friends, function(x) length(unlist(x)))),
  col = "gray", breaks = 30, main = "")

# unique ids
if (length(unique(d$user_id)) == 3000) {
  message("Wave data collection completed!")
}

##------------------------------------------------------------------##
##                          wave path                               ##
##------------------------------------------------------------------##
wave_num_path <- paste0(
  "/Users/mwk/r/tfse/study/data/wave", max(
    sapply(list.files("study/data")[grep("+wave",
      list.files("study/data"))], function(x)
        as.numeric(unlist(strsplit(x, split = "e")[[1]][2])))) + 1)

##------------------------------------------------------------------##
##                          save data                               ##
##------------------------------------------------------------------##

# save data
readr::write_rds(d, wave_num_path)
message(paste0("Data saved as ", wave_num_path, " !"))
