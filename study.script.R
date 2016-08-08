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

##------------------------------------------------------------------##
##                         collect data                             ##
##------------------------------------------------------------------##

# create list vector
d <- vector("list", 3000)
max_tkn <- (length(tokens) * 15)
j <- 1

# run loop(s) to collect data
for (i in seq_len(max_tkn)) {
  fds <- get_friends(user_ids[i], tokens[[j]], recode_error = TRUE)
  d[[i]] <- fds["ids"]
  if ((i %% 15) == 0) j <- j + 1
}

# wait if necessary
rate_limit(tokens[[j]], "friends/ids")

# final data collection loop
j <- 1
for (i in (max_tkn + 1):3000) {
  fds <- get_friends(user_ids[i], tokens[[j]], recode_error = TRUE)
  d[[i]] <- fds["ids"]
  if ((i %% 15) == 0) j <- j + 1
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
message(paste0("Data saved as ", wave_num_path, "!"))
