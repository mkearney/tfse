##------------------------------------------------------------------##
##                     data collection script                       ##
##------------------------------------------------------------------##

# install/load rtweet
# devtools::install_github("mkearney/rtweet")
library(rtweet)

# load twitter oauth tokens
tokens <- get_tokens()

# read sample user ids
o <- suppressMessages(readr::read_csv(
  "/Users/mwk/r/tfse/study/data/users_3000"))
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
  r <- tryCatch(get_friends(user_ids[i], token = tokens[j]),
    error = function(e) return(NULL))
  if (is.null(r)) r <- dplyr::data_frame(ids = NA_real_)
  d[[i]] <- r
  if (i %% 15 == 0) j <- j + 1
  if (i %% 300 == 0) message(paste0(i/30, "% completed."))
}


# wait if necessary
j <- 1
wait <- rate_limit(tokens[[j]], "friends/ids")[["reset"]]
Sys.sleep(wait[[1]] * 70)

# final data collection loop
for (i in (max_tkn + 1):3000) {
  r <- tryCatch(get_friends(user_ids[i], token = tokens[j]),
    error = function(e) return(NULL))
  if (is.null(r)) r <- dplyr::data_frame(ids = NA_real_)
  d[[i]] <- r
  if (i %% 15 == 0) j <- j + 1
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
fl <- unlist(lapply(d$friends, function(x) length(unlist(x))))
hist(fl, col = "gray", breaks = 30,
  main = "Friends", ylab = "freq",
  xlab = "# of friends")

# unique ids
if (all(
  length(unique(d$user_id)) == 3000,
  sum(fl == 1) < 100,
  length(unique(fl)))) {
  message("Wave data collection completed!")
}

##------------------------------------------------------------------##
##                          wave path                               ##
##------------------------------------------------------------------##
wave_num <- max(
    sapply(list.files("/Users/mwk/r/tfse/study/data")[grep("+wave",
      list.files("/Users/mwk/r/tfse/study/data"))], function(x)
        as.numeric(unlist(strsplit(x, split = "e")[[1]][2])))) + 1

##------------------------------------------------------------------##
##                          save data                               ##
##------------------------------------------------------------------##

# save local
readr::write_rds(d,
  paste0("/Users/mwk/r/tfse/study/data/wave", wave_num))

# save dropbox
readr::write_rds(d,
  paste0("/Users/mwk/Dropbox/dissrtweetion/data/wave", wave_num))

message(paste0("Data saved as ",
  paste0("/Users/mwk/r/tfse/study/data/wave", wave_num),
  "!"))
