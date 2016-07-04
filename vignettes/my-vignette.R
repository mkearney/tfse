## ---- echo = FALSE, eval = FALSE-----------------------------------------
#  if (packageVersion("devtools") < 1.6) {
#    install.packages("devtools")
#  }
#  devtools::install_github("mkearney/tfse")

## ------------------------------------------------------------------------
### install and load tfse
#devtools::install_github("mkearney/tfse")
library(tfse)

## ------------------------------------------------------------------------
### first time running get_token() function should open web browser 
### select yes/agree to authorize each app
### replace 'appX_name' with name of your application (see: 'obtaining access tokens')
### replace 'xxxx' with alpha-numeric keys associated with your apps
#tokens <- c(get_token(app = "app1_name",
#                      consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
#                      consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
#            get_token(app = "app2_name",
#                      consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
#                      consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
#            get_token(app = "app3_name",
#                      consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
#                      consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
### save tokens for easy use in future sessions
### take note of working directory
#save(tokens, file = "tokens")
#getwd()

### to load tokens in new session, uncomment the next line
load("/Users/mwk/r/tfse/tt/tokens") # specify whole path if changing working directory

## ------------------------------------------------------------------------
### use multiple tokens to return 225,000 followers 
### (75,000 followers for each token)
hrc_followers <- get_followers_max("HillaryClinton", tokens[3:4])
hrc_followers[1:10]

## ------------------------------------------------------------------------
### lookup user data for thousands of users
### (up to 18,000 users for each token)
users <- get_lookup_max(hrc_followers[1:1000], tokens[1:2])
users

## ------------------------------------------------------------------------
### use multiple tokens to return 45 user networks (aka friends)
### (15 user networks for each token)
user_ids <- sample(users$user_id[users$protected == FALSE], 45)
user_networks <- get_friends_max(user_ids, tokens[6:8])
user_networks[[1]][1:5]
user_networks[[2]][1:5]
user_networks[[3]][1:5]

## ------------------------------------------------------------------------
### search tweets via REST API
### (number of tweets returned varies; broader searchers are best)
elect16 <- search_tweets(q = "election2016", 
                         count = 500,
                         token = tokens[[2]])
elect16

## ------------------------------------------------------------------------
### read tweets in stream - collect tweets for 5 minutes (timeout = 300s)
### the code below returned a data frame with 3963 rows 
e16stream <- filter_stream(
  stream = "realdonaldtrump,hillaryclinton,hillary,trump,
            election2016,imwithher,makeamericagreatagain",
  timeout = 10,
  token = tokens[[2]])
e16stream

