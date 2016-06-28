# tfse
tfse = **t**witter **f**ollows & **s**elective **e**xposure

## about tfse
- this package contains R code used in my dissertation
- most of the functions are designed to interact with twitter's API
- this repository will one day be organized into more useful replication materials. until then, it's is a work in progress

## install
- to install
```{r}
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("mkearney/tfse")
```

## use
1. To create Twitter app(s) [and secure access to oauth tokens necessary for API queries]
visit http://apps.twitter.com/app/new
2. Enter information for `Name`, `Description`, `Website`, and `Callback URL` 
fields. Example provided below. For `Callback URL` make sure to copy/paste 
the following: `http://127.0.0.1:1410`

|  Field          |      Entry Box                          |
|-----------------|-----------------------------------------|
| Name            | tfse_app                                |
| Description     | Twitter follows and selective exposure  |
| Website         | http://twitter.com/kearneymw            |
| Callback URL    | http://127.0.0.1:1410                   |

```{r}
### save Twitter app name, consumer key, and consumer secret key as reusable oauth token script.
tokens <- c(get_token(app = "app1_name",
                      consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
                      consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
            get_token(app = "app2_name",
                      consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
                      consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
            get_token(app = "app3_name",
                      consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
                      consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
```
- example of package use
```{r}
### install and load tfse
devtools::install_github("mkearney/tfse")
library(tfse)

### call tokens script (saved .R file from above) to generate 'tokens' object
### first time running script should open web browser with Twitter 
### authorization page (for each app)
source("/Users/xxx/r/twitter_api/create.twitter.tokens.R") 
length(tokens) # number of tokens loaded

### use multiple tokens to return 225,000 followers (75,000 for each token) of Hillary Clinton
hrc_followers <- get_followers_max("HillaryClinton", tokens)
hrc_followers

### use multiple tokens to lookup user data for 54,000 (18,000 for each token) users.
users <- get_lookup_max(hrc_followers, tokens)
users

### use multiple tokens to return 45 (15 for each token) user networks (all of a 
### user's friends or accounts a user follows)
user_ids <- sample(users$id[users$protected == FALSE], 45) # random sample of 45 users with unprotected accounts
user_networks <- get_friends_max(user_ids, tokens)
user_networks
```

## about me
- i'm a phd candidate in political communication at the university of kansas
- my dissertation examines selective exposure and political polarization on twitter

## contact
- email me at mkearney@ku.edu
