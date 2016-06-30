# tfse
tfse = **t**witter **f**ollows & **s**elective **e**xposure

## about tfse
- this package contains R code used in my dissertation
- most of the functions are designed to interact with twitter's API
- this repository will one day be organized into more useful replication materials. until then, it's is a work in progress

## install
- to install `tfse` run the following code in R:
```{r}
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("mkearney/tfse")
```

## obtaining access tokens

1. To create Twitter app(s) [and secure access to oauth tokens necessary for API queries]
visit http://apps.twitter.com/app/new
2. Enter information in `Name`, `Description`, `Website`, and `Callback URL` 
fields like example provided below. For `Callback URL` make sure to copy/paste 
the following: `http://127.0.0.1:1410`
3. Once the app is created, copy and paste `consumer key` and `consumer secret key` 
into the `get_token()` function (see demo below).

|                 |                                         |
|-----------------|-----------------------------------------|
| Name            | tfse_app                                |
| Description     | Twitter follows and selective exposure  |
| Website         | http://twitter.com/kearneymw            |
| Callback URL    | http://127.0.0.1:1410                   |

## demo

```{r}
### install and load tfse
devtools::install_github("mkearney/tfse")
library(tfse)

### first time running get_token() function should open web browser 
### select yes/agree to authorize each app
tokens <- c(get_token(app = "app1_name",
                      consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
                      consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
            get_token(app = "app2_name",
                      consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
                      consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
            get_token(app = "app3_name",
                      consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
                      consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))

### save tokens for easy use in future sessions
### take note of working directory
save(tokens, file = "tokens")
getwd()

### to load tokens in new session, uncomment the next line
#load("tokens") # specify whole path if changing working directory

### use multiple tokens to return 225,000 followers 
### (75,000 followers for each token)
hrc_followers <- get_followers_max("HillaryClinton", tokens)
hrc_followers[1:10]

### use multiple tokens to lookup user data for 54,000 users
### (18,000 users for each token)
users <- get_lookup_max(hrc_followers, tokens)
users

### use multiple tokens to return 45 user networks (aka friends)
### (15 user networks for each token)
user_ids <- sample(users$user_id[users$protected == FALSE], 45)
user_networks <- get_friends_max(user_ids, tokens)
user_networks
```

## about me
- i'm a phd candidate in political communication at the university of kansas
- my dissertation examines selective exposure and political polarization on twitter

## contact
- email me at mkearney@ku.edu
