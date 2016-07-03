# tfse
tfse = **t**witter **f**ollows & **s**elective **e**xposure

## about tfse
- R package consisting of functions designed to interact with twitter's API
- I realize there's already an R package ([twitteR](https://github.com/geoffjentry/twitteR))
that does a lot of the same stuff, but I wanted to get a sense for the inner-workings
so I decided to write a package from the ground up (and b/c dissertations are 
supposed to be hard, right?)

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
```

### get_token()
```{r}
### first time running get_token() function should open web browser 
### select yes/agree to authorize each app
### replace 'appX_name' with name of your application (see: 'obtaining access tokens')
### replace 'xxxx' with alpha-numeric keys associated with your apps
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
```

### get_followers()
```{r}
### use multiple tokens to return 225,000 followers 
### (75,000 followers for each token)
hrc_followers <- get_followers_max("HillaryClinton", tokens)
hrc_followers[1:10]
```

### get_lookup()
```{r}
### lookup user data for thousands of users
### (up to 18,000 users for each token)
users <- get_lookup_max(hrc_followers[1:5000], tokens)
users
```

### get_friends()
```{r}
### use multiple tokens to return 45 user networks (aka friends)
### (15 user networks for each token)
user_ids <- sample(users$user_id[users$protected == FALSE], 45)
user_networks <- get_friends_max(user_ids, tokens)
user_networks
```

### search_tweets()
```{r}
### search tweets via REST API
### (number of tweets returned varies; broader searchers are best)
elect16 <- search_tweets(q = "#election2016", 
                         count = 2000,
                         token = tokens[[1]])
elect16
```

### filter_stream()
```{r}
### read tweets in stream - collect tweets for 5 minutes (timeout = 300s)
### (access lots and lots of tweets this way)
e16stream <- filter_stream(stream = "realdonaldtrump,hillaryclinton,hillary,trump,
                                     election2016,imwithher,makeamericagreatagain",
                                    file_name = "e16stream",
                                    timeout = 300,
                                    token = tokens[[1]])
e16stream
```

## about me
- i'm a phd candidate in political communication at the university of kansas
- i created this package as part of my dissertation on selective exposure and 
political polarization on twitter
- my dissertation exists in a private repository, but when it's done my goal is to 
organize and make everything available via github

## contact
- email me at mkearney@ku.edu
