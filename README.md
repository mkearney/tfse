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

## about me
- i'm a phd candidate in political communication at the university of kansas
- i created this package as part of my dissertation on selective exposure and 
political polarization on twitter
- my dissertation exists in a private repository, but when it's done my goal is to 
organize and make everything available via github

## contact
- email me at mkearney@ku.edu

----------------------------------------
----------------------------------------


## demo
```{r}
library(tfse)
```

### get_token()
```{r, echo = TRUE, eval = FALSE}
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
```

```
##  [1] "/Users/mwk/r/tfse"
```

```{r, echo = TRUE, eval = FALSE}
### to load tokens in new session
load("/Users/mwk/r/tfse/tt/tokens") # specify whole path if changing working directory
```

### get_followers()
```{r, echo = TRUE, eval = FALSE}
### use multiple tokens to return 225,000 followers 
### (75,000 followers for each token)
hrc_followers <- get_followers_max("HillaryClinton", tokens[3:4])
hrc_followers[1:10]
```

```
##  [1] "750026524779216896" "750027440135757824" "750026662402723841"
##  [4] "750027119271538688" "4159860484"         "750027122052333570"
##  [7] "750026175792185344" "750024264582135809" "42521826"          
## [10] "750027371332378625"
```


### get_lookup()
```{r, echo = TRUE, eval = FALSE}
### lookup user data for thousands of users
### (up to 18,000 users for each token)
users <- get_lookup_max(hrc_followers[1:1000], tokens[1:2])
users
```

```
## Source: local data frame [1,000 x 20]
## 
##               user_id                name     screen_name  location
##                 <chr>               <chr>           <chr>     <chr>
## 1  750026524779216896       Reuben Cargil   King_Smiley24        NA
## 2  750027440135757824   Donetta monferini donettamonferi5        NA
## 3  750026662402723841         Adel Hameed    AdelHameed16        NA
## 4  750027119271538688                 Dan   Project_Gen_Y        NA
## 5          4159860484         Dana Bassem     bassem_dana        NA
## 6  750027122052333570         Jerry Kauyu      JerryKauyu        NA
## 7  750026175792185344 Андрій Лоточинський        SKY__BOW        NA
## 8  750024264582135809    فاروق مح ابو عمر yyuuiioopp20151        NA
## 9            42521826    Dana Jacklyn Roy     yummydanish neverland
## 10 750027371332378625     PAZ Y SEGURIDAD      co_trabajo        NA
## ..                ...                 ...             ...       ...
## Variables not shown: description <chr>, url <chr>,
##   entities_description_url <lgl>, entities_url <lgl>, protected <lgl>,
##   followers_count <int>, friends_count <int>, listed_count <int>,
##   created_at <date>, favourites_count <int>, utc_offset <int>, time_zone
##   <chr>, geo_enabled <lgl>, verified <lgl>, statuses_count <int>, lang
##   <chr>.
```

### get_friends()
```{r, echo = TRUE, eval = FALSE}
### use multiple tokens to return 45 user networks (aka friends)
### (15 user networks for each token)
user_ids <- sample(users$user_id[users$protected == FALSE], 45)
user_networks <- get_friends_max(user_ids, tokens[6:8])
```
## ***

```{r, echo = TRUE, eval = FALSE}
user_networks[[1]][1:2]
```

```
## [1] "71201743"   "62513246"
```

```{r, echo = TRUE, eval = FALSE}
user_networks[[2]][1:2]
```

```
## [1] "3739058774" "9300262"
```

```{r, echo = TRUE, eval = FALSE}
user_networks[[3]][1:2]
```

```
## [1] "1339835893" "1536791610"
```

### search_tweets()
```{r, echo = TRUE, eval = FALSE}
### search tweets via REST API
### (number of tweets returned varies; broader searchers are best)
elect16 <- search_tweets(q = "election2016", 
                         count = 500,
                         token = tokens[[2]])
elect16
```

```
## Source: local data frame [900 x 116]
## 
##             status_id
##                 <chr>
## 1  750027959646453760
## 2  750027943057977344
## 3  750027784836227072
## 4  750027704158879744
## 5  750027639931502592
## 6  750027345151557632
## 7  750027283990089728
## 8  750027096173449216
## 9  750027093384294401
## 10 750027057745129472
## ..                ...
## Variables not shown: text <chr>, truncated <chr>, result_type <chr>,
##   created_at <date>, source <chr>, in_reply_to_status_id <chr>,
##   in_reply_to_user_id <chr>, in_reply_to_screen_name <chr>,
##   is_quote_status <chr>, retweet_count <chr>, favorite_count <chr>,
##   favorited <chr>, retweeted <chr>, lang <chr>, quoted_status_id <chr>,
##   place_id <chr>, place_url <chr>, place_type <chr>, place_name <chr>,
##   place_full_name <chr>, place_country_code <chr>, place_country <chr>,
##   place_long1 <list>, place_long2 <list>, place_long3 <list>, place_long4
##   <list>, place_lat1 <list>, place_lat2 <list>, place_lat3 <list>,
##   place_lat4 <list>, entities_hashtag <list>,
##   entities_user_mentions_user_id <list>,
##   entities_user_mentions_screen_name <list>, entities_user_mentions_name
##   <list>, entities_urls_expanded <list>, entities_media_id <list>,
##   entities_media_source_status_id <list>, entities_media_source_user_id
##   <list>, entities_media_expanded_url <list>, user_id <chr>, name <chr>,
##   screen_name <chr>, location <chr>, description <chr>, url <chr>,
##   entities_description_url <list>, entities_url <list>, protected <lgl>,
##   followers_count <int>, friends_count <int>, listed_count <int>,
##   favourites_count <int>, utc_offset <int>, time_zone <chr>, geo_enabled
##   <lgl>, verified <lgl>, statuses_count <int>, retweet_status_id <chr>,
##   retweet_text <chr>, retweet_truncated <chr>, retweet_result_type <chr>,
##   retweet_created_at <date>, retweet_source <chr>,
##   retweet_in_reply_to_status_id <chr>, retweet_in_reply_to_user_id <chr>,
##   retweet_in_reply_to_screen_name <chr>, retweet_is_quote_status <chr>,
##   retweet_retweet_count <chr>, retweet_favorite_count <chr>,
##   retweet_favorited <chr>, retweet_retweeted <chr>, retweet_lang <chr>,
##   retweet_quoted_status_id <chr>, retweet_user_id <chr>, retweet_name
##   <chr>, retweet_screen_name <chr>, retweet_location <chr>,
##   retweet_description <chr>, retweet_url <chr>,
##   retweet_entities_description_url <list>, retweet_entities_url <list>,
##   retweet_protected <lgl>, retweet_followers_count <int>,
##   retweet_friends_count <int>, retweet_listed_count <int>,
##   retweet_favourites_count <int>, retweet_utc_offset <int>,
##   retweet_time_zone <chr>, retweet_geo_enabled <lgl>, retweet_verified
##   <lgl>, retweet_statuses_count <int>, retweet_place_id <chr>,
##   retweet_place_url <chr>, retweet_place_type <chr>, retweet_place_name
##   <chr>, retweet_place_full_name <chr>, retweet_place_country_code <chr>,
##   retweet_place_country <chr>, retweet_place_long1 <list>,
##   retweet_place_long2 <list>, and 15 more <...>.
```

### filter_stream()
```{r, echo = TRUE, eval = FALSE}
### read tweets in stream - collect tweets for 5 minutes (timeout = 300s)
### the code below returned a data frame with 3963 rows 
e16stream <- filter_stream(
  stream = "realdonaldtrump,hillaryclinton,hillary,trump,
            election2016,imwithher,makeamericagreatagain",
  timeout = 10,
  token = tokens[[2]])
e16stream
```

```
## opening file input connection.
## 
 Found 158 records...
 Imported 158 records. Simplifying into dataframe...
## closing file input connection.
e16stream
## Source: local data frame [158 x 116]
## 
##             status_id
##                 <chr>
## 1  750028110972592128
## 2  750028111413071872
## 3  750028111845163008
## 4  750028112608370688
## 5  750028113015349248
## 6  750028113225056257
## 7  750028113158045696
## 8  750028112633602048
## 9  750028115175440384
## 10 750028115309686789
## ..                ...
## Variables not shown: text <chr>, truncated <chr>, result_type <chr>,
##   created_at <date>, source <chr>, in_reply_to_status_id <chr>,
##   in_reply_to_user_id <chr>, in_reply_to_screen_name <chr>,
##   is_quote_status <chr>, retweet_count <chr>, favorite_count <chr>,
##   favorited <chr>, retweeted <chr>, lang <chr>, quoted_status_id <chr>,
##   place_id <chr>, place_url <chr>, place_type <chr>, place_name <chr>,
##   place_full_name <chr>, place_country_code <chr>, place_country <chr>,
##   place_long1 <list>, place_long2 <list>, place_long3 <list>, place_long4
##   <list>, place_lat1 <list>, place_lat2 <list>, place_lat3 <list>,
##   place_lat4 <list>, entities_hashtag <list>,
##   entities_user_mentions_user_id <list>,
##   entities_user_mentions_screen_name <list>, entities_user_mentions_name
##   <list>, entities_urls_expanded <list>, entities_media_id <list>,
##   entities_media_source_status_id <list>, entities_media_source_user_id
##   <list>, entities_media_expanded_url <list>, user_id <chr>, name <chr>,
##   screen_name <chr>, location <chr>, description <chr>, url <chr>,
##   entities_description_url <lgl>, entities_url <lgl>, protected <lgl>,
##   followers_count <int>, friends_count <int>, listed_count <int>,
##   favourites_count <int>, utc_offset <int>, time_zone <chr>, geo_enabled
##   <lgl>, verified <lgl>, statuses_count <int>, retweet_status_id <chr>,
##   retweet_text <chr>, retweet_truncated <chr>, retweet_result_type <chr>,
##   retweet_created_at <date>, retweet_source <chr>,
##   retweet_in_reply_to_status_id <chr>, retweet_in_reply_to_user_id <chr>,
##   retweet_in_reply_to_screen_name <chr>, retweet_is_quote_status <chr>,
##   retweet_retweet_count <chr>, retweet_favorite_count <chr>,
##   retweet_favorited <chr>, retweet_retweeted <chr>, retweet_lang <chr>,
##   retweet_quoted_status_id <chr>, retweet_user_id <chr>, retweet_name
##   <chr>, retweet_screen_name <chr>, retweet_location <chr>,
##   retweet_description <chr>, retweet_url <chr>,
##   retweet_entities_description_url <lgl>, retweet_entities_url <lgl>,
##   retweet_protected <lgl>, retweet_followers_count <int>,
##   retweet_friends_count <int>, retweet_listed_count <int>,
##   retweet_favourites_count <int>, retweet_utc_offset <int>,
##   retweet_time_zone <chr>, retweet_geo_enabled <lgl>, retweet_verified
##   <lgl>, retweet_statuses_count <int>, retweet_place_id <chr>,
##   retweet_place_url <chr>, retweet_place_type <chr>, retweet_place_name
##   <chr>, retweet_place_full_name <chr>, retweet_place_country_code <chr>,
##   retweet_place_country <chr>, retweet_place_long1 <list>,
##   retweet_place_long2 <list>, and 15 more <...>.
```

