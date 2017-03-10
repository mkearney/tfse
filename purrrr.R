devtools::load_all("/Users/mwk/r/tfse")
devtools::document("/Users/mwk/r/tfse")
devtools::install("/Users/mwk/r/tfse")
library(rtweet)



tw <- search_tweets("trump clinton", n = 300, parse = FALSE)
tw[[2]] %>% lapply(., tidy_twt)
tw[[2]] %>% .[["statuses"]] %>% .[["retweeted"]]
tw[[3]] %>% tidy_twt %>% head
tw[[4]] %>% tidy_twt %>% head
tw %>% lapply(., tidy_twt) %>% lapply(., names)
tw[[4]] %>% .[["statuses"]] %>% .[["coordinates"]]%>% is.na
tw[[1]] %>% .[["statuses"]] %>% str1
tw[[2]] %>% .[["statuses"]] %>% str1(vec.len = 0)
tw %>% ply_twt %>% str1
tw %>% ply_usr %>% str1

?str
tidy_twt(tw[[1]][["statuses"]])
?rbind.data.frame
ply_twt <- function(x) {
  x <- lapply(x, tidy_twt)
  do.call("rbind", x)
}

ply_usr <- function(x) {
  x <- lapply(x, tidy_usr)
  do.call("rbind", x)
}

str1 <- function(., ...) str(., 1, vec.len = 0, ...)
str2 <- function(., ...) str(., 2, vec.len = 0, ...)

return_vectors <- function(x) {
  x %>% .[!sapply(., is.recursive)] %>% data.frame(.,
    stringsAsFactors = FALSE)
}
return_recursives <- function(x) {
  x %>% .[sapply(., is.recursive)]
}
is_empt <- function(x) {
  any(is.null(x), isTRUE(length(x) == 0))
}
uNAlist <- function(x) {
  unlist(
  lapply(x, function(.) ifelse (is_empt(.), NA, paste(., collapse = " "))),
  recursive = FALSE)
}

ply_extract <- function(., x, f) {
  if (!missing(x)) {
    . <- lapply(., function(.) .[[x]])
  }
  if (!missing(f)) {
    . <- lapply(., f)
  }
  .
}
dots <- function(...) {
  eval(substitute(alist(...)))
}

if_i <- function(.) {
  if (is.data.frame(eval(.))) {
    return(eval(.))
  }
  if (is.list(eval(.))) {
    return(I(eval(.)))
  } else {
    return(eval(.))
  }
}
data.frame_ <- function(...) {
  df <- data.frame(
    lapply(list(...), if_i),
    stringsAsFactors = FALSE)
  df
}
fill_na <- function(.) {
  lapply(., function(i) {
    if (is_empt(i)) {
      return(NA)
    } else {
      return(i)
    }
  })
}

tidy_twt <- function(d) {
  if (is.null(names(d))) {
    d <- d %>% .[[1]]
  }
  if ("statuses" %in% names(d)) {
    d <- d %>% .[["statuses"]]
  }
  vecs_df <- return_vectors(d)
  recur_df <- return_recursives(d)
  vecs_df <- vecs_df[!names(vecs_df) %in% c(names(recur_df),
    "hashtags", "expanded_url", "result_type", "coordinates",
    "retweet_status_id", "quoted_status_id", "place")]

  hashtags <- recur_df %>% .[["entities"]] %>% .[["hashtags"]] %>%
    ply_extract(x = "text", as.character) %>% fill_na

  expanded_url <- recur_df %>% .[["entities"]] %>% .[["urls"]] %>%
    ply_extract(x = "expanded_url", as.character) %>% fill_na

  mentions_screen_name <- recur_df %>% .[["entities"]] %>%
    .[["user_mentions"]] %>%
    ply_extract(x = "screen_name", as.character) %>% fill_na

  mentions_user_id <- recur_df %>% .[["entities"]] %>%
  	.[["user_mentions"]] %>%
    ply_extract(x = "id_str", as.character) %>% fill_na

  ##------------------------------------------------------------------##
  ##                           metadata                               ##
  ##------------------------------------------------------------------##
  result_type <- recur_df %>% .[["metadata"]] %>%
  	.[["result_type"]] %>% as.character

  ##------------------------------------------------------------------##
  ##                   PLACE VARIABLES START HERE                     ##
  ##------------------------------------------------------------------##
  place_vecs_df <- recur_df %>% .[["place"]] %>% return_vectors
  if (identical(length(place_vecs_df), 0L)) {

    place_vecs_df <- data.frame(
      place_url = rep(NA_character_, nrow(vecs_df)),
      place_type = rep(NA_character_, nrow(vecs_df)),
      place_name = rep(NA_character_, nrow(vecs_df)),
      place_country_code = rep(NA_character_, nrow(vecs_df)),
      place_country = rep(NA_character_, nrow(vecs_df)))
    coordinates <- lapply(rep(NA_real_, nrow(vecs_df)),
      function(.) matrix(., ncol = 8))
  } else {
    place_vecs_df <- place_vecs_df[names(place_vecs_df) %in% c(
    "url", "place_type", "name",
    "country_code", "country")]
    names(place_vecs_df) <- paste0("place_",
      gsub("place_", "", names(place_vecs_df)))
    coordinates <- recur_df %>% .[["place"]] %>%
    .[["bounding_box"]] %>% .[["coordinates"]] %>%
    ply_extract(., f = as.numeric) %>% lapply(., matrix, 1, 8)
  }

  if (any(identical(length(coordinates), 0L),
    all(is.na(coordinates)))) {

    coordinates <- matrix(NA, nrow(vecs_df), 8)
  }

  ##------------------------------------------------------------------##
  ##                        retweet_status_id                         ##
  ##------------------------------------------------------------------##
  retweet_status_id <- recur_df %>% .[["retweeted_status"]] %>%
    .[["id_str"]] %>% as.character
  if (identical(length(retweet_status_id), 0L)) {
    retweet_status_id <- rep(NA_character_, nrow(vecs_df))
  }
  ##------------------------------------------------------------------##
  ##                        quoted_status_id                          ##
  ##------------------------------------------------------------------##
  quoted_status_id <- recur_df %>% .[["quoted_status"]] %>%
    .[["id_str"]] %>% as.character
  if (identical(length(quoted_status_id), 0L)) {
    quoted_status_id <- rep(NA_character_, nrow(vecs_df))
  }
  ##------------------------------------------------------------------##
  ##                     USER ID and SCREN NAME                       ##
  ##------------------------------------------------------------------##
  user_id <- recur_df %>% .[["user"]] %>%
    .[["id_str"]] %>% as.character

  screen_name <- recur_df %>% .[["user"]] %>%
    .[["screen_name"]] %>% as.character

  data.frame_(
    vecs_df,
    user_id = user_id,
    screen_name = screen_name,
    place_vecs_df,
    coordinates = coordinates,
    retweet_status_id = retweet_status_id,
    quoted_status_id = quoted_status_id)
}


tidy_usr <- function(x) {
  if (is.null(names(x))) {
    x <- x[[1]]
  }
  if ("statuses" %in% names(x)) {
    x <- x %>% .[["statuses"]] %>% .[["user"]]
  }

  vecs_df <- x %>% return_vectors
  x_recs <- x %>% return_recursives

  user_url <- x_recs %>% .[["entities"]] %>% .[["url"]] %>%
    .[["urls"]] %>% ply_extract(x = "expanded_url") %>%
    as.character %>% fill_na

  description_urls <- x_recs %>% .[["entities"]] %>%
    .[["description"]] %>% .[["urls"]] %>%
    ply_extract(x = "expanded_url", f = as.character) %>%
    fill_na

  data.frame_(
    vecs_df,
    user_url = user_url,
    description_urls = description_urls)
}
