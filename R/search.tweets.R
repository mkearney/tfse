#' search_tweets
#'
#' Returns a collection of relevant Tweets matching a specified query. Please
#' note that Twitter’s search service and, by extension, the Search API is not
#' meant to be an exhaustive source of Tweets. Not all Tweets will be indexed or
#' made available via the search interface. In API v1.1, the response format of
#' the Search API has been improved to return Tweet objects more similar to the
#' objects you’ll find across the REST API and platform. However, perspectival
#' attributes (fields that pertain to the perspective of the authenticating
#' user) are not currently supported on this endpoint. To learn how to use
#' Twitter Search effectively, consult our guide to Using the Twitter Search
#' API. See Working with Timelines to learn best practices for navigating
#' results by since_id and max_id.
#'
#' @param q required, A UTF-8, URL-encoded search query of 500 characters
#'   maximum, including operators. Queries may additionally be limited by
#'   complexity.
#' @param geocode optional, Returns tweets by users located within a given
#'   radius of the given latitude/longitude. The location is preferentially
#'   taking from the Geotagging API, but will fall back to their Twitter
#'   profile. The parameter value is specified by “latitude,longitude,radius”,
#'   where radius units must be specified as either “mi” (miles) or “km”
#'   (kilometers). Note that you cannot use the near operator via the API to
#'   geocode arbitrary locations; however you can use this geocode parameter to
#'   search near geocodes directly. A maximum of 1,000 distinct “sub-regions”
#'   will be considered when using the radius modifier.
#' @param lang optional, Restricts tweets to the given language, given by an ISO
#'   639-1 code. Language detection is best-effort.
#' @param locale optional, Specify the language of the query you are sending
#'   (only ja is currently effective). This is intended for language-specific
#'   consumers and the default should work in the majority of cases.
#' @param result_type optional, Specifies what type of search results you would
#'   prefer to receive. The current default is “mixed.” Valid values include
#'   \code{"mixed"} to include both popular and real time results in the
#'   response, \code{"recent"} to return only the most recent results in the
#'   response, and\code{"popular"} to return only the most popular results in
#'   the response.
#' @param count optional, The number of tweets to return per page, up to a
#'   maximum of 100. Defaults to 15. This was formerly the “rpp” parameter in
#'   the old Search API.
#' @param until optional, Returns tweets created before the given date. Date
#'   should be formatted as YYYY-MM-DD. Keep in mind that the search index has a
#'   7-day limit. In other words, no tweets will be found for a date older than
#'   one week. Example Values: \code{"2015-07-19"}.
#' @param since_id optional, Returns results with an ID greater than (that is,
#'   more recent than) the specified ID. There are limits to the number of
#'   Tweets which can be accessed through the API. If the limit of Tweets has
#'   occured since the since_id, the since_id will be forced to the oldest ID
#'   available.
#' @param max_id optional, Returns results with an ID less than (that is, older
#'   than) or equal to the specified ID.
#' @param include_entities optional, The entities node will be disincluded when
#'   set to false.
#' @param callback optional, If supplied, the response will use the JSONP format
#'   with a callback of the given name. The usefulness of this parameter is
#'   somewhat diminished by the requirement of authentication for requests to
#'   this endpoint. Example Values: \code{"processTweets"}
#' @param token OAuth token (1.0 or 2.0)
#' @seealso \url{https://api.twitter.com/1.1/search/tweets.json}
#' @return json object
#' @import dplyr
#' @export
search_tweets <- function(q, token, geocode = NULL, lang = NULL, locale = NULL,
                          result_type = "recent", count = 100, until = NULL,
                          since_id = NULL, max_id = NULL, include_entities = TRUE) {

  tweets_df <- data_frame()

  params <- paste0("result_type=", result_type, "&count=", count,
                   if (!is.null(geocode)) paste0("&geocode=", geocode),
                   if (!is.null(lang)) paste0("&lang=", lang),
                   if (!is.null(locale)) paste0("&locale=", locale),
                   if (!is.null(until)) paste0("&until=", until),
                   if (!is.null(since_id)) paste0("&since_id=", since_id),
                   if (!is.null(max_id)) paste0("&max_id=", max_id),
                   if (include_entities) paste0("&include_entities=true") else paste0("&include_entities=false"))

  params <- paste0("q=", URLencode(q, reserved = TRUE), "&", params)

  nrows <- 0
  while (nrows < count) {
    out <- try_catch(TWIT(query = "search/tweets",
                          parameters = params,
                          token = token))

    tweets_df <- bind_rows(tweets_df, data_frame_status(out$statuses))

    nrows <- nrow(tweets_df)

    if (length(out$search_metadata$next_results) == 0) break

    params <- sub("[?]", "", out$search_metadata$next_results)
  }
  tweets_df
}


#' top_tweet_words
#'
#' @param tweets_text character vector of tweets text
#' @param min minimum number of ocurrences to include in returned object
#' @param exclude_words other words to exclude
#' @return list object with top mentions and top words
#' @import dplyr
#' @export
top_tweet_words <- function(tweets_text, min = 3, exclude_words = NULL) {
  tweets_text <- tweets_text[!is.na(tweets_text)]
  tweets_text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_text)
  tweets_text <- gsub("@\\w+", "", tweets_text)
  tweets_text <- gsub("[[:punct:]]", "", tweets_text)
  tweets_text <- gsub("[[:digit:]]", "", tweets_text)
  tweets_text <- gsub("htt\\w+", "", tweets_text)
  tweets_text <- gsub("\\n", "", tweets_text)
  tweets_text <- trimws(tweets_text)
  tweets_text <- tolower(tweets_text)
  tweets_text <- unlist(lapply(tweets_text, function(x) unlist(strsplit(x, split = " "))))
  tweets_text <- tweets_text[!is.na(tweets_text)]
  stopwords <- c("a", "a's", "able", "about", "above", "according",
                 "accordingly", "across", "actually", "after", "afterwards",
                 "again", "against", "ain't", "all", "allow", "allows", "almost",
                 "alone", "along", "already", "also", "although", "always",
                 "am", "among", "amongst", "an", "and", "another", "any",
                 "anybody", "anyhow", "anyone", "anything", "anyway", "anyways",
                 "anywhere", "apart", "appear", "appreciate", "appropriate",
                 "are", "aren't", "around", "as", "aside", "ask", "asking",
                 "associated", "at", "available", "away", "awfully", "b",
                 "be", "became", "because", "become", "becomes", "becoming",
                 "been", "before", "beforehand", "behind", "being", "believe",
                 "below", "beside", "besides", "best", "better", "between",
                 "beyond", "both", "brief", "but", "by", "c", "c'mon", "c's",
                 "came", "can", "can't", "cannot", "cant", "cause", "causes",
                 "certain", "certainly", "changes", "clearly", "co", "com",
                 "come", "comes", "concerning", "consequently", "consider",
                 "considering", "contain", "containing", "contains", "corresponding",
                 "could", "couldn't", "course", "currently", "d", "definitely",
                 "described", "despite", "did", "didn't", "different", "do",
                 "does", "doesn't", "doing", "don't", "done", "down", "downwards",
                 "during", "e", "each", "edu", "eg", "eight", "either", "else",
                 "elsewhere", "enough", "entirely", "especially", "et", "etc",
                 "even", "ever", "every", "everybody", "everyone", "everything",
                 "everywhere", "ex", "exactly", "example", "except", "f",
                 "far", "few", "fifth", "first", "five", "followed", "following",
                 "follows", "for", "former", "formerly", "forth", "four",
                 "from", "further", "furthermore", "g", "get", "gets", "getting",
                 "given", "gives", "go", "goes", "going", "gone", "got", "gotten",
                 "greetings", "h", "had", "hadn't", "happens", "hardly", "has",
                 "hasn't", "have", "haven't", "having", "he", "he's", "hello",
                 "help", "hence", "her", "here", "here's", "hereafter", "hereby",
                 "herein", "hereupon", "hers", "herself", "hi", "him", "himself",
                 "his", "hither", "hopefully", "how", "howbeit", "however",
                 "i", "i'd", "i'll", "i'm", "i've", "ie", "if", "ignored",
                 "immediate", "in", "inasmuch", "inc", "indeed", "indicate",
                 "indicated", "indicates", "inner", "insofar", "instead",
                 "into", "inward", "is", "isn't", "it", "it'd", "it'll", "it's",
                 "its", "itself", "j", "just", "k", "keep", "keeps", "kept",
                 "know", "knows", "known", "l", "last", "lately", "later",
                 "latter", "latterly", "least", "less", "lest", "let", "let's",
                 "like", "liked", "likely", "little", "look", "looking", "looks",
                 "ltd", "m", "mainly", "many", "may", "maybe", "me", "mean",
                 "meanwhile", "merely", "might", "more", "moreover", "most",
                 "mostly", "much", "must", "my", "myself", "n", "name", "namely",
                 "nd", "near", "nearly", "necessary", "need", "needs", "neither",
                 "never", "nevertheless", "new", "next", "nine", "no", "nobody",
                 "non", "none", "noone", "nor", "normally", "not", "nothing",
                 "novel", "now", "nowhere", "o", "obviously", "of", "off",
                 "often", "oh", "ok", "okay", "old", "on", "once", "one",
                 "ones", "only", "onto", "or", "other", "others", "otherwise",
                 "ought", "our", "ours", "ourselves", "out", "outside", "over",
                 "overall", "own", "p", "particular", "particularly", "per",
                 "perhaps", "placed", "please", "plus", "possible", "presumably",
                 "probably", "provides", "q", "que", "quite", "qv", "r", "rather",
                 "rd", "re", "they'd", "they'll", "they're", "they've", "try",
                 "trying", "twice", "two", "u", "un", "under", "unfortunately",
                 "unless", "unlikely", "until", "unto", "up", "upon", "us",
                 "use", "used", "useful", "uses", "using", "usually", "uucp",
                 "v", "value", "various", "very", "via", "viz", "vs", "w",
                 "want", "wants", "was", "wasn't", "way", "we", "we'd", "we'll",
                 "we're", "we've", "welcome", "well", "went", "were", "weren't",
                 "what", "what's", "whatever", "when", "whence", "whenever",
                 "where", "where's", "whereafter", "whereas", "whereby", "wherein",
                 "whereupon", "wherever", "whether", "which", "while", "whither",
                 "who", "who's", "whoever", "whole", "whom", "whose", "why",
                 "will", "willing", "wish", "with", "within", "without", "won't",
                 "wonder", "would", "would", "wouldn't", "x", "y", "yes",
                 "yet", "you", "you'd", "you'll", "you're", "you've", "your",
                 "yours", "yourself", "yourselves", "z", "zero", "really",
                 "reasonably", "regarding", "regardless", "regards", "relatively",
                 "respectively", "right", "s", "said", "same", "saw", "say",
                 "saying", "says", "second", "secondly", "see", "seeing",
                 "seem", "seemed", "seeming", "seems", "seen", "self", "selves",
                 "sensible", "sent", "serious", "seriously", "seven", "several",
                 "shall", "she", "should", "shouldn't", "since", "six", "so",
                 "some", "somebody", "somehow", "someone", "something", "sometime",
                 "sometimes", "somewhat", "somewhere", "soon", "sorry", "specified",
                 "specify", "specifying", "still", "sub", "such", "sup", "sure",
                 "t", "t's", "take", "taken", "tell", "tends", "th", "than",
                 "thank", "thanks", "thanx", "that", "that's", "thats", "the",
                 "their", "theirs", "them", "themselves", "then", "thence",
                 "there", "there's", "thereafter", "thereby", "therefore",
                 "therein", "theres", "thereupon", "these", "they", "think",
                 "third", "this", "thorough", "thoroughly", "those", "though",
                 "three", "through", "throughout", "thru", "thus", "to", "together",
                 "too", "took", "toward", "towards", "tried", "tries", "truly")
  tweets_text <- tweets_text[tweets_text != " "]
  tweets_text <- tweets_text[tweets_text != ""]
  tweets_text <- tweets_text[!tweets_text %in% stopwords]
  tweets_text <- tweets_text[!tweets_text %in% exclude_words]
  tweets_text <- sort(table(tweets_text), decreasing = TRUE)

  tweets_text
}
