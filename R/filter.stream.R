#' filter_stream
#' Returns public statuses that match one or more filter predicates. Multiple parameters may be specified which allows most clients to use a single connection to the Streaming API. Both GET and POST requests are supported, but GET requests with too many parameters may cause the request to be rejected for excessive URL length. Use a POST request to avoid long URLs.
#'
#' The track, follow, and locations fields should be considered to be combined with an OR operator. track=foo&follow=1234 returns Tweets matching “foo” OR created by user 1234.
#'
#' The default access level allows up to 400 track keywords, 5,000 follow userids and 25 0.1-360 degree location boxes. If you need elevated access to the Streaming API, you can contact Gnip.
#' @seealso \url{https://stream.twitter.com/1.1/statuses/filter.json}
#' @return json object
#' @param stream either follower A comma separated list of user IDs, indicating the users to return statuses for in the stream. See follow for more information. track Keywords to track. Phrases of keywords are specified by a comma-separated list. See track for more information. Or locations Specifies a set of bounding boxes to track. See locations for more information.
#' @param delimited optional Specifies whether messages should be length-delimited. See delimited for more information.
#' @param stall_warnings optional Specifies whether stall warnings should be delivered. See stall_warnings for more information.
#' @param token OAuth token (1.0 or 2.0)
#' @export
filter_stream <- function(stream, delimited = FALSE, stall_warnings = FALSE, token, timeout = 120, file_name) {
  file.create(paste0(file_name, ".stream.json"))

  if (missing(stream)) stop("Must include stream search call.")

  stream <- unlist(trimws(unlist(strsplit(stream, ","))))

  if (!is_screen_name(stream[[1]])) {
    if (is.integer(stream[[1]])) {
      params <- paste0("follow=", track_encode(stream))
    } else {
      params <- paste0("locations=", track_encode(stream))
    }
  } else {
    params <- paste0("track=", track_encode(stream))
  }
  if (missing(file_name)) file_name <- tempfile()

  stream_df <- TWIT(query = "statuses/filter",
                    parameters = params,
                    token = token,
                    timeout = timeout,
                    file_name = paste0(file_name, ".stream.json"))

  stream_df
}
