#' get_wave
#'
#' @param ids Vector of user ids
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return friends List of user ids each user follows.
#' @export
get_wave <- function(ids, tokens) {
  start <- 1

  while (start < length(ids)) {
    o <- get_friends_max(ids, tokens, start)

    if (exists("out")) {
      out <- rbind(out, o)
    } else {
      out <- o
    }

    start <- nrow(out) + 1

    if (start < 2881) {
      Sys.sleep(15*60)
    } else {
      break
    }
  }
  return(out)
}


#' get_wave_data
#'
#' @param wave Wave number
#' @return data Saved wave data and updated running data set
#' @export
get_wave_data <- function(wave) {
  # load tokens
  load_tokens()

  # load user ids
  data("ids")

  # get friend networks
  weekly <- get_wave(ids, tokens)

  # save wave
  save(weekly, file = paste0("d", wave, ".rda"))

  # load data set
  data("d")

  # specify friends wave #
  names(weekly)[names(weekly) == "friends"] <- paste0("friends_", wave)

  # merge old and new data
  d <- merge(d, weekly[, !names(weekly) %in% "date"])

  # save updated data
  save(d, file = "data/d.rda")

  # save backup data
  save(d, file = paste0("data/d", wave, ".backup.rda"))
}
