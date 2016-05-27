#' collect_diss_data
#'
#' @param wave Wave number
#' @return data Saved wave data and updated running data set
#' @export
collect_diss_data <- function(wave){
  # load tokens
  load_tokens()

  # load user ids
  data("ids")

  # get friend networks
  weekly <- get_wave_data(ids, tokens)

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
