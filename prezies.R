library(readr)
library(rtweet)

files <- list.files("/Users/mwk/r/tfse/study/data")
files <- files[7:19]
files <- paste0("/Users/mwk/r/tfse/study/data/", files)
waves <- lapply(files, read_rds)

usrs <- read_csv("/Users/mwk/r/tfse/study/data/users_3000")
g <- usrs$group
prestickets <- lookup_users(c("realdonaldtrump", "mike_pence",
  "hillaryclinton", "timkaine"))

cans <- data.frame(user_id = prestickets$user_id,
  screen_name = prestickets$screen_name,
  party = c("R", "R", "D", "D"),
  position = c("P", "VP", "P", "VP"))


check_prezies <- function(x) {
  x <- unlist(x, use.names = FALSE)
  if (is.null(x)) return(matrix(NA, 1, 4))
  if (length(x) < 2) {
    return(matrix(NA, 1, 4))
  }
  trump <- as.double(25073877) %in% as.double(x)
  pence <- as.double(22203756) %in% as.double(x)
  clinton <- as.double(1339835893) %in% as.double(x)
  kaine <- as.double(172858784) %in% as.double(x)
  matrix(c(trump, pence, clinton, kaine), 1, 4)
}

process_wave <- function(x, g) {
  o <- lapply(x[["friends"]], check_prezies)
  o <- do.call("rbind", o)
  x <- x[, -3]
  x$group <- g
  o <- cbind(x, o, stringsAsFactors = FALSE)
  names(o) <- c("user_id", "date", "group",
    "trump", "pence", "clinton", "kaine")
  o
}
foo2 <- function(x) length(unlist(x, use.names = FALSE))
foo <- function(x) {
  unlist(lapply(x[["friends"]], foo2),
    use.names = FALSE)
}
ffoo <- lapply(waves, foo)
lapply(ffoo, mean, na.rm = TRUE)
waves <- waves[c(1:8, 10, 13)]

get_date <- function(x) unique(x[["date"]])
lapply(waves, get_date)
waves[[4]]$date <- as.Date("2016-09-28")
waves[[5]]$date <- as.Date("2016-10-01")
waves[[6]]$date <- as.Date("2016-10-02")
waves[[7]]$date <- as.Date("2016-10-03")
waves[[8]]$date <- as.Date("2016-10-04")
waves[[9]]$date <- as.Date("2016-10-07")
waves[[10]]$date <- as.Date("2016-10-08")

wdf <- lapply(waves,
  function(x) process_wave(x, g = g))
df <- do.call("rbind", wdf)

df <- tfse::tbl_df(df)
df

library(dplyr)
p <- df %>% group_by(group, date) %>%
  summarise(
    trump = sum(trump, na.rm = TRUE),
    pence = sum(pence, na.rm = TRUE),
    clinton = sum(clinton, na.rm = TRUE),
    kaine = sum(kaine, na.rm = TRUE))

p <- reshape2::melt(p, id.vars = c("group", "date"),
  variable.name = "candidate", value.name = "followers")
p <- tfse::tbl_df(p)
library(ggplot2)
p$party <- NA_character_
p$party[p$candidate %in% c("trump", "pence")] <- "R"
p$party[!p$candidate %in% c("clinton", "pence")] <- "D"
p$pos <- NA_character_
p$pos[!p$candidate %in% c("trump", "clinton")] <- "P"
p$pos[!p$candidate %in% c("kaine", "pence")] <- "VP"

png("diss_dat.png", 700, 500)
ggplot(p, aes(x = date, y = followers,
  color = candidate, linetype = pos)) +
  geom_line(size = 1, alpha = 1) + theme_minimal() +
  facet_grid( ~ group) +
  theme(legend.position = "right",
    text = element_text(size = 22, family = "Roboto")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c(
    "#dd3333ff",
    "#ee8888ee",
    "#3366ffff",
    "#77aaddee"))
dev.off()
  ?scale_linetype_manual
?facet_grid
data.frame(subset(p, candidate == "pence"))
tail(p)
