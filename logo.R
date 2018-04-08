## load ggplot2
library(ggplot2)

## colors
red <- "#ee5555"
blue <- "#3366ff"
gold <- "goldenrod"
gray <- "#bbbbbb"

## top (triangle) of warehouse
top <- data.frame(
  x = c(-.45, -.45,   0,  .45,  .45,
         .40,  .40,   0, -.40, -.40, -.45),
  y = c( .45,  .52, .75,  .52,  .45,
         .45,  .47, .66,  .47,  .45,  .45))
## main warehouse square
body <- data.frame(
  x = c(-.40, -.40,  .40,  .40, 0, -.40, -.40),
  y = c( .47, .00,  .00,  .47, .66, .47,  .45)
)
## left-most window
w1 <- data.frame(
  x = c(-.25, -.25, -.15, -.15, -.25),
  y = c( .06,  .45,  .45,  .06,  .06)
)
## middle window
w2 <- data.frame(
  x = c(-.05, -.05,  .05,  .05, -.05),
  y = c( .06,  .30,  .30,  .06,  .06)
)
## right-most window
w3 <- data.frame(
  x = c( .15,  .15,  .25,  .25,  .15),
  y = c( .06,  .37,  .37,  .06,  .06)
)
## hexagon (background) data
dfhex <- hexdf()

## create hex logo plot object
p <- ggplot(dfhex, aes(x, y)) +
  geom_polygon(fill = "#BED0DF", colour = "black", size = 2) +
  geom_polygon(data = top, fill = "#555555")  +
  geom_polygon(data = body, fill = "#f0f0f0") +
  geom_polygon(data = w1, fill = blue) +
  geom_polygon(data = w2, fill = red) +
  geom_polygon(data = w3, fill = gold) +
  geom_path(size = 1.5, lineend = "square") +
  geom_path(data = body[-c(5:7), ], size = 1.75) +
  geom_path(data = w1, size = 1.5, lineend = "square") +
  geom_path(data = w2, size = 1.5, lineend = "square") +
  geom_path(data = w3, size = 1.5, lineend = "square") +
  coord_cartesian(xlim = range(dfhex$x), ylim = range(dfhex$y)) +
  annotate("text", 0, -.3, label = "tfse", size = 38, family = "FreeMono",
    fontface = "bold", colour = "black") +
  theme_void() +
  labs(x = NULL, y = NULL, title = NULL) +
  theme(plot.margin = margin(-1.55, -1.34, -1.71, -1.52, "line"),
    plot.background = element_rect(fill = "transparent"))

## quick view in device window
p

## save, making sure background is saved as transparent
ggsave("man/figures/logo.png", p,
  width = (1.73/2.0) * 7, height = 7,
  units = "in", bg = "transparent")
