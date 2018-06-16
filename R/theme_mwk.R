
#' theme_mwk
#'
#' My ggplot2 theme
#'
#' @param base_size Base font size
#' @param base_family Font family.
#' @param dark Primary forefront color.
#' @param light Primary background color.
#' @param gray Gray color.
#' @return My ggplot2 theme (similar to theme_minimal/theme_bw)
#' @export
theme_mwk <- function(base_size = 12,
                      base_family = "Roboto Condensed",
                      dark = "#24292e",
                      light = "#f7f7f7",
                      gray = "#eaeaea") {
  if (!requireNamespace("ggplot2", quietly = FALSE)) {
    stop("must install ggplot2 pkg", call. = FALSE)
  }
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(fill = light),
      legend.background = ggplot2::element_rect(
        fill = light, colour = light),
      legend.text = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(.75)),
      plot.background = ggplot2::element_rect(
        fill = light, colour = light),
      plot.caption = ggplot2::element_text(
        face = "italic", size = ggplot2::rel(.75)),
      panel.background = ggplot2::element_rect(
        fill = light, colour = light),
      panel.border = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        face = "bold", colour = dark, size = ggplot2::rel(1.2)),
      plot.subtitle = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(.85)),
      text = ggplot2::element_text(colour = dark),
      strip.background = ggplot2::element_rect(
        fill = gray, colour = dark),
      axis.text = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(.65)),
      axis.title = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(.85), hjust = .95, face = "italic"),
      panel.grid.major = ggplot2::element_line(
        color = "#111111", size = ggplot2::rel(.040)),
      panel.grid.minor = ggplot2::element_line(
        color = "#111111", size = ggplot2::rel(.020)),
      axis.ticks = ggplot2::element_blank()
    )
}


#' theme_chartist
#'
#' A chartist inspired ggplot2 theme
#'
#' @param base_size Base font size
#' @param base_family Font family.
#' @param dark Primary forefront color.
#' @param light Primary background color.
#' @param gray Gray color.
#' @return My ggplot2 theme (similar to theme_minimal/theme_bw)
#' @export
theme_chartist <- function(base_size = 12,
                      base_family = "Roboto Condensed") {
  if (!requireNamespace("ggplot2", quietly = FALSE)) {
    stop("must install ggplot2 pkg", call. = FALSE)
  }
  light <- rgb(234, 218, 196, maxColorValue = 255)
  gray <- light
  dark <- rgb(91, 68, 33, maxColorValue = 255)
  dark <- rgb(0, 0, 0, .4)
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      legend.position = "none",
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(fill = light),
      legend.background = ggplot2::element_rect(
        fill = light, colour = light),
      legend.text = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(.75)),
      plot.background = ggplot2::element_rect(
        fill = light, colour = light),
      plot.caption = ggplot2::element_text(
        face = "italic", size = ggplot2::rel(.75)),
      panel.background = ggplot2::element_rect(
        fill = light, colour = light),
      panel.border = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        face = "bold", colour = dark, size = ggplot2::rel(1.2)),
      plot.subtitle = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(1.0)),
      text = ggplot2::element_text(colour = dark),
      strip.background = ggplot2::element_rect(
        fill = gray, colour = dark),
      axis.text = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(1.0)),
      axis.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = dark, linetype = 3, size = ggplot2::rel(.65)),
      panel.grid.minor = ggplot2::element_line(
        color = dark, linetype = 3, size = ggplot2::rel(.65)),
      axis.ticks = ggplot2::element_blank()
    )
}



orange <- rgb(209, 121, 5, maxColorValue = 255)
salmon <- rgb(240, 91, 79, maxColorValue = 255)
red    <- rgb(215, 2, 6, maxColorValue = 255)
yellow <- rgb(244, 198, 61, maxColorValue = 255)

