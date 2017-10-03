
#' theme_mwk
#' 
#' My ggplot2 theme
#' 
#' @param base_size Base font size
#' @param base_family Font family.
#' @param dark Primary forefront color.
#' @param light Primary background color.
#' @param gray Gray color.
#' @param ... Other args passed along to theme
#' @return My ggplot2 theme (similar to theme_minimal/theme_bw)
#' @importFrom ggplot2 theme_bw theme element_blank element_rect element_text element_line rel
#' @export
theme_mwk <- function(base_size = 15,
                      base_family = "Roboto",
                      dark = "#24292e",
                      light = "#ffffff",
                      gray = "#f0f0f0",
                      ...) {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(
        fill = light),
      legend.background = ggplot2::element_rect(
        fill = light, colour = light),
      legend.text = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(.7)),
      plot.background = ggplot2::element_rect(
        fill = light, colour = light),
      panel.background = ggplot2::element_rect(
        fill = light, colour = light),
      plot.title = ggplot2::element_text(
        face = "bold", colour = dark, size = ggplot2::rel(1.2)),
      plot.subtitle = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(1.1)),
      text = ggplot2::element_text(colour = dark),
      strip.background = ggplot2::element_rect(
        fill = gray, colour = dark),
      axis.text = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(.6)),
      axis.title = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(.9)),
      panel.grid.major = ggplot2::element_line(
        color = dark, size = ggplot2::rel(.050)),
      panel.grid.minor = ggplot2::element_line(
        color = dark, size = ggplot2::rel(.025)),
      ...
    )
}
