#' Generate a hexagon shaped data frame
#'
#' Returns xy coordinates for hexagon shape using hex sticker dimensions
#'
#' @param size The size of hexagon (one half of the width).
#' @param mid The xy midpoint, defaulting to c(0, 0)
#' @param angle The angle at which to rotate the hexagon. The default of 90 results in the top and bottoms of the hexagon coming to a point
#' @return A tibble data frame
#' @examples
#' ## plot hexagon using function defaults
#' hex1 <- hexdf()
#' with(hex1, plot(x, y, type = "l"))
#'
#' ## create larger hexagon to the lower right
#' hex2 <- hexdf(2, mid = c(2.5, -2))
#'
#' ## plot both hex objects
#' ggplot2::ggplot(hex1, ggplot2::aes(x, y)) +
#'   ggplot2::geom_polygon(fill = "blue") +
#'   ggplot2::geom_polygon(data = hex2, fill = "red")
#'
#' ## create hex pkg sticker
#'
#' ## some points (dots)
#' pts <- data.frame(
#'   x = runif(30, -.5, .5),
#'   y = runif(30, -.05, .5))
#'
#' ## use ggplot to create sticker
#' ggplot2::ggplot(hex1,ggplot2::aes(x, y)) +
#'   ggplot2::geom_polygon(fill = "#2244aa", colour = "#001050", size = 4) +
#'   ggplot2::geom_point(data = pts, shape = 21, size = 10,
#'     colour = "#001050", fill = "white") +
#'   ggplot2::theme_void() +
#'   ggplot2::annotate("text", 0, -.4, label = "dots", colour = "white",  size = 42,
#'     fontface = "bold")
#' ## pass to ggplot
#' @export
hexdf <- function(size = 1, mid = c(0, 0), angle = 90) {
  width <- size * 1
  height <- size * 0.865
  xy <- shape:::getellipse(width, height, mid = mid, dr = 2 * pi/6)
  tibble::as_tibble(structure(as.data.frame(shape::rotatexy(xy, angle = angle, mid = mid)),
    class = "data.frame", names = c("x", "y")), validate = FALSE)
}
