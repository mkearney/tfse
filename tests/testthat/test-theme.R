context("test-theme")

test_that("theme_mwk", {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    expect_error({ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
        ggplot2::geom_point() + theme_mwk()})
  } else {
    p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
      ggplot2::geom_point() +
      theme_mwk(base_family = "serif") +
      ggplot2::labs(title = "Title",
        subtitle = "Subtitle",
        caption = theme_mwk_caption_text())
    expect_true(inherits(p, "ggplot"))
  }
  expect_true(is.character(theme_mwk_caption_text()))
})
