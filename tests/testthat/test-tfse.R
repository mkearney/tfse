context("test-tfse")

test_that("tfse works", {
  d <- data.frame(abc = sample(letters[1:3], 100, replace = TRUE),
    xyz = sample(letters[24:26], 100, replace = TRUE),
    stringsAsFactors = FALSE)
  d <- tabsort(d)
  expect_equal(ncol(d), 4)
})
