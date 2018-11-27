context("test-data_set")

test_that("data_set works", {
  y <- data.frame(
    x = rnorm(10),
    y = sample(1:10, 10),
    row.names = letters[1:10]
  )
  x <- data_set(name = names(y), value = row2vec(y[1, ]),
    rnorm(ncol(y)), stringsAsFactors = FALSE)
  e <- tryCatch(print_as_col(y), error = function(e) TRUE)
  expect_true(!identical(e, TRUE))
  expect_true(is.data.frame(data_set(y)))
  expect_true(is.data.frame(x))
})
