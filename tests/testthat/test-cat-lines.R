test_that("cat_line functions work", {
  ## multiple lines
  expect_equal(
    capture.output(cat_lines("this", "is", "a", "test.")),
    c("this", "is", "a", "test.")
  )
  expect_equal(
    capture.output({
      cat_lines("this", "is", "a", "test", fill = FALSE)
      cat(".")}),
    c("this", "is", "a", "test.")
  )
  expect_equal(
    capture.output(cat_lines(c("this", "is"), c("a", "test"), sep = " ")),
    capture.output(cat_lines(list("this", "is"), list("a", "test"), sep = " "))
  )
  expect_equal(
    capture.output(cat_lines("this", "is", "a", "test", collapse = " ")),
    "this is a test"
  )

  ## single line
  expect_equal(
    capture.output(cat_line("this", "is", "a", "test.")),
    "thisisatest."
  )
  expect_equal(
    capture.output({
      cat_line("this", "is", "a", "test", fill = FALSE)
      cat(".")}),
    "thisisatest."
  )
})
