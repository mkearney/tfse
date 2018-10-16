context("test-tfse")

test_that("col2hex", {
  blue <- col2hex("blue")
  expect_identical(blue, "#0000FF")
})

test_that("pmsg", {
  expect_message(pmsg("this ", "is ", "a", " test"))
})

test_that("rescale", {
  expect_true(is.numeric(rescale_log(1:10)))
  expect_lt(sum(rescale_log(1:10)), sum(1:10))
})

test_that("match_arg", {
  arg <- "yep "
  expect_identical("yep", match_arg(arg, c("yep", "na", "other")))
  expect_error(match_arg(arg, c("yep", "na", "other"), trim_ws = FALSE))
})

test_that("na_omit", {
  x <- c(letters[1:3], NA_character_, letters[4:6], NA_character_)
  expect_identical(na_omit(x), letters[1:6])
  y <- rnorm(length(x))
  y[c(4, 8)] <- NA_real_
  df <- data.frame(x, y, stringsAsFactors = FALSE)
  expect_equal(nrow(na_omit(df)), 6)
})

test_that("set_class", {
  x <- 10
  x <- set_class(x, "integer")
  expect_identical(class(x), "integer")
})


test_that("cc", {
  x <- cc("this,that,other,,the")
  expect_true(is.character(x))
  expect_equal(length(x), 5)
})

test_that("lines", {
  txt <- paste(sample(c(rep(" ", 10), letters), 100, replace = TRUE),
    collapse = "")
  tmp <- tempfile()
  cat(txt, file = tmp, fill = TRUE)
  x <- readlines(tmp)
  unlink(tmp)
  expect_identical(txt, x)
})

test_that("dsstore", {
  dir <- tempdir()
  owd <- getwd()
  setwd(dir)
  cat("asdf", file = ".DS_Store", fill = TRUE)
  rm_.DS_Store()
  f <- list.files(all.files = TRUE)
  setwd(owd)
  expect_true(!".DS_Store" %in% f)
})

test_that("in", {
  y <- yin(letters[1:3], letters[3:4])
  n <- nin(letters[1:3], letters[3:4])
  expect_identical("c", y)
  expect_identical(c("a", "b"), n)
})

test_that("gitrename", {
  expect_identical("git remote set-url origin {new_url}", rename_git_repo())
})

test_that("trim_ws", {
  expect_identical("this", trim_ws(" this "))
})

test_that("shhh", {
  o <- capture.output(shhh(rnorm(10)))
  expect_true(length(o) == 0)
})

test_that("psub", {
  expect_identical("testing psub function",
    psub("testing {this} function", this = "psub"))
})


test_that("regmatches_", {
  m <- gregexpr_(letters, "a")
  expect_equal(sum(sapply(m, function(.x) .x > 0)), 1)
  o <- regmatches_(letters, "a")
  expect_identical(c("a", rep("", 25)), o)
})

