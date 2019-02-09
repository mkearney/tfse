context("test-tfse")

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
  expect_identical(as.list(c("a", rep("", 25))), o)
  o <- regmatches_(letters, "a", drop = TRUE)
  expect_identical(o, "a")
  o <- regmatches_(as.list(letters), "a", drop = TRUE)
  expect_identical(o, as.list(c("a", rep(list(character()), 25))))
  o <- regmatches_(as.list(letters), "a", drop = FALSE)
  expect_identical(o, as.list(c(list(list("a")), rep(list(list("")), 25))))
})


test_that("is_installed", {
  expect_false(tfse:::is_installed("asdf"))
  expect_equal(tfse:::is_installed("asdf", "stats"), c(asdf = FALSE, stats = TRUE))
  expect_error(tfse:::is_installed("asdf", .stop = TRUE))
})


test_that("desc", {
  x <- desc_get_var("base", "license")
  expect_named(x)
  expect_equal(names(x[[1]]), "license")
  expect_null(desc_gh_repo("stats"))
  expect_equal("mkearney/tfse", desc_gh_repo("tfse"))
})




test_that("write_function", {
  x <- write_function(base::abbreviate)
  expect_true(file.exists(x))
})



test_that("search_files", {
  o <- capture.output(search_files("DESCRIPTION", path = "../.."))
  expect_true(is.character(o))
  expect_gt(length(o), 1)
})

test_that("renv", {
  expect_true(is.character(.Renviron()))
  expect_true(is.character(home()))
  cat("", file = ".Renviron")
  set_renv(tfse_test_var = "asdf")
  expect_equal("asdf", Sys.getenv("tfse_test_var"))
  unlink(".Renviron")
})


test_that("apa_citation", {
  expect_true(is.character(apa_citation("base")))
})



test_that("box_code", {
  expect_true(is.character(box_code("TEST")))
})


test_that("show_connections", {
  expect_true(is.data.frame(show_connections()))
})

test_that("r_dir", {
  expect_true(is.character(r_dir()))
})

test_that("r_dir", {
  expect_true(is.character(github_raw("https://github.com/mkearney/rtweet")))
})



test_that("na", {
  expect_equal(length(count_na(mtcars)), ncol(mtcars))
  expect_equal(ncol(min_var(mtcars, 1)), 6)
})



test_that("rescale", {
  x <- rnorm(100, 100, 1)
  expect_lt(max(rescale_normal(x)), 10)
  expect_lt(max(rescale_log(x)), 5)
  expect_lt(max(rescale_pointscale(x, -15, 15)), 16)
  expect_gt(min(rescale_pointscale(x, -15, 15)), -16)
})



test_that("regmatches lists", {
  x <- lapply(as.list(letters), factor)
  expect_true(is.list(regmatches_(x, "a")))
  expect_true(is.list(regmatches_first(x, "a")))
})
