context("utility helpers")
library(testthat)
library(bidser)

# Tests for str_detect_null
test_that("str_detect_null handles NULL and NA correctly", {
  expect_true(bidser:::str_detect_null("abc", "a"))
  expect_false(bidser:::str_detect_null(NULL, "a"))
  expect_true(bidser:::str_detect_null(NULL, "a", default = TRUE))
  expect_false(bidser:::str_detect_null(NA_character_, "a"))
  expect_true(bidser:::str_detect_null(NA_character_, "a", default = TRUE))
})

# Tests for key_match
test_that("key_match matcher works with various patterns", {
  matcher <- bidser:::key_match(default = FALSE, subid = "01", task = "task")
  expect_true(matcher(list(subid = "01", task = "task")))
  expect_false(matcher(list(subid = "02", task = "task")))
  expect_false(matcher(list(subid = "01")))

  matcher2 <- bidser:::key_match(default = TRUE, subid = "01", task = "task")
  expect_true(matcher2(list(subid = "01")))
  expect_true(matcher2(list(subid = "01", task = "task")))

  wildcard_matcher <- bidser:::key_match(subid = "01", task = ".*")
  expect_true(wildcard_matcher(list(subid = "01", task = "whatever")))
  expect_true(wildcard_matcher(list(subid = "01")))

  nullpattern_matcher <- bidser:::key_match(task = NULL)
  expect_true(nullpattern_matcher(list(task = NULL)))
  expect_false(nullpattern_matcher(list(task = "something")))
})

