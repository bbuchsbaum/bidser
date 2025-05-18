library(testthat)
library(bidser)

context("fmap_parser")

test_that("fmap_parser can be constructed", {
  obj <- fmap_parser()
  expect_s3_class(obj, c("fmap_parser", "parser"))
})
