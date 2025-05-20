context("plot.bids_project dendrogram")

library(testthat)
library(bidser)

make_proj <- function(...) {
  bidser:::create_virtual_bids_project(
    name = "test_proj",
    subjects = c("sub-01", "sub-02"),
    sessions = NULL,
    tasks = c("rest"),
    runs = c("01"),
    modalities = c("T1w", "bold"),
    derivatives = FALSE,
    ...
  )
}

proj <- make_proj()


test_that("plot.bids_project returns invisibly", {
  expect_invisible(plot(proj, max_depth = 2))
})
