context("plot_bids and bids_heatmap")

library(testthat)
library(bidser)

# Helper to make a tiny virtual project
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

# Basic dataset
proj <- make_proj()

# dataset without run column to trigger plot_bids_heatmap
proj_missing_run <- make_proj()
proj_missing_run$raw_data$run <- NULL


test_that("plot_bids returns ggplot", {
  p_static <- plot_bids(proj, interactive = FALSE)
  expect_s3_class(p_static, "ggplot")

  p_interactive <- plot_bids(proj, interactive = TRUE)
  expect_s3_class(p_interactive, "ggplot")
})


test_that("bids_heatmap returns ggplot", {
  p_static <- bids_heatmap(proj, interactive = FALSE)
  expect_s3_class(p_static, "ggplot")

  p_interactive <- bids_heatmap(proj, interactive = TRUE)
  expect_s3_class(p_interactive, "ggplot")
})


test_that("bids_heatmap falls back when tasks or runs missing", {
  p <- bids_heatmap(proj_missing_run, interactive = FALSE)
  expect_s3_class(p, "ggplot")
})


