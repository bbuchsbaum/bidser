context("plot.bids_confounds")
library(testthat)
library(bidser)
library(tibble)

create_confounds_proj_plot <- function() {
  temp_dir <- tempfile("bids_conf_plot_")
  dir.create(temp_dir)
  readr::write_tsv(tibble(participant_id = "01"),
                   file.path(temp_dir, "participants.tsv"))
  jsonlite::write_json(list(Name = "TestConfounds", BIDSVersion = "1.7.0"),
                       file.path(temp_dir, "dataset_description.json"),
                       auto_unbox = TRUE)
  dir.create(file.path(temp_dir, "sub-01"))
  conf_dir <- file.path(temp_dir, "derivatives", "fmriprep", "sub-01", "func")
  dir.create(conf_dir, recursive = TRUE)
  conf_data <- tibble(
    CSF = c(0.1, 0.2, 0.3, 0.2),
    WhiteMatter = c(0.5, 0.6, 0.7, 0.6),
    FramewiseDisplacement = c(0.01, 0.02, 0.03, 0.02),
    GlobalSignal = c(1.1, 1.2, 1.3, 1.2)
  )
  conf_file <- file.path(conf_dir,
                         "sub-01_task-test_run-01_desc-confounds_timeseries.tsv")
  readr::write_tsv(conf_data, conf_file)
  list(path = temp_dir,
       proj = bids_project(temp_dir, fmriprep = TRUE))
}

test_that("plot.bids_confounds returns a plot object", {
  skip_if_not_installed("ggplot2")
  setup <- create_confounds_proj_plot()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)
  conf <- read_confounds(setup$proj, npcs = 2)
  p <- plot(conf)
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork") || is.list(p))
})
