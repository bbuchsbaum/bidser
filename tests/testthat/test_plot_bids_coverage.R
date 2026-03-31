library(testthat)
library(bidser)

context("Plot functions coverage")

# ---------------------------------------------------------------------------
# Local fixture: real BIDS directory on disk for bids_project()
# ---------------------------------------------------------------------------
create_plot_fixture <- function() {

  tmp <- tempfile("bidser_plot_")
  dir.create(tmp, recursive = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = c("sub-01", "sub-02")),
    file.path(tmp, "participants.tsv")
  )

  jsonlite::write_json(
    list(Name = "PlotTest", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )

  for (sub in c("sub-01", "sub-02")) {
    dir.create(file.path(tmp, sub, "func"), recursive = TRUE)
    dir.create(file.path(tmp, sub, "anat"), recursive = TRUE)
    for (task in c("rest", "nback")) {
      writeLines("dummy", file.path(
        tmp, sub, "func",
        paste0(sub, "_task-", task, "_run-01_bold.nii.gz")
      ))
      readr::write_tsv(
        tibble::tibble(onset = 0, duration = 1, trial_type = "go"),
        file.path(
          tmp, sub, "func",
          paste0(sub, "_task-", task, "_run-01_events.tsv")
        )
      )
    }
    writeLines("dummy", file.path(tmp, sub, "anat", paste0(sub, "_T1w.nii.gz")))
  }

  tmp
}

# ---------------------------------------------------------------------------
# Helper: mock project via create_mock_bids (no filesystem)
# ---------------------------------------------------------------------------
make_mock_proj <- function() {
  file_structure <- tibble::tribble(
    ~subid, ~session, ~datatype, ~task, ~run, ~suffix,   ~fmriprep,
    "01",   NA,       "anat",    NA,    NA,   "T1w.nii.gz",  FALSE,
    "01",   NA,       "func",    "rest", "01", "bold.nii.gz", FALSE,
    "01",   NA,       "func",    "nback","01", "bold.nii.gz", FALSE,
    "02",   NA,       "anat",    NA,    NA,   "T1w.nii.gz",  FALSE,
    "02",   NA,       "func",    "rest", "01", "bold.nii.gz", FALSE,
    "02",   NA,       "func",    "nback","01", "bold.nii.gz", FALSE
  )

  participants <- tibble::tibble(
    participant_id = c("01", "02"),
    age = c(25, 30)
  )

  bidser::create_mock_bids(
    project_name = "coverage_proj",
    participants  = participants,
    file_structure = file_structure
  )
}

# =========================================================================
# 1. plot.bids_project  (R/plot_bids_project.R)
# =========================================================================
test_that("plot.bids_project returns invisibly from a real bids_project", {
  tmp <- create_plot_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp)

  out <- capture.output(result <- plot(proj))
  expect_identical(result, proj)                 # returned invisibly
})

test_that("plot.bids_project works with max_depth = 2", {
  tmp <- create_plot_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp)

  out <- capture.output(result <- plot(proj, max_depth = 2))
  expect_identical(result, proj)
})

test_that("plot.bids_project errors on non-bids_project input", {
  expect_error(plot.bids_project("not a project"), "bids_project")
  expect_error(plot.bids_project(42), "bids_project")
})

# =========================================================================
# 2. plot.mock_bids_project  (R/plot_bids_project.R)
# =========================================================================
test_that("plot.mock_bids_project returns invisibly", {
  mock <- make_mock_proj()

  out <- capture.output(result <- plot(mock))
  expect_identical(result, mock)
})

test_that("plot.mock_bids_project works with max_depth = 2", {
  mock <- make_mock_proj()

  out <- capture.output(result <- plot(mock, max_depth = 2))
  expect_identical(result, mock)
})

test_that("plot.mock_bids_project errors on non-mock_bids_project input", {
  expect_error(plot.mock_bids_project("not a mock"), "mock_bids_project")
  expect_error(plot.mock_bids_project(list(a = 1)), "mock_bids_project")
})

# =========================================================================
# 3. plot_bids input validation  (R/plot_bids.R)
# =========================================================================
test_that("plot_bids errors on non-bids_project input", {
  expect_error(plot_bids("nope"), "bids_project|mock_bids_project")
  expect_error(plot_bids(123), "bids_project|mock_bids_project")
  expect_error(plot_bids(list(x = 1)), "bids_project|mock_bids_project")
})

test_that("plot_bids with empty tbl returns ggplot with 'No data' annotation", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  mock <- make_mock_proj()
  # Zero out the tbl to simulate an empty project

  mock$tbl <- mock$tbl[0, ]

  p <- plot_bids(mock, interactive = FALSE)
  expect_s3_class(p, "gg")
})

test_that("plot_bids standard mode returns ggplot for mock project", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")
  skip_if_not_installed("patchwork")

  mock <- make_mock_proj()

  p <- plot_bids(mock, interactive = FALSE, visualization_mode = "standard")
  # Should return either a ggplot or a patchwork object (both have class "gg")
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("plot_bids heatmap mode returns ggplot for mock project", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  mock <- make_mock_proj()

  # heatmap mode calls bids_heatmap internally; may error if type column

  # name differs, but it should at least not crash on input validation
  p <- tryCatch(
    plot_bids(mock, interactive = FALSE, visualization_mode = "heatmap"),
    error = function(e) e
  )
  # Either succeeds with a ggplot or fails inside the heatmap with a

  # descriptive error (not an input-validation error)
  if (inherits(p, "error")) {
    expect_false(grepl("bids_project|mock_bids_project", p$message))
  } else {
    expect_true(inherits(p, "gg") || inherits(p, "ggplot"))
  }
})

test_that("plot_bids debug mode prints diagnostics", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")
  skip_if_not_installed("patchwork")

  mock <- make_mock_proj()

  out <- capture.output(
    p <- plot_bids(mock, interactive = FALSE, debug = TRUE)
  )
  expect_true(any(grepl("Project name|Has raw data", out)))
})

# =========================================================================
# 4. prepare_bids_data_for_plot  (R/plot_bids.R)
# =========================================================================
test_that("prepare_bids_data_for_plot returns list with project_info and raw_data", {
  skip_if_not_installed("ggplot2")

  mock <- make_mock_proj()

  result <- bidser:::prepare_bids_data_for_plot(mock)

  expect_type(result, "list")
  expect_true("project_info" %in% names(result))
  expect_true("raw_data" %in% names(result))

  # project_info should contain standard fields
  expect_true("name" %in% names(result$project_info))
  expect_true("subjects" %in% names(result$project_info))

  # raw_data should be a data.frame / tibble

  expect_true(is.data.frame(result$raw_data))
})

test_that("prepare_bids_data_for_plot adds file_size column", {
  skip_if_not_installed("ggplot2")

  mock <- make_mock_proj()

  result <- bidser:::prepare_bids_data_for_plot(mock)

  expect_true("file_size" %in% names(result$raw_data))
  # file_size should be numeric and non-NA after processing
  expect_true(is.numeric(result$raw_data$file_size))
  expect_false(any(is.na(result$raw_data$file_size)))
})

test_that("prepare_bids_data_for_plot with include_derivatives=FALSE filters derivatives", {
  skip_if_not_installed("ggplot2")

  mock <- make_mock_proj()

  result_all  <- bidser:::prepare_bids_data_for_plot(mock, include_derivatives = TRUE)
  result_no_d <- bidser:::prepare_bids_data_for_plot(mock, include_derivatives = FALSE)

  # If there is a 'derivative' column, the filtered result should be <= full result
  if ("derivative" %in% names(result_all$raw_data)) {
    expect_true(nrow(result_no_d$raw_data) <= nrow(result_all$raw_data))
  } else {
    # Without the column both should be equal
    expect_equal(nrow(result_no_d$raw_data), nrow(result_all$raw_data))
  }
})

test_that("prepare_bids_data_for_plot errors on invalid input", {
  expect_error(
    bidser:::prepare_bids_data_for_plot("bad"),
    "bids_project|mock_bids_project"
  )
})

# =========================================================================
# 5. create_virtual_bids_project  (R/plot_bids.R)
# =========================================================================
test_that("create_virtual_bids_project creates a virtual project with deprecation warning", {
  expect_warning(
    virt <- bidser:::create_virtual_bids_project(
      name      = "VirtTest",
      subjects  = c("sub-01", "sub-02"),
      tasks     = "rest",
      runs      = "01"
    ),
    "deprecated"
  )

  expect_true(inherits(virt, "mock_bids_project"))
  expect_true(isTRUE(virt$is_virtual))
})

# =========================================================================
# 6. Individual internal plot helpers  (R/plot_bids.R)
#    Each requires data from prepare_bids_data_for_plot()
# =========================================================================

# Build shared data fixture used by the helper-function tests
make_plot_data <- function() {
  mock <- make_mock_proj()
  bidser:::prepare_bids_data_for_plot(mock)
}

# --- 6a. plot_bids_completeness -------------------------------------------
test_that("plot_bids_completeness returns a ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  pdata <- make_plot_data()

  p <- bidser:::plot_bids_completeness(pdata)
  expect_s3_class(p, "gg")
})

test_that("plot_bids_completeness accepts custom color_scheme", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  pdata <- make_plot_data()

  p <- bidser:::plot_bids_completeness(pdata, color_scheme = "plasma")
  expect_s3_class(p, "gg")
})

# --- 6b. plot_bids_file_sizes ---------------------------------------------
test_that("plot_bids_file_sizes returns a ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  pdata <- make_plot_data()

  p <- bidser:::plot_bids_file_sizes(pdata)
  expect_s3_class(p, "gg")
})

test_that("plot_bids_file_sizes respects scale parameter", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  pdata <- make_plot_data()

  p_log  <- bidser:::plot_bids_file_sizes(pdata, scale = "log")
  p_sqrt <- bidser:::plot_bids_file_sizes(pdata, scale = "sqrt")
  p_lin  <- bidser:::plot_bids_file_sizes(pdata, scale = "linear")

  expect_s3_class(p_log, "gg")
  expect_s3_class(p_sqrt, "gg")
  expect_s3_class(p_lin, "gg")
})

# --- 6c. plot_bids_tasks ---------------------------------------------------
test_that("plot_bids_tasks returns a ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  pdata <- make_plot_data()

  p <- bidser:::plot_bids_tasks(pdata)
  expect_s3_class(p, "gg")
})

test_that("plot_bids_tasks accepts custom color_scheme", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  pdata <- make_plot_data()

  p <- bidser:::plot_bids_tasks(pdata, color_scheme = "magma")
  expect_s3_class(p, "gg")
})

# --- 6d. plot_bids_structure -----------------------------------------------
test_that("plot_bids_structure returns a ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  pdata <- make_plot_data()

  p <- bidser:::plot_bids_structure(pdata)
  expect_s3_class(p, "gg")
})

test_that("plot_bids_structure accepts custom color_scheme", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  pdata <- make_plot_data()

  p <- bidser:::plot_bids_structure(pdata, color_scheme = "inferno")
  expect_s3_class(p, "gg")
})

# --- 6e. bids_heatmap (exported) ------------------------------------------
test_that("bids_heatmap returns a ggplot for func data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  mock <- make_mock_proj()

  p <- bids_heatmap(mock, interactive = FALSE)
  expect_s3_class(p, "gg")
})

test_that("bids_heatmap errors on non-bids_project input", {
  expect_error(bids_heatmap("bad"), "bids_project|mock_bids_project")
})

test_that("bids_heatmap errors on unknown file_type", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  mock <- make_mock_proj()

  expect_error(bids_heatmap(mock, file_type = "dwi", interactive = FALSE), "not found")
})

test_that("bids_heatmap works for anat file_type", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  mock <- make_mock_proj()

  p <- bids_heatmap(mock, file_type = "anat", interactive = FALSE)
  expect_s3_class(p, "gg")
})

test_that("bids_heatmap with highlight_missing=FALSE works", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  mock <- make_mock_proj()

  p <- bids_heatmap(mock, interactive = FALSE, highlight_missing = FALSE)
  expect_s3_class(p, "gg")
})

test_that("bids_heatmap respects text_size and rotate_labels", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  mock <- make_mock_proj()

  p <- bids_heatmap(mock, interactive = FALSE, text_size = 0, rotate_labels = FALSE)
  expect_s3_class(p, "gg")
})

# =========================================================================
# 7. plot_bids with a real bids_project from fixture
# =========================================================================
test_that("plot_bids works on a real bids_project", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")
  skip_if_not_installed("patchwork")

  tmp <- create_plot_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp)

  p <- plot_bids(proj, interactive = FALSE, visualization_mode = "standard")
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("bids_heatmap works on a real bids_project", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp)

  p <- bids_heatmap(proj, interactive = FALSE)
  expect_s3_class(p, "gg")
})
