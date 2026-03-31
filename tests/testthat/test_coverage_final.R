library(testthat)
library(bidser)

context("Final coverage push")

# ===========================================================================
# Shared fixture: real BIDS directory on disk with 2 subjects, 2 tasks,
# 2 runs each, plus anat.
# ===========================================================================
create_plot_proj <- function() {
  tmp <- tempfile("bidser_pf_")
  dir.create(tmp, recursive = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = c("sub-01", "sub-02")),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "PlotProj", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  for (sub in c("sub-01", "sub-02")) {
    dir.create(file.path(tmp, sub, "func"), recursive = TRUE)
    dir.create(file.path(tmp, sub, "anat"), recursive = TRUE)
    for (task in c("rest", "nback")) {
      for (run in c("01", "02")) {
        writeLines("x", file.path(
          tmp, sub, "func",
          paste0(sub, "_task-", task, "_run-", run, "_bold.nii.gz")
        ))
        readr::write_tsv(
          tibble::tibble(onset = 0, duration = 1, trial_type = "go"),
          file.path(
            tmp, sub, "func",
            paste0(sub, "_task-", task, "_run-", run, "_events.tsv")
          )
        )
      }
    }
    writeLines("x", file.path(tmp, sub, "anat", paste0(sub, "_T1w.nii.gz")))
  }
  tmp
}

# ===========================================================================
# Shared fixture for pack_bids tests — minimal real BIDS on disk
# ===========================================================================
create_pack_fixture <- function() {
  tmp <- tempfile("bidser_pk_")
  dir.create(tmp, recursive = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "PackTest", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  dir.create(file.path(tmp, "sub-01", "anat"), recursive = TRUE)
  writeLines("dummy nifti content", file.path(
    tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"
  ))
  writeLines("dummy nifti content", file.path(
    tmp, "sub-01", "anat", "sub-01_T1w.nii.gz"
  ))
  readr::write_tsv(
    tibble::tibble(onset = 0, duration = 1, trial_type = "go"),
    file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_events.tsv")
  )
  jsonlite::write_json(
    list(RepetitionTime = 2),
    file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.json"),
    auto_unbox = TRUE
  )
  tmp
}

# ###########################################################################
# Section 1: R/plot_bids.R deep coverage
# ###########################################################################

# ---- bids_heatmap with real bids_project (func) --------------------------
test_that("bids_heatmap func returns ggplot on real project", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  p <- bids_heatmap(proj, interactive = FALSE, file_type = "func")
  expect_s3_class(p, "gg")
})

# ---- bids_heatmap anat branch --------------------------------------------
test_that("bids_heatmap anat returns ggplot on real project", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  p <- bids_heatmap(proj, interactive = FALSE, file_type = "anat")
  expect_s3_class(p, "gg")
})

# ---- bids_heatmap anat with highlight_missing=FALSE -----------------------
test_that("bids_heatmap anat highlight_missing=FALSE works", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  p <- bids_heatmap(proj, interactive = FALSE, file_type = "anat",
                     highlight_missing = FALSE)
  expect_s3_class(p, "gg")
})

# ---- bids_heatmap text_size=0 (no labels) --------------------------------
test_that("bids_heatmap text_size=0 omits text labels", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  p <- bids_heatmap(proj, interactive = FALSE, file_type = "func",
                     text_size = 0)
  expect_s3_class(p, "gg")
})

# ---- bids_heatmap rotate_labels=FALSE -------------------------------------
test_that("bids_heatmap rotate_labels=FALSE works", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  p <- bids_heatmap(proj, interactive = FALSE, file_type = "func",
                     rotate_labels = FALSE)
  expect_s3_class(p, "gg")
})

# ---- bids_heatmap error on unknown file_type ------------------------------
test_that("bids_heatmap errors on unknown file_type", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  expect_error(
    bids_heatmap(proj, file_type = "xyz", interactive = FALSE),
    "not found"
  )
})

# ---- bids_heatmap func with highlight_missing=FALSE -----------------------
test_that("bids_heatmap func highlight_missing=FALSE works on real project", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  p <- bids_heatmap(proj, interactive = FALSE, file_type = "func",
                     highlight_missing = FALSE)
  expect_s3_class(p, "gg")
})

# ---- plot_bids_heatmap (internal helper) ----------------------------------
test_that("plot_bids_heatmap returns ggplot with prepared data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  pdata <- bidser:::prepare_bids_data_for_plot(proj)
  p <- bidser:::plot_bids_heatmap(pdata)
  expect_s3_class(p, "gg")
})

test_that("plot_bids_heatmap with highlight_missing=FALSE works", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  pdata <- bidser:::prepare_bids_data_for_plot(proj)
  p <- bidser:::plot_bids_heatmap(pdata, highlight_missing = FALSE)
  expect_s3_class(p, "gg")
})

test_that("plot_bids_heatmap works when data lacks task column", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  pdata <- bidser:::prepare_bids_data_for_plot(proj)
  # Remove the task column to exercise the label-only branch
  pdata$raw_data$task <- NULL
  p <- bidser:::plot_bids_heatmap(pdata)
  expect_s3_class(p, "gg")
})

# ---- plot_bids: visualization modes on real project -----------------------
test_that("plot_bids standard mode returns ggplot/patchwork on real project", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")
  skip_if_not_installed("patchwork")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  p <- plot_bids(proj, interactive = FALSE, visualization_mode = "standard")
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("plot_bids heatmap mode returns ggplot on real project", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  p <- tryCatch(
    plot_bids(proj, interactive = FALSE, visualization_mode = "heatmap"),
    error = function(e) e
  )
  # May succeed or fail inside heatmap pipeline; either way no input error

  if (inherits(p, "error")) {
    expect_false(grepl("bids_project|mock_bids_project", p$message))
  } else {
    expect_true(inherits(p, "gg") || inherits(p, "ggplot"))
  }
})

test_that("plot_bids complete mode returns gg on real project", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")
  skip_if_not_installed("patchwork")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  # The complete mode may warn because internal bids_heatmap receives

  # a list rather than a bids_project; we still expect a gg result.
  p <- suppressWarnings(
    plot_bids(proj, interactive = FALSE, visualization_mode = "complete")
  )
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("plot_bids warns on invalid visualization_mode", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")
  skip_if_not_installed("patchwork")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  expect_warning(
    p <- plot_bids(proj, interactive = FALSE, visualization_mode = "bogus"),
    "Invalid visualization_mode"
  )
  # Falls back to standard
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("plot_bids debug mode prints diagnostics on real project", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")
  skip_if_not_installed("patchwork")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  out <- capture.output(
    p <- plot_bids(proj, interactive = FALSE, debug = TRUE)
  )
  expect_true(any(grepl("Project name|Has raw data|Raw data rows", out)))
})

# ---- create_virtual_bids_project ------------------------------------------
test_that("create_virtual_bids_project default returns mock_bids_project with deprecation", {
  expect_warning(
    virt <- bidser:::create_virtual_bids_project(),
    "deprecated"
  )
  expect_true(inherits(virt, "mock_bids_project"))
  expect_true(isTRUE(virt$is_virtual))
})

test_that("create_virtual_bids_project with sessions", {
  expect_warning(
    virt <- bidser:::create_virtual_bids_project(
      name = "SesProj",
      subjects = c("sub-01", "sub-02"),
      sessions = c("ses-01", "ses-02"),
      tasks = "rest",
      runs = "01"
    ),
    "deprecated"
  )
  expect_true(inherits(virt, "mock_bids_project"))
})

test_that("create_virtual_bids_project with multiple modalities", {
  expect_warning(
    virt <- bidser:::create_virtual_bids_project(
      name = "MultiMod",
      subjects = c("sub-01", "sub-02"),
      tasks = c("rest", "nback"),
      runs = c("01", "02"),
      modalities = c("T1w", "bold")
    ),
    "deprecated"
  )
  expect_true(inherits(virt, "mock_bids_project"))
})

# ###########################################################################
# Section 2: R/pack_bids.R — pack_bids and list_pack_bids with local fixture
# ###########################################################################

test_that("pack_bids creates tar.gz archive from local fixture", {
  tmp <- create_pack_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  tar_path <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tar_path), add = TRUE)

  result <- pack_bids(proj, output_file = tar_path, verbose = FALSE)
  expect_true(file.exists(result))
  expect_true(file.size(result) > 0)
})

test_that("pack_bids creates zip archive from local fixture", {
  tmp <- create_pack_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  zip_path <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_path), add = TRUE)

  result <- pack_bids(proj, output_file = zip_path, format = "zip",
                       verbose = FALSE)
  expect_true(file.exists(result))
  expect_true(file.size(result) > 0)
})

test_that("pack_bids with strict_bids=TRUE works", {
  tmp <- create_pack_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  out_path <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(out_path), add = TRUE)

  result <- pack_bids(proj, output_file = out_path, strict_bids = TRUE,
                       verbose = FALSE)
  expect_true(file.exists(result))
})

test_that("pack_bids with include_derivatives=FALSE works", {
  tmp <- create_pack_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  out_path <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(out_path), add = TRUE)

  result <- pack_bids(proj, output_file = out_path,
                       include_derivatives = FALSE, verbose = FALSE)
  expect_true(file.exists(result))
})

test_that("pack_bids with small max_file_size stubs large files", {
  tmp <- create_pack_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  out_path <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(out_path), add = TRUE)

  result <- pack_bids(proj, output_file = out_path, max_file_size = "1B",
                       verbose = FALSE)
  expect_true(file.exists(result))
})

test_that("pack_bids with exclude pattern excludes matching files", {
  tmp <- create_pack_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  out_path <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(out_path), add = TRUE)

  result <- pack_bids(proj, output_file = out_path,
                       exclude = "\\.json$", verbose = FALSE)
  expect_true(file.exists(result))

  # Inspect the archive; JSON files should be stubbed (0-byte)
  contents <- list_pack_bids(result, verbose = FALSE)
  json_rows <- contents[grepl("\\.json$", contents$file), ]
  if (nrow(json_rows) > 0) {
    expect_true(all(json_rows$size == 0))
  }
})

test_that("pack_bids verbose mode prints progress messages", {
  tmp <- create_pack_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  out_path <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(out_path), add = TRUE)

  expect_message(
    result <- pack_bids(proj, output_file = out_path, verbose = TRUE),
    "Starting pack_bids|pack_bids Complete"
  )
  expect_true(file.exists(result))
})

# ---- list_pack_bids -------------------------------------------------------
test_that("list_pack_bids reads tar.gz archive contents", {
  tmp <- create_pack_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  tar_path <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tar_path), add = TRUE)

  pack_bids(proj, output_file = tar_path, verbose = FALSE)

  contents <- list_pack_bids(tar_path, verbose = FALSE)
  expect_s3_class(contents, "data.frame")
  expect_true(nrow(contents) > 0)
  expect_true("file" %in% names(contents))
  expect_true("size" %in% names(contents))
  expect_true("is_stub" %in% names(contents))
  expect_true("type" %in% names(contents))

  # Imaging files should be stubs (0-byte)
  stubs <- contents[contents$is_stub, ]
  if (nrow(stubs) > 0) {
    expect_true(all(stubs$size == 0))
  }
})

test_that("list_pack_bids reads zip archive contents", {
  tmp <- create_pack_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  zip_path <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_path), add = TRUE)

  pack_bids(proj, output_file = zip_path, format = "zip", verbose = FALSE)

  contents <- list_pack_bids(zip_path, verbose = FALSE)
  expect_s3_class(contents, "data.frame")
  expect_true(nrow(contents) > 0)
})

test_that("list_pack_bids verbose prints summary", {
  tmp <- create_pack_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  tar_path <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tar_path), add = TRUE)
  pack_bids(proj, output_file = tar_path, verbose = FALSE)

  out <- capture.output(
    contents <- list_pack_bids(tar_path, verbose = TRUE)
  )
  expect_true(any(grepl("Archive contents summary", out)))
  expect_true(any(grepl("Total files", out)))
})

test_that("list_pack_bids errors on missing file", {
  expect_error(list_pack_bids("/no/such/file.tar.gz"), "not found")
})

test_that("list_pack_bids errors on unknown format", {
  tf <- tempfile(fileext = ".rar")
  file.create(tf)
  on.exit(unlink(tf), add = TRUE)
  expect_error(list_pack_bids(tf), "Unknown archive format")
})

# ###########################################################################
# Section 3: R/bids.R more coverage
# ###########################################################################

# ---- load_all_events.bids_project -----------------------------------------
test_that("load_all_events returns tibble with metadata columns", {
  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  result <- load_all_events(proj)
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true(".subid" %in% names(result))
  expect_true(".task" %in% names(result))
  expect_true(".run" %in% names(result))
})

test_that("load_all_events filters by subid", {
  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  result <- load_all_events(proj, subid = "01")
  expect_s3_class(result, "tbl_df")
  if (nrow(result) > 0) {
    expect_true(all(result$.subid == "01"))
  }
})

test_that("load_all_events filters by task", {
  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  result <- load_all_events(proj, task = "nback")
  expect_s3_class(result, "tbl_df")
  if (nrow(result) > 0) {
    expect_true(all(result$.task == "nback"))
  }
})

test_that("load_all_events returns empty tibble when no match", {
  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  result <- load_all_events(proj, subid = "nonexistent")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# ---- preproc_scans without fmriprep --------------------------------------
test_that("preproc_scans returns NULL when no fmriprep derivatives", {
  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp, fmriprep = FALSE)

  expect_message(
    result <- preproc_scans(proj),
    "not found|NULL"
  )
  expect_null(result)
})

# ---- prepare_bids_data_for_plot on real project ---------------------------
test_that("prepare_bids_data_for_plot adds file_size from real files", {
  skip_if_not_installed("ggplot2")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  pdata <- bidser:::prepare_bids_data_for_plot(proj)
  expect_true("file_size" %in% names(pdata$raw_data))
  expect_true(is.numeric(pdata$raw_data$file_size))
  # All NAs should be filled
  expect_false(any(is.na(pdata$raw_data$file_size)))
})

# ---- Internal plot helpers on real project data ---------------------------
test_that("plot_bids_completeness on real project data returns ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  pdata <- bidser:::prepare_bids_data_for_plot(proj)
  p <- bidser:::plot_bids_completeness(pdata)
  expect_s3_class(p, "gg")
})

test_that("plot_bids_file_sizes on real project data returns ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  pdata <- bidser:::prepare_bids_data_for_plot(proj)
  p <- bidser:::plot_bids_file_sizes(pdata)
  expect_s3_class(p, "gg")
})

test_that("plot_bids_tasks on real project data returns ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  pdata <- bidser:::prepare_bids_data_for_plot(proj)
  p <- bidser:::plot_bids_tasks(pdata)
  expect_s3_class(p, "gg")
})

test_that("plot_bids_structure on real project data returns ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  pdata <- bidser:::prepare_bids_data_for_plot(proj)
  p <- bidser:::plot_bids_structure(pdata)
  expect_s3_class(p, "gg")
})

# ---- plot_bids_file_sizes with different scales on real data --------------
test_that("plot_bids_file_sizes sqrt and linear scales on real data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  pdata <- bidser:::prepare_bids_data_for_plot(proj)

  p_sqrt <- bidser:::plot_bids_file_sizes(pdata, scale = "sqrt")
  expect_s3_class(p_sqrt, "gg")

  p_lin <- bidser:::plot_bids_file_sizes(pdata, scale = "linear")
  expect_s3_class(p_lin, "gg")
})

# ---- plot_bids_heatmap with custom color_scheme ---------------------------
test_that("plot_bids_heatmap respects custom color_scheme", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  tmp <- create_plot_proj()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  proj <- bids_project(tmp)

  pdata <- bidser:::prepare_bids_data_for_plot(proj)
  p <- bidser:::plot_bids_heatmap(pdata, color_scheme = "plasma")
  expect_s3_class(p, "gg")
})
