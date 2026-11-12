context("acquisition matrix, structure tree and NIfTI volume counts")

skip_if_not_installed("ggplot2")
skip_if_not_installed("patchwork")

write_nifti_header <- function(path, nvol) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- gzfile(path, "wb")
  on.exit(close(con))
  writeBin(348L, con, size = 4, endian = "little")
  writeBin(raw(36), con)
  writeBin(as.integer(c(4, 8, 8, 4, nvol, 1, 1, 1)), con, size = 2, endian = "little")
  writeBin(raw(348 - 56), con)
}

make_qc_project <- function() {
  root <- tempfile("bidser-qc-")
  dir.create(root)
  jsonlite::write_json(list(Name = "QC", BIDSVersion = "1.8.0"),
                       file.path(root, "dataset_description.json"), auto_unbox = TRUE)
  readr::write_tsv(tibble::tibble(participant_id = sprintf("sub-%02d", 1:4)),
                   file.path(root, "participants.tsv"))
  for (s in sprintf("%02d", 1:4)) {
    sb <- paste0("sub-", s)
    file.create(file.path(root, sb, "anat", paste0(sb, "_T1w.nii.gz")) |>
                  (\(f) { dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE); f })())
    for (r in 1:2) {
      if (s == "03" && r == 2) next                    # missing run
      nv <- if (s == "04" && r == 1) 40L else 100L     # truncated run
      f <- file.path(root, sb, "func", sprintf("%s_task-rest_run-%d_bold.nii.gz", sb, r))
      write_nifti_header(f, nv)
    }
  }
  bids_project(root)
}

test_that("nifti_nvols reads dim[5] and tolerates non-NIfTI files", {
  f <- tempfile(fileext = ".nii.gz")
  write_nifti_header(f, 123L)
  empty <- tempfile(fileext = ".nii.gz")
  file.create(empty)
  expect_equal(bidser:::nifti_nvols(c(f, empty, NA)), c(123L, NA, NA))
})

test_that("matrix cells mark missing and short runs", {
  proj <- make_qc_project()
  frame <- bidser:::bids_plot_frame(proj)
  cells <- bidser:::bids_matrix_data(frame)
  expect_true(all(c("present", "missing", "size") %in% cells$state))
  miss <- cells[cells$state == "missing", ]
  expect_equal(as.character(miss$subject), "03")
  short <- cells[cells$state == "size", ]
  expect_equal(as.character(short$subject), "04")
  expect_equal(short$nvol, 40L)
  probs <- bidser:::bids_matrix_problems(cells)
  expect_true(any(grepl("sub-03_task-rest_run-2_bold: missing", probs, fixed = TRUE)))
  expect_true(any(grepl("40 of 100 volumes", probs, fixed = TRUE)))
})

test_that("plot_bids standard mode returns a patchwork for a real project", {
  proj <- make_qc_project()
  p <- plot_bids(proj, interactive = FALSE)
  expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("plot_bids_tree folds subjects and names who is missing", {
  proj <- make_qc_project()
  p <- plot_bids_tree(proj)
  expect_s3_class(p, "gg")
  txt <- unlist(lapply(p$layers, function(l) if (is.data.frame(l$data)) l$data$txt))
  expect_true(any(grepl("7 of 8", txt)))
  expect_true(any(grepl("sub-03 run-2", txt)))
  expect_true(any(grepl("40/100 vols", txt)))
  expect_error(plot_bids_tree("nope"), "bids_project")
})

test_that("theme and palette helpers behave", {
  expect_s3_class(theme_bidser(), "theme")
  cols <- bids_datatype_colors(c("anat", "func", "weird"))
  expect_equal(names(cols), c("anat", "func", "weird"))
  expect_equal(unname(cols["weird"]), unname(bids_datatype_colors()["other"]))
  expect_equal(bidser:::format_bytes(c(950, 1.2e6, NA)), c("950 B", "1.2 MB", NA))
})
