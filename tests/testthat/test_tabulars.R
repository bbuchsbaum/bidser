# tests/testthat/test_tabulars.R

library(testthat)
library(bidser)
library(tibble)
library(dplyr)

# ---------------------------------------------------------------------------
# Helper: minimal BIDS project dir with participants.tsv
# ---------------------------------------------------------------------------

make_bids_dir_with_participants <- function(rows = NULL, sidecar = NULL) {
  td <- tempfile()
  dir.create(td)

  # dataset_description.json
  jsonlite::write_json(
    list(Name = "TabTest", BIDSVersion = "1.9.0"),
    file.path(td, "dataset_description.json"),
    auto_unbox = TRUE
  )

  # participants.tsv
  if (is.null(rows)) {
    rows <- data.frame(
      participant_id = c("sub-01", "sub-02"),
      age = c(25L, 30L),
      stringsAsFactors = FALSE
    )
  }
  readr::write_tsv(rows, file.path(td, "participants.tsv"), na = "n/a")

  # optional sidecar JSON
  if (!is.null(sidecar)) {
    jsonlite::write_json(sidecar, file.path(td, "participants.json"),
                         auto_unbox = TRUE)
  }

  td
}

# ---------------------------------------------------------------------------
# read_participants
# ---------------------------------------------------------------------------

test_that("read_participants returns bids_participants inheriting tbl_df", {
  td <- make_bids_dir_with_participants()
  on.exit(unlink(td, recursive = TRUE))

  proj <- suppressWarnings(bids_project(td, strict_participants = FALSE))
  result <- read_participants(proj)

  expect_s3_class(result, "bids_participants")
  expect_s3_class(result, "tbl_df")
})

test_that("read_participants.character accepts a project directory", {
  td <- make_bids_dir_with_participants()
  on.exit(unlink(td, recursive = TRUE))

  result <- read_participants(td)
  expect_s3_class(result, "bids_participants")
  expect_s3_class(result, "tbl_df")
})

test_that("missing participants.tsv returns NULL with message", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  expect_message(
    result <- read_participants(td),
    "participants.tsv"
  )
  expect_null(result)
})

# ---------------------------------------------------------------------------
# sidecar()
# ---------------------------------------------------------------------------

test_that("sidecar() returns list() when no sidecar file", {
  td <- make_bids_dir_with_participants()
  on.exit(unlink(td, recursive = TRUE))

  result <- read_participants(td)
  expect_equal(sidecar(result), list())
})

test_that("sidecar() returns metadata when sidecar JSON exists", {
  td <- make_bids_dir_with_participants(
    sidecar = list(
      participant_id = list(Description = "Unique participant label"),
      age = list(Description = "Age in years", Units = "years")
    )
  )
  on.exit(unlink(td, recursive = TRUE))

  result <- read_participants(td)
  sc <- sidecar(result)
  expect_true(is.list(sc))
  expect_true(length(sc) > 0L)
  expect_true("age" %in% names(sc))
})

# ---------------------------------------------------------------------------
# dplyr integration (tibble inheritance)
# ---------------------------------------------------------------------------

test_that("dplyr::filter works on bids_participants", {
  td <- make_bids_dir_with_participants()
  on.exit(unlink(td, recursive = TRUE))

  result <- read_participants(td)
  filtered <- dplyr::filter(result, participant_id == "sub-01")
  expect_equal(nrow(filtered), 1L)
})

# ---------------------------------------------------------------------------
# as_tibble strips subclass
# ---------------------------------------------------------------------------

test_that("as_tibble() strips bids_participants subclass", {
  td <- make_bids_dir_with_participants()
  on.exit(unlink(td, recursive = TRUE))

  result <- read_participants(td)
  plain <- tibble::as_tibble(result)
  expect_false(inherits(plain, "bids_participants"))
  expect_s3_class(plain, "tbl_df")
})

# ---------------------------------------------------------------------------
# print header line
# ---------------------------------------------------------------------------

test_that("print shows header line with class name", {
  td <- make_bids_dir_with_participants()
  on.exit(unlink(td, recursive = TRUE))

  result <- read_participants(td)
  expect_output(print(result), "<bids_participants>")
})

# ---------------------------------------------------------------------------
# Missing scans.tsv → NULL + message
# ---------------------------------------------------------------------------

test_that("read_scans_tsv returns NULL with message when file is missing", {
  td <- make_bids_dir_with_participants()
  on.exit(unlink(td, recursive = TRUE))

  proj <- suppressWarnings(bids_project(td, strict_participants = FALSE))
  expect_message(
    result <- read_scans_tsv(proj, subid = "01"),
    "scans.tsv"
  )
  expect_null(result)
})

# ---------------------------------------------------------------------------
# Missing sessions.tsv → NULL + message
# ---------------------------------------------------------------------------

test_that("read_sessions_tsv returns NULL with message when file is missing", {
  td <- make_bids_dir_with_participants()
  on.exit(unlink(td, recursive = TRUE))

  proj <- suppressWarnings(bids_project(td, strict_participants = FALSE))
  expect_message(
    result <- read_sessions_tsv(proj, subid = "01"),
    "sessions.tsv"
  )
  expect_null(result)
})
