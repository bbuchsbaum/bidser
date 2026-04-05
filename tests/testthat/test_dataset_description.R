# tests/testthat/test_dataset_description.R

library(testthat)
library(bidser)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_desc_dir <- function(fields) {
  td <- tempfile()
  dir.create(td)
  json_path <- file.path(td, "dataset_description.json")
  jsonlite::write_json(fields, json_path, auto_unbox = TRUE)
  td
}

# ---------------------------------------------------------------------------
# Basic read
# ---------------------------------------------------------------------------

test_that("read_dataset_description() returns bids_dataset_description", {
  td <- make_desc_dir(list(
    Name = "TestDataset",
    BIDSVersion = "1.9.0",
    DatasetType = "raw"
  ))
  on.exit(unlink(td, recursive = TRUE))

  obj <- read_dataset_description(td)
  expect_s3_class(obj, "bids_dataset_description")
})

test_that("dataset_name(), bids_version(), dataset_type() return correct values", {
  td <- make_desc_dir(list(
    Name = "MyStudy",
    BIDSVersion = "1.8.0",
    DatasetType = "raw"
  ))
  on.exit(unlink(td, recursive = TRUE))

  obj <- read_dataset_description(td)
  expect_equal(dataset_name(obj), "MyStudy")
  expect_equal(bids_version(obj), "1.8.0")
  expect_equal(dataset_type(obj), "raw")
})

# ---------------------------------------------------------------------------
# Warnings
# ---------------------------------------------------------------------------

test_that("missing Name field emits a warning", {
  td <- make_desc_dir(list(BIDSVersion = "1.9.0"))
  on.exit(unlink(td, recursive = TRUE))

  expect_warning(
    read_dataset_description(td),
    "missing 'Name'"
  )
})

test_that("missing BIDSVersion field emits a warning", {
  td <- make_desc_dir(list(Name = "NoVersion"))
  on.exit(unlink(td, recursive = TRUE))

  expect_warning(
    read_dataset_description(td),
    "missing 'BIDSVersion'"
  )
})

test_that("derivative type without GeneratedBy emits a warning", {
  td <- make_desc_dir(list(
    Name = "Deriv",
    BIDSVersion = "1.9.0",
    DatasetType = "derivative"
  ))
  on.exit(unlink(td, recursive = TRUE))

  expect_warning(
    read_dataset_description(td),
    "Derivative dataset missing 'GeneratedBy'"
  )
})

test_that("missing file returns NULL and warns", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  expect_warning(
    result <- read_dataset_description(td),
    "No dataset_description.json found"
  )
  expect_null(result)
})

# ---------------------------------------------------------------------------
# bids_version on bids_project
# ---------------------------------------------------------------------------

test_that("bids_version(proj) returns the version string from bids_project", {
  td <- make_desc_dir(list(
    Name = "ProjTest",
    BIDSVersion = "1.7.0"
  ))
  # We need a minimal valid BIDS project — use the participants.tsv approach
  # Write a minimal participants.tsv
  write.table(
    data.frame(participant_id = character(0)),
    file.path(td, "participants.tsv"),
    sep = "\t", row.names = FALSE, quote = FALSE
  )
  on.exit(unlink(td, recursive = TRUE))

  proj <- tryCatch(
    bids_project(td, strict_participants = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(proj), "Could not build minimal bids_project for version test")
  expect_equal(bids_version(proj), "1.7.0")
})

# ---------------------------------------------------------------------------
# as.list, format, print
# ---------------------------------------------------------------------------

test_that("as.list() returns the fields list", {
  td <- make_desc_dir(list(Name = "X", BIDSVersion = "1.0.0"))
  on.exit(unlink(td, recursive = TRUE))

  obj <- suppressWarnings(read_dataset_description(td))
  lst <- as.list(obj)
  expect_true(is.list(lst))
  expect_equal(lst$Name, "X")
})

test_that("format() returns a compact one-liner", {
  td <- make_desc_dir(list(Name = "Compact", BIDSVersion = "2.0.0"))
  on.exit(unlink(td, recursive = TRUE))

  obj <- suppressWarnings(read_dataset_description(td))
  f <- format(obj)
  expect_match(f, "\\[bids_dataset_description:")
  expect_match(f, "Compact")
  expect_match(f, "2.0.0")
})

test_that("print() works without error", {
  td <- make_desc_dir(list(Name = "PrintTest", BIDSVersion = "1.9.0"))
  on.exit(unlink(td, recursive = TRUE))

  obj <- suppressWarnings(read_dataset_description(td))
  expect_output(print(obj), "bids_dataset_description")
})
