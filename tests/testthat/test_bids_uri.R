# tests/testthat/test_bids_uri.R

library(testthat)
library(bidser)

# ---------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------

test_that("bids_uri parses empty dataset_name correctly", {
  u <- bids_uri("bids::sub-01/fmap/sub-01_epi.nii.gz")
  expect_s3_class(u, "bids_uri")
  expect_equal(u$dataset_name, "")
  expect_equal(u$relative_path, "sub-01/fmap/sub-01_epi.nii.gz")
})

test_that("bids_uri parses named dataset correctly", {
  u <- bids_uri("bids:deriv1:sub-01/anat/T1w.nii.gz")
  expect_s3_class(u, "bids_uri")
  expect_equal(u$dataset_name, "deriv1")
  expect_equal(u$relative_path, "sub-01/anat/T1w.nii.gz")
})

test_that("bids_uri stores the original uri string", {
  raw <- "bids:myds:sub-02/func/sub-02_task-rest_bold.nii.gz"
  u <- bids_uri(raw)
  expect_equal(u$uri, raw)
  expect_equal(as.character(u), raw)
})

# ---------------------------------------------------------------------------
# Error cases
# ---------------------------------------------------------------------------

test_that("path starting with / errors", {
  expect_error(
    bids_uri("bids:ds:/absolute/path.nii.gz"),
    "must not start with"
  )
})

test_that("path starting with \\ errors", {
  expect_error(
    bids_uri("bids:ds:\\windows\\path.nii.gz"),
    "must not start with"
  )
})

test_that("string not matching bids: prefix errors", {
  expect_error(bids_uri("notbids:ds:file.nii.gz"), "must start with")
})

test_that("URI missing second colon errors", {
  expect_error(bids_uri("bids:onlyone"), "two colons")
})

# ---------------------------------------------------------------------------
# Predicates and coercions
# ---------------------------------------------------------------------------

test_that("is_bids_uri returns TRUE for bids_uri and FALSE otherwise", {
  u <- bids_uri("bids::sub-01/anat/T1w.nii.gz")
  expect_true(is_bids_uri(u))
  expect_false(is_bids_uri("bids::sub-01/anat/T1w.nii.gz"))
  expect_false(is_bids_uri(42L))
})

test_that("as_bids_uri.character coerces string to bids_uri", {
  u <- as_bids_uri("bids::sub-01/func/sub-01_bold.nii.gz")
  expect_s3_class(u, "bids_uri")
})

test_that("as_bids_uri.bids_uri is identity", {
  u <- bids_uri("bids:ds:file.nii.gz")
  expect_identical(as_bids_uri(u), u)
})

# ---------------------------------------------------------------------------
# format / print
# ---------------------------------------------------------------------------

test_that("format returns bids:<name>:<path> string", {
  u <- bids_uri("bids:myds:sub-01/anat/T1w.nii.gz")
  expect_equal(format(u), "bids:myds:sub-01/anat/T1w.nii.gz")
})

test_that("print shows <bids_uri> prefix", {
  u <- bids_uri("bids::sub-01/anat/T1w.nii.gz")
  expect_output(print(u), "<bids_uri>")
})

# ---------------------------------------------------------------------------
# resolve_bids_uri with DatasetLinks
# ---------------------------------------------------------------------------

test_that("resolve_bids_uri with empty dataset_name resolves to parent_directory", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  json_path <- file.path(td, "dataset_description.json")
  jsonlite::write_json(
    list(Name = "Base", BIDSVersion = "1.9.0"),
    json_path, auto_unbox = TRUE
  )
  desc <- suppressWarnings(read_dataset_description(td))

  u        <- bids_uri("bids::sub-01/anat/T1w.nii.gz")
  resolved <- resolve_bids_uri(u, desc)
  # normalizePath(td) resolves macOS /var -> /private/var because td exists
  expected <- file.path(normalizePath(td), "sub-01/anat/T1w.nii.gz")
  expect_equal(as.character(resolved), expected)
})

test_that("resolve_bids_uri uses DatasetLinks for named dataset", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  link_dir <- tempfile()
  dir.create(link_dir)
  on.exit(unlink(link_dir, recursive = TRUE), add = TRUE)

  json_path <- file.path(td, "dataset_description.json")
  jsonlite::write_json(
    list(
      Name = "WithLinks",
      BIDSVersion = "1.9.0",
      DatasetLinks = list(deriv1 = link_dir)
    ),
    json_path, auto_unbox = TRUE
  )
  desc <- suppressWarnings(read_dataset_description(td))

  u        <- bids_uri("bids:deriv1:sub-01/anat/T1w.nii.gz")
  resolved <- resolve_bids_uri(u, desc)
  # normalizePath(link_dir) resolves macOS /var -> /private/var because link_dir exists
  expected <- file.path(normalizePath(link_dir), "sub-01/anat/T1w.nii.gz")
  expect_equal(as.character(resolved), expected)
})

test_that("resolve_bids_uri errors when DatasetLinks key is missing", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  json_path <- file.path(td, "dataset_description.json")
  jsonlite::write_json(
    list(Name = "NoLinks", BIDSVersion = "1.9.0"),
    json_path, auto_unbox = TRUE
  )
  desc <- suppressWarnings(read_dataset_description(td))

  u <- bids_uri("bids:missing:sub-01/anat/T1w.nii.gz")
  expect_error(resolve_bids_uri(u, desc), "DatasetLinks")
})

test_that("resolve_bids_uri.character coerces string before resolving", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  json_path <- file.path(td, "dataset_description.json")
  jsonlite::write_json(
    list(Name = "Base2", BIDSVersion = "1.9.0"),
    json_path, auto_unbox = TRUE
  )
  desc <- suppressWarnings(read_dataset_description(td))

  resolved <- resolve_bids_uri("bids::sub-01/anat/T1w.nii.gz", desc)
  expect_type(resolved, "character")
  expect_length(resolved, 1L)
})
