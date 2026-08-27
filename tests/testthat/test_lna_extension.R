library(testthat)
library(bidser)

make_lna_exchange_fixture <- function(root) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  writeLines("LNA extension fixture", file.path(root, "README"))
  readr::write_tsv(
    tibble::tibble(participant_id = c("sub-01", "sub-02")),
    file.path(root, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "LNA fixture", BIDSVersion = "1.11.1"),
    file.path(root, "dataset_description.json"),
    auto_unbox = TRUE
  )

  raw_dir <- file.path(root, "sub-01", "ses-01", "func")
  other_dir <- file.path(root, "sub-02", "func")
  deriv_root <- file.path(root, "derivatives", "lna")
  deriv_dir <- file.path(deriv_root, "sub-01", "ses-01", "func")
  dir.create(raw_dir, recursive = TRUE)
  dir.create(other_dir, recursive = TRUE)
  dir.create(deriv_dir, recursive = TRUE)
  jsonlite::write_json(
    list(
      Name = "LNA derivative",
      BIDSVersion = "1.11.1",
      DatasetType = "derivative",
      GeneratedBy = list(list(Name = "neuroarchive"))
    ),
    file.path(deriv_root, "dataset_description.json"),
    auto_unbox = TRUE
  )

  raw <- file.path(
    raw_dir,
    "sub-01_ses-01_task-rest_acq-mb_run-01_echo-2_bold.lna.h5"
  )
  other <- file.path(other_dir, "sub-02_task-nback_run-02_bold.lna.h5")
  derivative <- file.path(
    deriv_dir,
    paste0(
      "sub-01_ses-01_task-rest_acq-mb_run-01_echo-2_",
      "space-MNI152NLin2009cAsym_desc-preproc_bold.lna.h5"
    )
  )
  file.create(raw, other, derivative)

  list(
    root = root,
    raw = gsub("\\\\", "/", substring(raw, nchar(root) + 2L)),
    other = gsub("\\\\", "/", substring(other, nchar(root) + 2L)),
    derivative = gsub("\\\\", "/", substring(derivative, nchar(root) + 2L))
  )
}

test_that(".lna.h5 is parsed as one extensible compound extension", {
  path <- paste0(
    "sub-01/ses-01/func/",
    "sub-01_ses-01_task-rest_run-01_echo-2_bold.lna.h5"
  )
  entity <- bids_entities(path)

  expect_equal(entity$kind, "bold")
  expect_equal(entity$suffix, "lna.h5")
  expect_equal(entity$extension, ".lna.h5")
  expect_equal(entity$datatype, "func")
  expect_equal(entity$type, "func")

  bare <- bids_entities(basename(path))
  expect_equal(bare$kind, "bold")
  expect_equal(bare$extension, ".lna.h5")
  expect_true(is.na(bare$datatype))
  expect_true(is.na(bare$type))

  old_options <- options(bidser.compound_extensions = ".archive.h5")
  on.exit(options(old_options), add = TRUE)
  expect_equal(
    bidser:::.bidser_extract_extension("sub-01_bold.archive.h5"),
    ".archive.h5"
  )
})

test_that("LNA public entities, index, and query rows use one derivation", {
  fixture_root <- tempfile("bidser_lna_")
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)
  fixture <- make_lna_exchange_fixture(fixture_root)
  project <- bids_project(fixture$root, derivatives = "auto")
  index <- bids_index(project, rebuild = TRUE, persist = FALSE)

  indexed <- query_files(
    project,
    regex = "\\.lna\\.h5$",
    scope = "all",
    return = "tibble",
    use_index = "auto"
  )
  filesystem <- query_files(
    project,
    regex = "\\.lna\\.h5$",
    scope = "all",
    return = "tibble",
    use_index = "never"
  )
  expect_equal(indexed, filesystem)
  expect_setequal(indexed$path, c(fixture$raw, fixture$other, fixture$derivative))

  public <- bids_entities(fixture$derivative, include_path = FALSE, coerce = FALSE)
  indexed_row <- index[index$path == fixture$derivative, ]
  query_row <- indexed[indexed$path == fixture$derivative, ]
  fields <- c(
    "subid", "session", "task", "run", "echo", "space", "desc",
    "kind", "suffix", "type", "extension", "datatype"
  )
  for (field in fields) {
    expect_equal(as.character(public[[field]]), as.character(indexed_row[[field]]))
    expect_equal(as.character(indexed_row[[field]]), as.character(query_row[[field]]))
  }
})

test_that("exact LNA queries work through all entity aliases", {
  fixture_root <- tempfile("bidser_lna_")
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)
  fixture <- make_lna_exchange_fixture(fixture_root)
  project <- bids_project(fixture$root, derivatives = "auto")

  result <- query_files(
    project,
    regex = "\\.lna\\.h5$",
    match_mode = "exact",
    scope = "derivatives",
    sub = "01",
    ses = "01",
    task = "rest",
    acq = "mb",
    run = "01",
    echo = "2",
    space = "MNI152NLin2009cAsym",
    desc = "preproc",
    kind = "bold",
    suffix = "lna.h5",
    type = "func",
    extension = ".lna.h5",
    datatype = "func"
  )
  expect_identical(result, fixture$derivative)

  raw <- query_files(
    project,
    regex = "\\.lna\\.h5$",
    match_mode = "exact",
    scope = "raw",
    subid = "01",
    session = "01",
    task = "rest",
    run = "01",
    echo = "2",
    kind = "bold",
    extension = ".lna.h5",
    datatype = "func"
  )
  expect_identical(raw, fixture$raw)
})

test_that("standard and registered datatype parsing does not regress", {
  expect_equal(
    bidser:::.bidser_extract_extension("sub-01_bold.nii.gz"),
    ".nii.gz"
  )
  expect_equal(
    bidser:::.bidser_extract_extension("sub-01_events.tsv.gz"),
    ".tsv.gz"
  )
  expect_equal(
    bidser:::.bidser_extract_extension("sub-01_from-a_to-b_xfm.h5"),
    ".h5"
  )
  expect_equal(
    bidser:::.bidser_parse_entities_from_path("sub-01_from-a_to-b_xfm.h5")$kind,
    "xfm"
  )

  name <- paste0("custom_", sample.int(1e7, 1L))
  folder <- paste0("folder_", sample.int(1e7, 1L))
  on.exit(try(unregister_datatype(name), silent = TRUE), add = TRUE)
  register_datatype(
    name,
    spec = func_spec(),
    parser_fn = func_parser(),
    folder = folder,
    scope = "both"
  )
  expect_equal(
    bidser:::.bidser_extract_datatype(file.path("sub-01", folder, "file.h5")),
    folder
  )
})
