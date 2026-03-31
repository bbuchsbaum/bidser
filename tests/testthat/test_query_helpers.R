library(testthat)
library(bidser)

context("Internal query helper functions (R/query.R)")

# ---------------------------------------------------------------------------
# .bidser_parse_entities_from_path
# ---------------------------------------------------------------------------

test_that(".bidser_parse_entities_from_path parses full path with all entities", {
  path <- "sub-01/ses-02/func/sub-01_ses-02_task-rest_run-01_bold.nii.gz"
  ent <- bidser:::.bidser_parse_entities_from_path(path)


  expect_equal(ent$subid, "01")
  expect_equal(ent$session, "02")
  expect_equal(ent$task, "rest")
  expect_equal(ent$run, "01")
  expect_equal(ent$kind, "bold")
})

test_that(".bidser_parse_entities_from_path handles path without session", {
  path <- "sub-03/func/sub-03_task-nback_run-02_bold.nii.gz"
  ent <- bidser:::.bidser_parse_entities_from_path(path)

  expect_equal(ent$subid, "03")
  expect_null(ent$session)
  expect_equal(ent$task, "nback")
  expect_equal(ent$run, "02")
  expect_equal(ent$kind, "bold")
})

test_that(".bidser_parse_entities_from_path handles bare filename (no directories)", {
  path <- "sub-05_task-rest_bold.nii.gz"
  ent <- bidser:::.bidser_parse_entities_from_path(path)

  expect_equal(ent$subid, "05")
  expect_equal(ent$task, "rest")
  expect_equal(ent$kind, "bold")
  # No directory components, so no directory-derived session

  expect_null(ent$session)
})

test_that(".bidser_parse_entities_from_path extracts desc and space entities", {
  path <- "sub-01/func/sub-01_task-rest_space-MNI_desc-preproc_bold.nii.gz"
  ent <- bidser:::.bidser_parse_entities_from_path(path)

  expect_equal(ent$subid, "01")
  expect_equal(ent$space, "MNI")
  expect_equal(ent$desc, "preproc")
  expect_equal(ent$kind, "bold")
  expect_equal(ent$task, "rest")
})

test_that(".bidser_parse_entities_from_path handles derivative path", {
  path <- "derivatives/fmriprep/sub-01/func/sub-01_task-rest_desc-preproc_bold.nii.gz"
  ent <- bidser:::.bidser_parse_entities_from_path(path)

  expect_equal(ent$subid, "01")
  expect_equal(ent$task, "rest")
  expect_equal(ent$desc, "preproc")
  expect_equal(ent$kind, "bold")
})

test_that(".bidser_parse_entities_from_path returns list with kind for single-token stem", {
  # e.g. "participants.tsv" => kind = "participants", no key-value pairs
  path <- "participants.tsv"
  ent <- bidser:::.bidser_parse_entities_from_path(path)

  expect_equal(ent$kind, "participants")
  expect_null(ent$subid)
})

# ---------------------------------------------------------------------------
# .bidser_extract_extension
# ---------------------------------------------------------------------------

test_that(".bidser_extract_extension handles .nii.gz", {
  expect_equal(bidser:::.bidser_extract_extension("sub-01_bold.nii.gz"), ".nii.gz")
})

test_that(".bidser_extract_extension handles .tsv.gz", {
  expect_equal(bidser:::.bidser_extract_extension("sub-01_confounds.tsv.gz"), ".tsv.gz")
})

test_that(".bidser_extract_extension handles .json", {
  expect_equal(bidser:::.bidser_extract_extension("sub-01_bold.json"), ".json")
})

test_that(".bidser_extract_extension handles .tsv", {
  expect_equal(bidser:::.bidser_extract_extension("sub-01_events.tsv"), ".tsv")
})

test_that(".bidser_extract_extension handles file within a path", {
  expect_equal(
    bidser:::.bidser_extract_extension("sub-01/func/sub-01_task-rest_bold.nii.gz"),
    ".nii.gz"
  )
})

# ---------------------------------------------------------------------------
# .bidser_extract_datatype
# ---------------------------------------------------------------------------

test_that(".bidser_extract_datatype extracts func", {
  expect_equal(bidser:::.bidser_extract_datatype("sub-01/func/file.nii.gz"), "func")
})

test_that(".bidser_extract_datatype extracts anat", {
  expect_equal(bidser:::.bidser_extract_datatype("sub-01/anat/file.nii.gz"), "anat")
})

test_that(".bidser_extract_datatype returns NA for bare filename", {
  expect_true(is.na(bidser:::.bidser_extract_datatype("file.nii.gz")))
})

test_that(".bidser_extract_datatype extracts dwi", {
  expect_equal(bidser:::.bidser_extract_datatype("sub-01/ses-01/dwi/file.nii.gz"), "dwi")
})

test_that(".bidser_extract_datatype picks last known datatype in nested path", {
  # If multiple datatypes appear in the path (unusual), the last one wins
  expect_equal(
    bidser:::.bidser_extract_datatype("derivatives/fmriprep/sub-01/func/file.nii.gz"),
    "func"
  )
})

# ---------------------------------------------------------------------------
# .bidser_normalize_filter_names
# ---------------------------------------------------------------------------

test_that(".bidser_normalize_filter_names maps sub/ses to canonical names", {
  result <- bidser:::.bidser_normalize_filter_names(list(sub = "01", ses = "02"))
  expect_equal(result[["subid"]], "01")
  expect_equal(result[["session"]], "02")
  # The function replaces aliases with canonical names; original keys are not kept
  expect_false("sub" %in% names(result))
  expect_false("ses" %in% names(result))
})

test_that(".bidser_normalize_filter_names passes through non-aliased names", {
  result <- bidser:::.bidser_normalize_filter_names(list(task = "rest", run = "01"))
  expect_equal(result$task, "rest")
  expect_equal(result$run, "01")
})

test_that(".bidser_normalize_filter_names errors on unnamed filters", {
  expect_error(
    bidser:::.bidser_normalize_filter_names(list("01", "02")),
    "must be named"
  )
})

test_that(".bidser_normalize_filter_names errors on partially named filters", {
  expect_error(
    bidser:::.bidser_normalize_filter_names(list(sub = "01", "02")),
    "must be named"
  )
})

test_that(".bidser_normalize_filter_names returns empty list for empty input", {
  result <- bidser:::.bidser_normalize_filter_names(list())
  expect_equal(length(result), 0)
})

test_that(".bidser_normalize_filter_names detects conflicting aliases", {
  expect_error(
    bidser:::.bidser_normalize_filter_names(list(sub = "01", subid = "02")),
    "Conflicting"
  )
})

# ---------------------------------------------------------------------------
# .bidser_prepare_query_filters
# ---------------------------------------------------------------------------

test_that(".bidser_prepare_query_filters wraps exact mode in anchors", {
  result <- bidser:::.bidser_prepare_query_filters(list(task = "rest"), "exact")
  expect_true(grepl("^\\^", result$task))
  expect_true(grepl("\\$$", result$task))
})

test_that(".bidser_prepare_query_filters exact mode escapes regex metacharacters", {
  result <- bidser:::.bidser_prepare_query_filters(list(desc = "a.b"), "exact")
  # The dot should be escaped so it doesn't act as regex wildcard
  expect_true(grepl("\\\\.", result$desc))
})

test_that(".bidser_prepare_query_filters passes through regex mode", {
  result <- bidser:::.bidser_prepare_query_filters(list(task = "rest.*"), "regex")
  expect_equal(result$task, "rest.*")
})

test_that(".bidser_prepare_query_filters converts glob mode", {
  result <- bidser:::.bidser_prepare_query_filters(list(task = "rest*"), "glob")
  # glob2rx("rest*") produces "^rest.*$", then anchors are stripped
  expect_true(grepl("rest", result$task))
})

test_that(".bidser_prepare_query_filters returns empty list for empty input", {
  result <- bidser:::.bidser_prepare_query_filters(list(), "exact")
  expect_equal(length(result), 0)
})

test_that(".bidser_prepare_query_filters handles NULL values in filters", {
  result <- bidser:::.bidser_prepare_query_filters(list(task = NULL), "exact")
  expect_null(result$task)
})

test_that(".bidser_prepare_query_filters exact mode handles multi-value filters", {
  result <- bidser:::.bidser_prepare_query_filters(list(task = c("rest", "nback")), "exact")
  # Should produce an alternation pattern like "^(?:rest|nback)$"
  expect_true(grepl("rest", result$task))
  expect_true(grepl("nback", result$task))
  expect_true(grepl("^\\^", result$task))
  expect_true(grepl("\\$$", result$task))
})

# ---------------------------------------------------------------------------
# .bidser_filter_extension
# ---------------------------------------------------------------------------

test_that(".bidser_filter_extension keeps matching paths", {
  paths <- c(
    "sub-01/func/sub-01_bold.nii.gz",
    "sub-01/func/sub-01_events.tsv",
    "sub-01/func/sub-01_bold.json"
  )
  result <- bidser:::.bidser_filter_extension(paths, "\\.nii\\.gz")
  expect_equal(length(result), 1)
  expect_true(grepl("bold\\.nii\\.gz$", result))
})

test_that(".bidser_filter_extension returns NULL when nothing matches", {
  paths <- c("sub-01/func/sub-01_events.tsv")
  result <- bidser:::.bidser_filter_extension(paths, "\\.nii\\.gz")
  expect_null(result)
})

test_that(".bidser_filter_extension passes through when ext_pattern is NULL", {
  paths <- c("a.nii.gz", "b.tsv")
  result <- bidser:::.bidser_filter_extension(paths, NULL)
  expect_equal(result, paths)
})

test_that(".bidser_filter_extension handles empty paths vector", {
  result <- bidser:::.bidser_filter_extension(character(0), "\\.nii\\.gz")
  expect_equal(length(result), 0)
})

# ---------------------------------------------------------------------------
# .bidser_filter_datatype
# ---------------------------------------------------------------------------

test_that(".bidser_filter_datatype keeps matching paths", {
  paths <- c(
    "sub-01/func/sub-01_bold.nii.gz",
    "sub-01/anat/sub-01_T1w.nii.gz"
  )
  result <- bidser:::.bidser_filter_datatype(paths, "func")
  expect_equal(length(result), 1)
  expect_true(grepl("func", result))
})

test_that(".bidser_filter_datatype returns NULL when nothing matches", {
  paths <- c("sub-01/anat/sub-01_T1w.nii.gz")
  result <- bidser:::.bidser_filter_datatype(paths, "func")
  expect_null(result)
})

test_that(".bidser_filter_datatype passes through when dt_pattern is NULL", {
  paths <- c("sub-01/func/file.nii.gz", "sub-01/anat/file.nii.gz")
  result <- bidser:::.bidser_filter_datatype(paths, NULL)
  expect_equal(result, paths)
})

# ---------------------------------------------------------------------------
# .bidser_to_relative_path
# ---------------------------------------------------------------------------

test_that(".bidser_to_relative_path strips project root prefix", {
  tmp <- tempfile("bidser_rel_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  sub_dir <- file.path(tmp, "sub-01", "func")
  dir.create(sub_dir, recursive = TRUE)
  f <- file.path(sub_dir, "sub-01_bold.nii.gz")
  file.create(f)

  rel <- bidser:::.bidser_to_relative_path(tmp, f)
  expect_equal(rel, "sub-01/func/sub-01_bold.nii.gz")
})

test_that(".bidser_to_relative_path returns already-relative paths unchanged", {
  rel <- bidser:::.bidser_to_relative_path("/some/root", "sub-01/func/file.nii.gz")
  expect_equal(rel, "sub-01/func/file.nii.gz")
})

test_that(".bidser_to_relative_path strips leading ./", {
  rel <- bidser:::.bidser_to_relative_path("/some/root", "./sub-01/func/file.nii.gz")
  expect_equal(rel, "sub-01/func/file.nii.gz")
})

# ---------------------------------------------------------------------------
# .bidser_is_absolute_path
# ---------------------------------------------------------------------------

test_that(".bidser_is_absolute_path detects Unix absolute paths", {
  expect_true(bidser:::.bidser_is_absolute_path("/home/user/data"))
})

test_that(".bidser_is_absolute_path detects Windows absolute paths", {
  expect_true(bidser:::.bidser_is_absolute_path("C:/Users/data"))
  expect_true(bidser:::.bidser_is_absolute_path("D:\\data\\file"))
})

test_that(".bidser_is_absolute_path rejects relative paths", {
  expect_false(bidser:::.bidser_is_absolute_path("sub-01/func/file.nii.gz"))
  expect_false(bidser:::.bidser_is_absolute_path("./relative/path"))
})

# ---------------------------------------------------------------------------
# .bidser_sort_query_tibble
# ---------------------------------------------------------------------------

test_that(".bidser_sort_query_tibble sorts by standard BIDS columns", {
  tbl <- tibble::tibble(
    subid = c("02", "01", "01"),
    session = c("01", "02", "01"),
    task = c("rest", "rest", "nback"),
    run = c("01", "01", "01"),
    path = c("c.nii.gz", "b.nii.gz", "a.nii.gz")
  )
  sorted <- bidser:::.bidser_sort_query_tibble(tbl)

  expect_equal(sorted$subid, c("01", "01", "02"))
  expect_equal(sorted$task[1:2], c("nback", "rest"))
})

test_that(".bidser_sort_query_tibble returns NULL for NULL input", {
  expect_null(bidser:::.bidser_sort_query_tibble(NULL))
})

test_that(".bidser_sort_query_tibble returns empty tibble unchanged", {
  empty <- tibble::tibble(subid = character(0), path = character(0))
  result <- bidser:::.bidser_sort_query_tibble(empty)
  expect_equal(nrow(result), 0)
})

test_that(".bidser_sort_query_tibble handles tibble without standard columns", {
  tbl <- tibble::tibble(x = c(3, 1, 2))
  result <- bidser:::.bidser_sort_query_tibble(tbl)
  # No sort columns present, should return unchanged

  expect_equal(result$x, c(3, 1, 2))
})
