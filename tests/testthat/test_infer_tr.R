context("infer_tr")

library(testthat)
library(bidser)

# ---------- JSON with RepetitionTime ----------

test_that("infer_tr reads RepetitionTime from JSON", {
  tmp_json <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_json), add = TRUE)

  jsonlite::write_json(list(RepetitionTime = 2.0), tmp_json, auto_unbox = TRUE)
  tr <- infer_tr(tmp_json)

  expect_equal(as.numeric(tr), 2.0)
  expect_equal(attr(tr, "source"), "json:RepetitionTime")
  expect_equal(attr(tr, "path"), tmp_json)
})

# ---------- JSON with VolumeTiming ----------

test_that("infer_tr computes TR from VolumeTiming", {
  tmp_json <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_json), add = TRUE)

  jsonlite::write_json(list(VolumeTiming = c(0, 2, 4, 6, 8)),
                       tmp_json, auto_unbox = FALSE)
  tr <- infer_tr(tmp_json)

  expect_equal(as.numeric(tr), 2.0)
  expect_equal(attr(tr, "source"), "json:VolumeTiming")
  expect_equal(attr(tr, "path"), tmp_json)
})

# ---------- Variable VolumeTiming ----------

test_that("infer_tr flags variable VolumeTiming", {
  tmp_json <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_json), add = TRUE)

  # Non-uniform spacing: diffs are 2, 3, 2, 3
  jsonlite::write_json(list(VolumeTiming = c(0, 2, 5, 7, 10)),
                       tmp_json, auto_unbox = FALSE)
  tr <- infer_tr(tmp_json)

  expect_true(is.numeric(tr))
  expect_equal(attr(tr, "source"), "json:VolumeTiming")
  expect_true(isTRUE(attr(tr, "variable")))
})

# ---------- NIfTI path with companion .json ----------

test_that("infer_tr resolves JSON sidecar for NIfTI path", {
  tmp_dir <- tempfile("infer_tr_nii_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  nii_path <- file.path(tmp_dir, "sub-01_task-rest_bold.nii.gz")
  json_path <- file.path(tmp_dir, "sub-01_task-rest_bold.json")

  # Create a stub NIfTI file (content doesn't matter for JSON resolution)
  writeLines("stub", nii_path)
  jsonlite::write_json(list(RepetitionTime = 1.5), json_path, auto_unbox = TRUE)

  tr <- infer_tr(nii_path)

  expect_equal(as.numeric(tr), 1.5)
  expect_equal(attr(tr, "source"), "json:RepetitionTime")
})

# ---------- Non-existent file ----------

test_that("infer_tr errors on non-existent file", {
  expect_error(infer_tr("/no/such/file.json"), "File not found")
})

# ---------- Multiple file paths ----------

test_that("infer_tr errors on multiple file paths", {
  tmp1 <- tempfile(fileext = ".json")
  tmp2 <- tempfile(fileext = ".json")
  on.exit(unlink(c(tmp1, tmp2)), add = TRUE)

  writeLines("{}", tmp1)
  writeLines("{}", tmp2)

  expect_error(infer_tr(c(tmp1, tmp2)), "expects a single file path")
})

# ---------- coerce_units auto: millisecond-like TR ----------

test_that("coerce_units='auto' converts millisecond TR to seconds", {
  tmp_json <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_json), add = TRUE)

  jsonlite::write_json(list(RepetitionTime = 2000), tmp_json, auto_unbox = TRUE)
  tr <- infer_tr(tmp_json, coerce_units = "auto")

  expect_equal(as.numeric(tr), 2.0)
  expect_equal(attr(tr, "unit"), "ms->s")
  expect_equal(attr(tr, "source"), "json:RepetitionTime")
})

# ---------- coerce_units strict: millisecond-like TR left as-is ----------

test_that("coerce_units='strict' leaves millisecond-like TR unchanged", {
  tmp_json <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_json), add = TRUE)

  jsonlite::write_json(list(RepetitionTime = 2000), tmp_json, auto_unbox = TRUE)
  tr <- infer_tr(tmp_json, coerce_units = "strict")

  expect_equal(as.numeric(tr), 2000)
  expect_null(attr(tr, "unit"))
})

# ---------- SBRef file returns NA ----------

test_that("infer_tr returns NA for SBRef files", {
  tmp_dir <- tempfile("infer_tr_sbref_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sbref_path <- file.path(tmp_dir, "sub-01_task-rest_sbref.nii.gz")
  writeLines("stub", sbref_path)

  tr <- infer_tr(sbref_path, verbose = FALSE)
  expect_true(is.na(tr))
})

test_that("infer_tr emits message for SBRef when verbose", {
  tmp_dir <- tempfile("infer_tr_sbref_v_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sbref_path <- file.path(tmp_dir, "sub-01_task-rest_sbref.nii.gz")
  writeLines("stub", sbref_path)

  expect_message(infer_tr(sbref_path, verbose = TRUE), "SBRef")
})

# ---------- JSON missing both RepetitionTime and VolumeTiming ----------

test_that("infer_tr returns NA when JSON lacks TR fields", {
  tmp_json <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_json), add = TRUE)

  jsonlite::write_json(list(TaskName = "rest", Manufacturer = "Siemens"),
                       tmp_json, auto_unbox = TRUE)
  tr <- infer_tr(tmp_json)

  expect_true(is.na(tr))
})

# ---------- infer_tr.bids_project dispatches to get_repetition_time ----------

test_that("infer_tr.bids_project dispatches to get_repetition_time", {
  tmp <- tempfile("bidser_tr_proj_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "TRTest", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)

  bold_name <- "sub-01_task-rest_run-01_bold.nii.gz"
  json_name <- "sub-01_task-rest_run-01_bold.json"
  writeLines("stub", file.path(tmp, "sub-01", "func", bold_name))
  jsonlite::write_json(list(RepetitionTime = 0.8),
                       file.path(tmp, "sub-01", "func", json_name),
                       auto_unbox = TRUE)

  proj <- bids_project(tmp)

  # infer_tr on a bids_project should call get_repetition_time
  tr <- infer_tr(proj, subid = "01", task = "rest", run = "01")
  expect_equal(as.numeric(tr), 0.8)
})

# ---------- coerce_units auto: millisecond VolumeTiming ----------

test_that("coerce_units='auto' converts millisecond VolumeTiming diffs", {
  tmp_json <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_json), add = TRUE)

  # diffs are 500 ms each -> should convert to 0.5 s
  jsonlite::write_json(list(VolumeTiming = c(0, 500, 1000, 1500)),
                       tmp_json, auto_unbox = FALSE)
  tr <- infer_tr(tmp_json, coerce_units = "auto")

  expect_equal(as.numeric(tr), 0.5)
  expect_equal(attr(tr, "unit"), "ms->s")
})
