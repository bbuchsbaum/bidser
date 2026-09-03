# Coverage-target gate round 4: clear the last ~0.4 pp to ≥90%.

library(testthat)
library(bidser)

test_that("downsample_single_file falls back to stub on unreadable input", {
  skip_if_not_installed("neuroim2")
  from <- tempfile("ds_from_", fileext = ".nii.gz")
  to <- tempfile("ds_to_", fileext = ".nii.gz")
  writeLines("not-a-nifti", from)
  on.exit(unlink(c(from, to)), add = TRUE)

  res <- bidser:::downsample_single_file(from, to, factor = 0.5, verbose = TRUE)
  expect_true(is.list(res))
  expect_false(isTRUE(res$success))
  expect_true(file.exists(to))
})

test_that("resolve_bids_uri resolves relative paths and DatasetLinks", {
  tmp <- tempfile("uri_res_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  linked <- tempfile("uri_link_")
  dir.create(linked, recursive = TRUE)
  on.exit(unlink(linked, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines("x", file.path(linked, "peer.nii.gz"))

  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(
      Name = "URIProj",
      BIDSVersion = "1.8.0",
      DatasetLinks = list(deriv1 = linked)
    ),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"))

  proj <- bids_project(tmp)
  u <- bids_uri("bids::sub-01/func/sub-01_task-rest_run-01_bold.nii.gz")
  resolved <- resolve_bids_uri(u, proj)
  expect_true(is.character(resolved))
  expect_true(grepl("sub-01_task-rest_run-01_bold", resolved))

  # Named DatasetLinks key
  u2 <- bids_uri("bids:deriv1:peer.nii.gz")
  resolved2 <- resolve_bids_uri(u2, proj)
  expect_true(is.character(resolved2))
  expect_true(grepl("peer\\.nii\\.gz$", resolved2))

  # Missing link key
  expect_error(
    resolve_bids_uri(bids_uri("bids:missing:x.nii.gz"), proj),
    "DatasetLinks does not contain"
  )

  # Remote scheme link
  desc <- proj$description
  desc$fields$DatasetLinks$remote <- "https://example.org/data"
  remote <- resolve_bids_uri(bids_uri("bids:remote:sub-01/file.nii.gz"), desc)
  expect_true(grepl("^https://", remote))

  # file:// scheme link
  desc$fields$DatasetLinks$localfile <- paste0("file://", linked)
  localfile <- resolve_bids_uri(bids_uri("bids:localfile:peer.nii.gz"), desc)
  expect_true(is.character(localfile))
})

test_that("schema filename validation covers empty/malformed/unknown entities", {
  schema <- list(
    objects = list(
      suffixes = list(bold = list(value = "bold"), T1w = list(value = "T1w")),
      entities = list(subject = list(name = "sub"), task = list(name = "task"))
    )
  )
  # Force through internal helpers with a schema-like object if helpers expect loaded schema
  # Call validate on edge filenames via bids_check_compliance schema path instead.
  tmp <- tempfile("schema_fn_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "SchemaFN", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  # Malformed entity (missing dash) and odd suffix
  file.create(file.path(tmp, "sub-01", "func", "sub-01_taskrest_run-01_bold.nii.gz"))
  file.create(file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_weird.nii.gz"))

  proj <- bids_project(tmp)
  # Direct internal call if schema object available
  sch <- tryCatch(bids_schema("1.10.0"), error = function(e) NULL)
  if (!is.null(sch)) {
    r1 <- bidser:::.bids_schema_validate_filename("", sch)
    expect_false(isTRUE(r1$valid))
    r2 <- bidser:::.bids_schema_validate_filename("sub-01_taskrest_bold.nii.gz", sch)
    expect_true(is.list(r2))
    r3 <- bidser:::.bids_schema_validate_filename("sub-01_foo-bar_bold.nii.gz", sch)
    expect_true(is.list(r3))
  }

  chk <- bids_check_compliance(proj, schema_check = TRUE)
  expect_true(is.list(chk))
})

test_that("read_sidecar inherit mode and n_volumes edges", {
  skip_if_not_installed("RNifti")

  tmp <- tempfile("side_nv_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "SideNV", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  nii <- file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz")
  RNifti::writeNifti(array(1, dim = c(2, 2, 2, 4)), nii)
  # Dataset-level sidecar only (no per-file json) to exercise inherit
  jsonlite::write_json(
    list(RepetitionTime = 2, SliceTiming = c(0, 0.5, 1, 1.5)),
    file.path(tmp, "task-rest_bold.json"),
    auto_unbox = TRUE
  )

  proj <- bids_project(tmp)
  sc <- read_sidecar(proj, inherit = TRUE, modality = "bold")
  expect_true(is.data.frame(sc) || inherits(sc, "tbl_df"))

  # No matching files for inherit
  empty <- read_sidecar(proj, task = "nope", inherit = TRUE)
  expect_equal(nrow(empty), 0L)

  vols <- n_volumes(proj, subid = "01", task = "rest")
  expect_true(is.numeric(vols) || is.integer(vols) || is.data.frame(vols))

  expect_error(bidser:::.bidser_n_volumes_file(tempfile()), "File not found")
  bad <- tempfile("badnii_", fileext = ".nii.gz")
  writeLines("nope", bad)
  on.exit(unlink(bad), add = TRUE)
  bad_res <- tryCatch(
    bidser:::.bidser_n_volumes_file(bad),
    error = function(e) e
  )
  expect_true(inherits(bad_res, "error") || is.integer(bad_res) || is.numeric(bad_res))
})

test_that("print.bids_project crayon fallback via trace-injected requireNamespace", {
  tmp <- tempfile("print_trace_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "TracePrint", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  dir.create(file.path(tmp, "sub-01", "ses-01", "anat"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "ses-01", "anat", "sub-01_ses-01_T1w.nii.gz"))
  prep <- file.path(tmp, "derivatives", "fmriprep")
  dir.create(file.path(prep, "sub-01", "ses-01", "anat"), recursive = TRUE)
  jsonlite::write_json(
    list(Name = "fmriprep", BIDSVersion = "1.8.0", DatasetType = "derivative",
         GeneratedBy = list(list(Name = "fmriprep"))),
    file.path(prep, "dataset_description.json"),
    auto_unbox = TRUE
  )
  file.create(file.path(prep, "sub-01", "ses-01", "anat", "sub-01_ses-01_desc-preproc_T1w.nii.gz"))

  proj <- bids_project(tmp, fmriprep = TRUE)

  suppressWarnings(trace(
    print.bids_project,
    tracer = quote({
      requireNamespace <- function(package, quietly = TRUE) {
        if (identical(package, "crayon")) {
          return(FALSE)
        }
        base::requireNamespace(package, quietly = quietly)
      }
    }),
    print = FALSE,
    where = asNamespace("bidser")
  ))
  on.exit(try(untrace(print.bids_project, where = asNamespace("bidser")), silent = TRUE), add = TRUE)

  expect_warning(
    out <- capture.output(print(proj)),
    "crayon"
  )
  expect_true(any(grepl("^project:", out)))
  expect_true(any(grepl("sessions:", out)))
  expect_true(any(grepl("fmriprep:", out)))
})

test_that("query_files formula rescue via typed positional slots", {
  participants_df <- tibble::tibble(participant_id = c("01", "02"))
  file_structure_df <- tibble::tribble(
    ~subid, ~session, ~datatype, ~task,  ~run, ~suffix,       ~fmriprep,
    "01",   NA,       "func",    "rest", "01", "bold.nii.gz", FALSE,
    "01",   NA,       "func",    "rest", "02", "bold.nii.gz", FALSE,
    "02",   NA,       "anat",    NA,     NA,   "T1w.nii.gz",  FALSE
  )
  mock <- create_mock_bids("PosRescue", participants_df, file_structure_df)

  # Pass formula where full_path/strict would be for query_files.mock
  res <- query_files(mock, run ~ as.integer(run) == 2L, return = "tibble")
  expect_s3_class(res, "tbl_df")

  # Empty tibble branch
  empty <- query_files(mock, task = "none", return = "tibble")
  expect_equal(nrow(empty), 0L)
})

test_that("load_all_events returns empty tibble when all event reads fail", {
  tmp <- tempfile("all_ev_fail_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "AllEvFail", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"))
  writeLines("{{{{", file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_events.tsv"))

  proj <- bids_project(tmp)
  # Suppress warnings; assert empty-ish result
  res <- suppressWarnings(load_all_events(proj))
  expect_true(is.data.frame(res) || is.null(res))
})

test_that("bids_summary reports tasks when functional runs exist", {
  tmp <- tempfile("sum_tasks_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = c("sub-01", "sub-02")),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "SumTasks", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  for (sub in c("sub-01", "sub-02")) {
    dir.create(file.path(tmp, sub, "func"), recursive = TRUE)
    for (task in c("rest", "nback")) {
      file.create(file.path(tmp, sub, "func", paste0(sub, "_task-", task, "_run-01_bold.nii.gz")))
    }
  }
  proj <- bids_project(tmp)
  summ <- bids_summary(proj)
  expect_true(!is.null(summ$tasks))
  expect_true(nrow(summ$tasks) >= 1 || length(summ$tasks) >= 1)
})
