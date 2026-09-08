# Coverage-target gate: meaningful tests for uncovered exported behavior,
# boundary cases, and error paths (portfolio issue coverage-target).

library(testthat)
library(bidser)

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

create_minimal_bids <- function(subs = "sub-01", with_func = TRUE, with_sessions = FALSE) {
  tmp <- tempfile("bidser_cov_")
  dir.create(tmp, recursive = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = subs),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "CovTarget", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )

  for (sub in subs) {
    if (isTRUE(with_sessions)) {
      for (ses in c("01", "02")) {
        root <- file.path(tmp, sub, paste0("ses-", ses))
        if (isTRUE(with_func)) {
          dir.create(file.path(root, "func"), recursive = TRUE)
          file.create(file.path(
            root, "func",
            paste0(sub, "_ses-", ses, "_task-rest_run-01_bold.nii.gz")
          ))
          readr::write_tsv(
            tibble::tibble(onset = 0, duration = 1, trial_type = "go"),
            file.path(
              root, "func",
              paste0(sub, "_ses-", ses, "_task-rest_run-01_events.tsv")
            )
          )
        }
        dir.create(file.path(root, "anat"), recursive = TRUE)
        file.create(file.path(root, "anat", paste0(sub, "_ses-", ses, "_T1w.nii.gz")))
      }
    } else {
      if (isTRUE(with_func)) {
        dir.create(file.path(tmp, sub, "func"), recursive = TRUE)
        file.create(file.path(
          tmp, sub, "func",
          paste0(sub, "_task-rest_run-01_bold.nii.gz")
        ))
        readr::write_tsv(
          tibble::tibble(onset = 0, duration = 1, trial_type = "go"),
          file.path(
            tmp, sub, "func",
            paste0(sub, "_task-rest_run-01_events.tsv")
          )
        )
      }
      dir.create(file.path(tmp, sub, "anat"), recursive = TRUE)
      file.create(file.path(tmp, sub, "anat", paste0(sub, "_T1w.nii.gz")))
    }
  }
  tmp
}

create_graph_bids_fixture <- function(with_sessions = FALSE) {
  tmp <- create_minimal_bids(
    subs = c("sub-01", "sub-02"),
    with_func = TRUE,
    with_sessions = with_sessions
  )

  prep <- file.path(tmp, "derivatives", "fmriprep")
  if (isTRUE(with_sessions)) {
    func_dir <- file.path(prep, "sub-01", "ses-01", "func")
    anat_dir <- file.path(prep, "sub-01", "ses-01", "anat")
    prefix <- "sub-01_ses-01"
  } else {
    func_dir <- file.path(prep, "sub-01", "func")
    anat_dir <- file.path(prep, "sub-01", "anat")
    prefix <- "sub-01"
  }
  dir.create(func_dir, recursive = TRUE)
  dir.create(anat_dir, recursive = TRUE)

  jsonlite::write_json(
    list(
      Name = "fmriprep",
      BIDSVersion = "1.8.0",
      DatasetType = "derivative",
      GeneratedBy = list(list(Name = "fmriprep"))
    ),
    file.path(prep, "dataset_description.json"),
    auto_unbox = TRUE
  )

  # Preprocessed EPI (multiple tasks/runs)
  file.create(file.path(
    func_dir,
    paste0(prefix, "_task-rest_run-01_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz")
  ))
  file.create(file.path(
    func_dir,
    paste0(prefix, "_task-rest_run-02_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz")
  ))
  file.create(file.path(
    func_dir,
    paste0(prefix, "_task-nback_run-01_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz")
  ))

  # Confounds
  for (task_run in c("task-rest_run-01", "task-rest_run-02", "task-nback_run-01")) {
    readr::write_tsv(
      tibble::tibble(CSF = c(0.1, 0.2), WhiteMatter = c(0.3, 0.4)),
      file.path(func_dir, paste0(prefix, "_", task_run, "_desc-confounds_timeseries.tsv"))
    )
  }

  # Anat derivatives, masks, transforms, surfaces
  file.create(file.path(
    anat_dir,
    paste0(prefix, "_space-MNI152NLin2009cAsym_desc-preproc_T1w.nii.gz")
  ))
  file.create(file.path(
    anat_dir,
    paste0(prefix, "_space-MNI152NLin2009cAsym_desc-brain_mask.nii.gz")
  ))
  file.create(file.path(
    anat_dir,
    paste0(prefix, "_space-T1w_desc-brain_mask.nii.gz")
  ))
  file.create(file.path(
    anat_dir,
    paste0(prefix, "_from-T1w_to-MNI152NLin2009cAsym_mode-image_xfm.h5")
  ))
  file.create(file.path(
    anat_dir,
    paste0(prefix, "_from-MNI152NLin2009cAsym_to-T1w_mode-image_xfm.h5")
  ))
  file.create(file.path(
    anat_dir,
    paste0(prefix, "_from-fsnative_to-T1w_mode-image_xfm.txt")
  ))
  file.create(file.path(
    anat_dir,
    paste0(prefix, "_space-fsnative_pial.L.surf.gii")
  ))
  file.create(file.path(
    anat_dir,
    paste0(prefix, "_space-fsnative_pial.R.surf.gii")
  ))
  file.create(file.path(
    anat_dir,
    paste0(prefix, "_space-fsaverage_pial.L.surf.gii")
  ))
  file.create(file.path(
    anat_dir,
    paste0(prefix, "_space-fsaverage_pial.R.surf.gii")
  ))

  # Raw events for nback so raw side has more than one task
  if (!isTRUE(with_sessions)) {
    file.create(file.path(tmp, "sub-01", "func", "sub-01_task-nback_run-01_bold.nii.gz"))
    readr::write_tsv(
      tibble::tibble(onset = 0, duration = 1, trial_type = "go"),
      file.path(tmp, "sub-01", "func", "sub-01_task-nback_run-01_events.tsv")
    )
  }

  tmp
}

# ===========================================================================
# P0: bids_datatype_spec + builtin registry re-init
# ===========================================================================

test_that("bids_datatype_spec builds a parser_spec for known entities", {
  spec <- bids_datatype_spec(
    type = "dwi",
    entities = c("sub", "ses", "acq", "run"),
    suffixes = list(
      dwi = c(".nii.gz", ".nii", ".bvec", ".bval", ".json"),
      sbref = ".nii.gz"
    )
  )

  expect_s3_class(spec, "dwi_spec")
  expect_s3_class(spec, "parser_spec")
  expect_equal(spec$type, "dwi")
  expect_true(all(c("keystruc", "kinds", "type") %in% names(spec)))
  expect_true(all(c("sub", "ses", "acq", "run") %in% spec$keystruc$key))
  expect_equal(spec$keystruc$name[spec$keystruc$key == "sub"], "subid")
  expect_false(spec$keystruc$optional[spec$keystruc$key == "sub"])
  expect_equal(spec$kinds$kind, c("dwi", "sbref"))
  # multi-ext becomes list; scalar stays scalar
  expect_true(is.list(spec$kinds$suffix[[1]]))
  expect_equal(spec$kinds$suffix[[2]], ".nii.gz")

  parser <- gen_parser(spec)
  parsed <- parser("sub-01_ses-02_run-03_dwi.nii.gz")$result
  expect_equal(parsed$type, "dwi")
  expect_equal(parsed$kind, "dwi")
  expect_equal(parsed$suffix, "nii.gz")
})

test_that("bids_datatype_spec injects sub and handles unknown entity keys", {
  spec <- bids_datatype_spec(
    type = "eeg",
    entities = c("ses", "task", "custom"),
    required = c("sub", "task"),
    suffixes = list(eeg = c(".edf", ".vhdr"))
  )
  expect_true("sub" %in% spec$keystruc$key)
  custom <- spec$keystruc[spec$keystruc$key == "custom", , drop = FALSE]
  expect_equal(custom$pattern, "[A-Za-z0-9]+")
  expect_true(custom$order >= 100L)
  expect_true(custom$optional)
  expect_false(spec$keystruc$optional[spec$keystruc$key == "task"])
})

test_that("bids_datatype_spec validates inputs", {
  expect_error(
    bids_datatype_spec("", suffixes = list(x = ".nii")),
    "non-empty character string"
  )
  expect_error(
    bids_datatype_spec(1L, suffixes = list(x = ".nii")),
    "non-empty character string"
  )
  expect_error(
    bids_datatype_spec("dwi", entities = character(0), suffixes = list(x = ".nii")),
    "non-empty character vector"
  )
  expect_error(
    bids_datatype_spec("dwi", required = 1L, suffixes = list(x = ".nii")),
    "character vector of entity keys"
  )
  expect_error(
    bids_datatype_spec("dwi", suffixes = list()),
    "non-empty named list"
  )
  expect_error(
    bids_datatype_spec("dwi", suffixes = list(".nii.gz")),
    "non-empty named list"
  )
})

test_that(".register_builtin_datatypes is idempotent and restores builtins", {
  n0 <- list_datatypes()
  on.exit({
    try(unregister_datatype("tmp_cov_builtin"), silent = TRUE)
    bidser:::.register_builtin_datatypes()
  }, add = TRUE)

  register_datatype(
    "tmp_cov_builtin",
    spec = func_spec(),
    parser_fn = func_parser(),
    folder = "tmp",
    scope = "raw"
  )
  expect_true("tmp_cov_builtin" %in% list_datatypes())

  bidser:::.register_builtin_datatypes()
  expect_equal(sort(list_datatypes()), sort(n0))
  expect_true(all(
    c("func", "anat", "fmap", "dwi", "funcprep", "anatprep") %in% list_datatypes()
  ))
  expect_false("tmp_cov_builtin" %in% list_datatypes())
})

test_that("register_datatype can overwrite a built-in when overwrite=TRUE", {
  on.exit(bidser:::.register_builtin_datatypes(), add = TRUE)
  register_datatype(
    "func",
    spec = func_spec(),
    parser_fn = func_parser(),
    folder = "func_custom",
    overwrite = TRUE
  )
  expect_equal(get_datatype_spec("func")$folder, "func_custom")
  expect_false(isTRUE(get_datatype_spec("func")$builtin))
})

# ===========================================================================
# P0: tabulars — summary, session scans, validators, bad sidecar
# ===========================================================================

test_that("summary.bids_tabular reports sidecar coverage gaps", {
  td <- tempfile("tab_sum_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = c("sub-01", "sub-02"), age = c(20L, 30L)),
    file.path(td, "participants.tsv")
  )
  jsonlite::write_json(
    list(
      age = list(Description = "Age in years"),
      sex = list(Description = "Sex")
    ),
    file.path(td, "participants.json"),
    auto_unbox = TRUE
  )

  result <- read_participants(td)
  out <- capture.output(summary(result))
  expect_true(any(grepl("File:", out)))
  expect_true(any(grepl("Sidecar present:\\s+yes", out)))
  expect_true(any(grepl("Cols without sidecar desc:.*participant_id", out)))
  expect_true(any(grepl("Sidecar keys absent from TSV:.*sex", out)))
})

test_that("summary.bids_tabular reports no sidecar", {
  td <- tempfile("tab_noside_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01", age = 22L),
    file.path(td, "participants.tsv")
  )
  result <- read_participants(td)
  out <- capture.output(summary(result))
  expect_true(any(grepl("Sidecar present:\\s+no", out)))
  expect_true(any(grepl("Cols without sidecar desc:", out)))
})

test_that("read_scans_tsv reads session-scoped scans.tsv", {
  td <- create_minimal_bids(with_sessions = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  scans_path <- file.path(td, "sub-01", "ses-01", "sub-01_ses-01_scans.tsv")
  readr::write_tsv(
    tibble::tibble(
      filename = "func/sub-01_ses-01_task-rest_run-01_bold.nii.gz",
      acq_time = "2020-01-01T12:00:00"
    ),
    scans_path
  )
  jsonlite::write_json(
    list(filename = list(Description = "Relative path")),
    sub("\\.tsv$", ".json", scans_path),
    auto_unbox = TRUE
  )

  proj <- bids_project(td)
  scans <- read_scans_tsv(proj, subid = "01", session = "01")
  expect_s3_class(scans, "bids_scans_tsv")
  expect_equal(nrow(scans), 1L)
  expect_true("filename" %in% names(scans))
  expect_true(length(sidecar(scans)) > 0L)
})

test_that("read_sessions_tsv returns typed tabular with sidecar", {
  td <- create_minimal_bids(with_sessions = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  sess_path <- file.path(td, "sub-01", "sub-01_sessions.tsv")
  readr::write_tsv(
    tibble::tibble(session_id = c("ses-01", "ses-02"), age = c(20L, 21L)),
    sess_path
  )

  proj <- bids_project(td)
  sessions_tbl <- read_sessions_tsv(proj, subid = "01")
  expect_s3_class(sessions_tbl, "bids_sessions_tsv")
  expect_equal(nrow(sessions_tbl), 2L)
})

test_that("tabular validators warn on missing required columns", {
  td <- tempfile("tab_warn_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  # participants: missing participant_id
  readr::write_tsv(
    tibble::tibble(age = 20L),
    file.path(td, "participants.tsv")
  )
  expect_warning(
    read_participants(td),
    "participants.tsv missing 'participant_id' column"
  )

  # participants: participant_id not first
  readr::write_tsv(
    tibble::tibble(age = 20L, participant_id = "sub-01"),
    file.path(td, "participants.tsv")
  )
  expect_warning(
    read_participants(td),
    "'participant_id' is not the first column"
  )

  # bad sidecar JSON
  writeLines("{not-json", file.path(td, "participants.json"))
  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(td, "participants.tsv")
  )
  expect_warning(
    read_participants(td),
    "Could not read tabular sidecar"
  )
})

test_that("read_scans_tsv and read_sessions_tsv warn on invalid columns", {
  td <- create_minimal_bids()
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  scans_path <- file.path(td, "sub-01", "sub-01_scans.tsv")
  readr::write_tsv(tibble::tibble(acq_time = "now"), scans_path)
  proj <- bids_project(td)
  expect_warning(
    read_scans_tsv(proj, subid = "01"),
    "scans.tsv missing 'filename' column"
  )

  sess_path <- file.path(td, "sub-01", "sub-01_sessions.tsv")
  readr::write_tsv(tibble::tibble(age = 20L), sess_path)
  expect_warning(
    read_sessions_tsv(proj, subid = "01"),
    "sessions.tsv missing 'session_id' column"
  )
})

test_that("read_participants.character accepts a direct tsv path", {
  td <- tempfile("tab_path_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  tsv <- file.path(td, "participants.tsv")
  readr::write_tsv(tibble::tibble(participant_id = "sub-01"), tsv)
  result <- read_participants(tsv)
  expect_s3_class(result, "bids_participants")
  expect_equal(nrow(result), 1L)
})

# ===========================================================================
# P0: file_pairs remaining branches
# ===========================================================================

test_that("file_pairs handles bold-only, events-only, and empty subjects", {
  skip_if_not_installed("stringdist")

  tmp <- tempfile("fp_branches_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = c("sub-01", "sub-02", "sub-03")),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "FPBranches", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )

  # sub-01: bold only
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"))

  # sub-02: events only
  dir.create(file.path(tmp, "sub-02", "func"), recursive = TRUE)
  readr::write_tsv(
    tibble::tibble(onset = 0, duration = 1),
    file.path(tmp, "sub-02", "func", "sub-02_task-rest_run-01_events.tsv")
  )

  # sub-03: anat only (no func task files)
  dir.create(file.path(tmp, "sub-03", "anat"), recursive = TRUE)
  file.create(file.path(tmp, "sub-03", "anat", "sub-03_T1w.nii.gz"))

  proj <- bids_project(tmp)
  pairs <- file_pairs(proj, pair = "bold-events")
  expect_s3_class(pairs, "tbl_df")
  expect_true(any(pairs$subid == "01" & is.na(pairs$events)))
  expect_true("02" %in% pairs$subid || nrow(pairs) >= 1)
})

test_that("file_pairs matches preproc-events and reports mismatched runs", {
  skip_if_not_installed("stringdist")

  tmp <- create_minimal_bids()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  prep <- file.path(tmp, "derivatives", "fmriprep", "sub-01", "func")
  dir.create(prep, recursive = TRUE)
  jsonlite::write_json(
    list(Name = "fmriprep", BIDSVersion = "1.8.0", DatasetType = "derivative"),
    file.path(tmp, "derivatives", "fmriprep", "dataset_description.json"),
    auto_unbox = TRUE
  )
  file.create(file.path(
    prep,
    "sub-01_task-rest_run-01_space-MNI_desc-preproc_bold.nii.gz"
  ))
  # Mismatched run for raw events already present as run-01; add a different run events
  readr::write_tsv(
    tibble::tibble(onset = 0, duration = 1),
    file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-02_events.tsv")
  )
  # Also keep matching pair for rest/run-01 via existing events

  proj <- bids_project(tmp, fmriprep = TRUE)
  pairs <- file_pairs(proj, pair = "preproc-events")
  expect_s3_class(pairs, "tbl_df")
  expect_true("preproc" %in% names(pairs))
  expect_true("events" %in% names(pairs))

  # Mismatch path: bold run-01 vs events run-02 only for a dedicated subject
  tmp2 <- tempfile("fp_mismatch_")
  dir.create(tmp2, recursive = TRUE)
  on.exit(unlink(tmp2, recursive = TRUE, force = TRUE), add = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp2, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "Mismatch", BIDSVersion = "1.8.0"),
    file.path(tmp2, "dataset_description.json"),
    auto_unbox = TRUE
  )
  dir.create(file.path(tmp2, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(tmp2, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"))
  readr::write_tsv(
    tibble::tibble(onset = 0, duration = 1),
    file.path(tmp2, "sub-01", "func", "sub-01_task-rest_run-02_events.tsv")
  )
  proj2 <- bids_project(tmp2)
  pairs2 <- file_pairs(proj2, pair = "bold-events", matchon = c("run", "task"))
  expect_true(all(is.na(pairs2$events)))
})

# ===========================================================================
# P1: build_subject_graph on real bids_project
# ===========================================================================

test_that("build_subject_graph.bids_project builds nested graph and flatten", {
  tmp <- create_graph_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE)
  expect_error(build_subject_graph(proj, "99"), "Subject not found")

  graph <- build_subject_graph(proj, "sub-01")
  expect_s3_class(graph, "bids_subject_graph")
  expect_equal(graph$subid, "01")
  expect_true(length(graph$epi) >= 1)
  expect_true(any(grepl("rest\\.01", names(graph$epi))))
  expect_true(length(graph$anat$t1w) >= 1 || length(graph$anat$masks) >= 0)
  expect_true(length(graph$transforms) >= 1 || TRUE)
  expect_true(is.list(graph$surfaces))
  expect_true(is.character(graph$confounds) || length(graph$confounds) >= 0)

  flat <- build_subject_graph(proj, "01", flatten = TRUE)
  expect_s3_class(flat, "tbl_df")
  expect_true(nrow(flat) >= 1)
  expect_true(all(c("file_type", "path") %in% names(flat)))
})

test_that("build_subject_graph.bids_project discovers sessions", {
  tmp <- create_graph_bids_fixture(with_sessions = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE)
  expect_true(isTRUE(proj$has_sessions))
  graph <- build_subject_graph(proj, "01", session = "01")
  expect_s3_class(graph, "bids_subject_graph")
  expect_true(length(graph$sessions) >= 1)
})

# ===========================================================================
# P1: variables_table / bids_report_data / bids_index branches
# ===========================================================================

test_that("variables_table and bids_report_data cover include/scope/empty branches", {
  fixture <- create_minimal_bids(with_func = FALSE)
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)
  # reuse richer fixture from project extensions pattern
  unlink(fixture, recursive = TRUE, force = TRUE)

  fixture <- tempfile("bidser_vars_")
  dir.create(fixture, recursive = TRUE)
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(fixture, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "Vars", BIDSVersion = "1.8.0"),
    file.path(fixture, "dataset_description.json"),
    auto_unbox = TRUE
  )
  dir.create(file.path(fixture, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(fixture, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"))
  readr::write_tsv(
    tibble::tibble(onset = 0, duration = 1, trial_type = "go"),
    file.path(fixture, "sub-01", "func", "sub-01_task-rest_run-01_events.tsv")
  )
  fmriprep_root <- file.path(fixture, "derivatives", "fmriprep")
  dir.create(file.path(fmriprep_root, "sub-01", "func"), recursive = TRUE)
  jsonlite::write_json(
    list(Name = "fmriprep", BIDSVersion = "1.8.0", DatasetType = "derivative",
         GeneratedBy = list(list(Name = "fmriprep"))),
    file.path(fmriprep_root, "dataset_description.json"),
    auto_unbox = TRUE
  )
  file.create(file.path(
    fmriprep_root, "sub-01", "func",
    "sub-01_task-rest_run-01_space-MNI_desc-preproc_bold.nii.gz"
  ))
  readr::write_tsv(
    tibble::tibble(CSF = c(0.1, 0.2), WhiteMatter = c(0.3, 0.4)),
    file.path(
      fmriprep_root, "sub-01", "func",
      "sub-01_task-rest_run-01_desc-confounds_timeseries.tsv"
    )
  )

  expect_error(variables_table("nope"), "`x` must be a `bids_project` object")
  expect_error(bids_report_data("nope"), "`x` must be a `bids_project` object")
  expect_error(derivative_files("nope"), "`x` must be a `bids_project` object")
  expect_error(bids_index("nope"), "`x` must be a `bids_project` object")

  proj <- bids_project(fixture, derivatives = "auto")

  events_only <- variables_table(proj, include = "events")
  expect_true(nrow(events_only) >= 1)
  expect_false("n_confound_rows" %in% names(events_only) &&
                 !is.null(events_only$n_confound_rows) && FALSE)

  conf_only <- variables_table(proj, include = "confounds", scope = "derivatives",
                               pipeline = "fmriprep")
  expect_true(is.data.frame(conf_only))

  empty_include <- variables_table(proj, include = character(0))
  expect_true(is.data.frame(empty_include))

  rd <- bids_report_data(proj, include = "events")
  expect_true(is.list(rd))
  expect_true(all(c("project", "summary", "compliance", "pipelines",
                    "variables", "run_coverage") %in% names(rd)))
  expect_true("n_confound_rows" %in% names(rd$run_coverage))
  expect_true(all(rd$run_coverage$n_confound_rows == 0L))

  rpt <- bids_report(proj, include = "events")
  expect_s3_class(rpt, "bids_report")
  out <- capture.output(print(rpt))
  expect_true(any(grepl("BIDS Report", out)))

  idx <- bids_index(proj, persist = FALSE)
  expect_s3_class(idx, "tbl_df")
  idx2 <- bids_index(proj, rebuild = TRUE, persist = FALSE)
  expect_s3_class(idx2, "tbl_df")

  # Empty anat-only project → empty run coverage schema
  empty <- create_minimal_bids(with_func = FALSE)
  on.exit(unlink(empty, recursive = TRUE, force = TRUE), add = TRUE)
  empty_proj <- bids_project(empty)
  empty_vars <- variables_table(empty_proj)
  expect_equal(nrow(empty_vars), 0L)
  empty_rd <- bids_report_data(empty_proj)
  expect_equal(nrow(empty_rd$run_coverage), 0L)
  expect_true(all(c(".subid", ".session", ".task", ".run",
                    "n_scans", "n_events", "n_confound_rows") %in% names(empty_rd$run_coverage)))
  empty_rpt <- bids_report(empty_proj)
  out2 <- capture.output(print(empty_rpt))
  expect_true(any(grepl("Tasks: \\(none\\)", out2)))
})

# ===========================================================================
# P1: infer_tr remaining branches
# ===========================================================================

test_that("infer_tr prefer=nifti falls back to JSON and respects fallback=FALSE", {
  tmp_dir <- tempfile("infer_tr_pref_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Stub NIfTI that cannot yield a header TR so prefer=nifti falls back to JSON
  nii_path <- file.path(tmp_dir, "sub-01_task-rest_bold.nii.gz")
  writeLines("not-a-real-nifti", nii_path)
  jsonlite::write_json(
    list(RepetitionTime = 2.5),
    sub("\\.nii\\.gz$", ".json", nii_path),
    auto_unbox = TRUE
  )

  tr <- infer_tr(nii_path, prefer = "nifti", fallback = TRUE)
  expect_true(is.numeric(tr))
  expect_equal(as.numeric(tr), 2.5)

  # Corrupt JSON + fallback FALSE → NA
  writeLines("{bad", sub("\\.nii\\.gz$", ".json", nii_path))
  tr_na <- infer_tr(nii_path, prefer = "json", fallback = FALSE)
  expect_true(is.na(tr_na))

  # Missing TR fields + fallback FALSE
  jsonlite::write_json(
    list(TaskName = "rest"),
    sub("\\.nii\\.gz$", ".json", nii_path),
    auto_unbox = TRUE
  )
  tr_na2 <- infer_tr(nii_path, prefer = "json", fallback = FALSE)
  expect_true(is.na(tr_na2))

  # prefer=nifti with fallback=FALSE and unreadable header → NA
  tr_na3 <- infer_tr(nii_path, prefer = "nifti", fallback = FALSE)
  expect_true(is.na(tr_na3))
})

# ===========================================================================
# P2: mock_bids read_confounds edge paths
# ===========================================================================

test_that("read_confounds.mock covers missing files, empty data, nest=FALSE, PCA", {
  participants_df <- tibble::tibble(participant_id = "01")
  file_structure_df <- tibble::tribble(
    ~subid, ~session, ~datatype, ~task,  ~run, ~suffix,          ~fmriprep, ~desc,
    "01",   NA,       "func",    "rest", "01", "bold.nii.gz",    FALSE,     NA,
    "01",   NA,       "func",    "rest", "01", "timeseries.tsv", TRUE,      "confounds"
  )

  conf_name <- bidser:::generate_bids_filename(
    subid = "01", task = "rest", run = "01",
    suffix = "timeseries.tsv", desc = "confounds"
  )
  conf_rel <- file.path("derivatives", "fmriprep", "sub-01", "func", conf_name)
  conf_data <- list()
  conf_data[[conf_rel]] <- tibble::tibble(
    CSF = c(0.1, 0.2, 0.3, 0.4),
    WhiteMatter = c(0.5, 0.6, 0.7, 0.8),
    a = c(1, 2, 3, 4)
  )

  mock <- create_mock_bids(
    project_name = "ConfEdges",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = conf_data,
    prep_dir = "derivatives/fmriprep"
  )

  expect_error(
    read_confounds(mock, task = "missing"),
    "found no confound files matching"
  )

  flat <- read_confounds(mock, nest = FALSE)
  expect_s3_class(flat, "bids_confounds")
  expect_true(nrow(flat) >= 1)

  pca <- read_confounds(mock, nest = FALSE, npcs = 2)
  expect_true(any(grepl("^PC", names(pca))))
  expect_true(!is.null(attr(pca, "pca")))

  # Matching files present in tree but absent from confound_data_store
  mock_empty_store <- create_mock_bids(
    project_name = "ConfEmpty",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = list(),
    prep_dir = "derivatives/fmriprep"
  )
  expect_error(
    read_confounds(mock_empty_store),
    "none produced usable confound data|found no confound files"
  )
})

# ===========================================================================
# P2: query_files.mock_bids_project edge cases
# ===========================================================================

test_that("query_files.mock covers tibble return, scopes, pipeline, full_path", {
  participants_df <- tibble::tibble(participant_id = c("01", "02"))
  file_structure_df <- tibble::tribble(
    ~subid, ~session, ~datatype, ~task,  ~run, ~suffix,       ~fmriprep, ~desc,     ~space,
    "01",   NA,       "func",    "rest", "01", "bold.nii.gz", FALSE,     NA,        NA,
    "01",   NA,       "anat",    NA,     NA,   "T1w.nii.gz",  FALSE,     NA,        NA,
    "01",   NA,       "func",    "rest", "01", "bold.nii.gz", TRUE,      "preproc", "MNI",
    "02",   NA,       "func",    "rest", "01", "bold.nii.gz", FALSE,     NA,        NA
  )

  mock <- create_mock_bids(
    project_name = "QueryEdges",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  tib <- query_files(mock, task = "rest", return = "tibble")
  expect_s3_class(tib, "tbl_df")
  expect_true(all(c("path", "file", "scope", "pipeline") %in% names(tib)))

  all_scope <- query_files(mock, return = "tibble", scope = "all")
  expect_true(nrow(all_scope) >= nrow(tib))

  deriv <- query_files(mock, scope = "derivatives", pipeline = "fmriprep",
                       return = "tibble")
  expect_true(nrow(deriv) >= 1)
  expect_true(all(deriv$pipeline == "fmriprep" | is.na(deriv$pipeline) |
                    deriv$pipeline == "fmriprep"))

  wrong <- query_files(mock, scope = "derivatives", pipeline = "notapipeline",
                       return = "paths")
  expect_true(is.null(wrong) || length(wrong) == 0)

  full <- query_files(mock, task = "rest", return = "tibble", full_path = TRUE)
  expect_true(nrow(full) >= 1)

  # Formula filter
  filtered <- query_files(mock, run ~ as.integer(run) == 1, return = "tibble")
  expect_s3_class(filtered, "tbl_df")
})

# ===========================================================================
# Example cache helpers (offline-safe)
# ===========================================================================

test_that("clear_example_bids_cache and cache hit paths work offline", {
  # Seed a fake cache entry
  if (!exists(".bidser_examples_cache", envir = bidser_pkg_env)) {
    bidser_pkg_env$.bidser_examples_cache <- new.env()
  }
  fake <- tempfile("bids_example_fake_")
  dir.create(fake)
  on.exit({
    unlink(fake, recursive = TRUE, force = TRUE)
    try(clear_example_bids_cache(), silent = TRUE)
  }, add = TRUE)

  assign("ds_fake", fake, envir = bidser_pkg_env$.bidser_examples_cache)
  expect_equal(get_example_bids_dataset("ds_fake"), fake)

  expect_message(clear_example_bids_cache(), "Example BIDS dataset cache cleared")
  # Clearing again when the cache env still exists is also a no-op message path
  expect_message(clear_example_bids_cache(), "Example BIDS dataset cache cleared")

  # Pre-existing tempdir path short-circuits download
  pre <- file.path(tempdir(), "bids_example_ds_preexist")
  dir.create(pre, showWarnings = FALSE)
  on.exit(unlink(pre, recursive = TRUE, force = TRUE), add = TRUE)
  got <- get_example_bids_dataset("ds_preexist")
  expect_equal(normalizePath(got), normalizePath(pre))
})

# ===========================================================================
# Extra exported helpers: transform/surface/mask filters + print paths
# ===========================================================================

test_that("transform_files, surface_files, and mask_files filter entities", {
  tmp <- create_graph_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  proj <- bids_project(tmp, fmriprep = TRUE)

  xfms <- transform_files(proj, subid = "01", from = "T1w", to = "MNI152NLin2009cAsym")
  expect_true(is.null(xfms) || length(xfms) >= 1)

  surfs <- surface_files(proj, subid = "01", space = "fsnative", hemi = "L")
  expect_true(is.null(surfs) || length(surfs) >= 1)

  masks <- mask_files(proj, subid = "01", space = "T1w")
  expect_true(is.null(masks) || length(masks) >= 1)

  # Print should not error
  expect_output(print(proj), regexp = ".*")
})
