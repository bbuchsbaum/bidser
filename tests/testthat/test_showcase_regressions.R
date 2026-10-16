# Regression tests for issues #80-#83 (found while building the package
# showcase).

make_showcase_mock <- function() {
  subs <- sprintf("%02d", 1:4)
  fs <- do.call(rbind, lapply(subs, function(s) {
    rbind(
      data.frame(
        subid = s, datatype = "func", task = "rest", run = c("01", "02", "03"),
        suffix = "bold.nii.gz", fmriprep = FALSE, desc = NA, space = NA
      ),
      data.frame(
        subid = s, datatype = "func", task = "rest", run = c("01", "02", "03"),
        suffix = "desc-confounds_timeseries.tsv", fmriprep = TRUE,
        desc = "confounds", space = NA
      )
    )
  }))
  conf <- list()
  for (s in subs) {
    for (r in c("01", "02", "03")) {
      conf[[sprintf(
        "derivatives/fmriprep/sub-%s/func/sub-%s_task-rest_run-%s_desc-confounds_timeseries.tsv",
        s, s, r
      )]] <- data.frame(
        csf = c(0.1, 0.2, 0.3, 0.4, 0.5),
        white_matter = c(1, 2, 3, 4, 5),
        framewise_displacement = c(NA, 0.1, 0.2, 0.3, 0.4)
      )
    }
  }
  stub <- file.path(tempfile("showcase_"), "mockds")
  create_mock_bids(
    "M", subs, fs,
    confound_data = conf, create_stub = TRUE, stub_path = stub
  )
  list(stub = stub, conf = conf)
}

# ---------------------------------------------------------------------------
# #80: bids_summary() total_runs counts runs, not distinct run labels
# ---------------------------------------------------------------------------

test_that("bids_summary counts every BOLD run across subjects (#80)", {
  m <- make_showcase_mock()
  on.exit(unlink(dirname(m$stub), recursive = TRUE), add = TRUE)
  proj <- suppressWarnings(bids_project(m$stub, fmriprep = TRUE))

  s <- bids_summary(proj)
  expect_equal(length(func_scans(proj)), 12L)
  expect_equal(s$total_runs, 12L)
  expect_equal(s$tasks$task, "rest")
  expect_equal(s$tasks$n_runs, 12L)
})

test_that("bids_summary counts sessions separately, echoes once, and ignores non-BOLD files (#80)", {
  tmp <- tempfile("sum_runs_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = c("sub-01", "sub-02")),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "Runs", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  for (sub in c("sub-01", "sub-02")) {
    for (ses in c("ses-1", "ses-2")) {
      d <- file.path(tmp, sub, ses, "func")
      dir.create(d, recursive = TRUE)
      stem <- paste0(sub, "_", ses)
      # two runs of nback, each multi-echo (2 echoes) with events + sidecar
      for (run in c("01", "02")) {
        for (echo in c("1", "2")) {
          file.create(file.path(d, sprintf(
            "%s_task-nback_run-%s_echo-%s_bold.nii.gz", stem, run, echo
          )))
        }
        file.create(file.path(d, sprintf("%s_task-nback_run-%s_events.tsv", stem, run)))
        file.create(file.path(d, sprintf("%s_task-nback_run-%s_bold.json", stem, run)))
      }
      # one rest scan without a run entity
      file.create(file.path(d, paste0(stem, "_task-rest_bold.nii.gz")))
    }
  }

  proj <- bids_project(tmp, fmriprep = FALSE)
  s <- bids_summary(proj)
  tasks <- s$tasks[order(s$tasks$task), ]
  expect_equal(tasks$task, c("nback", "rest"))
  # nback: 2 subjects x 2 sessions x 2 runs; rest: 2 subjects x 2 sessions
  expect_equal(tasks$n_runs, c(8L, 4L))
  expect_equal(s$total_runs, 12L)
})

# ---------------------------------------------------------------------------
# #81: read_confounds()/variables_table() do not leak readr messages
# ---------------------------------------------------------------------------

test_that("read_confounds and variables_table do not print readr column specs (#81)", {
  m <- make_showcase_mock()
  on.exit(unlink(dirname(m$stub), recursive = TRUE), add = TRUE)
  proj <- suppressWarnings(bids_project(m$stub, fmriprep = TRUE))

  msgs <- character(0)
  collect <- function(expr) {
    withCallingHandlers(expr, message = function(cnd) {
      msgs <<- c(msgs, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    })
  }
  cf <- collect(read_confounds(proj, subid = "01"))
  expect_no_message(read_confounds(proj, subid = "01"))
  vt <- collect(variables_table(proj, subid = "01"))

  expect_false(any(grepl("Column specification|show_col_types|Rows: ", msgs)))
  expect_true(nrow(cf) > 0)
})

# ---------------------------------------------------------------------------
# #82: mock stubs include a derivative dataset_description.json
# ---------------------------------------------------------------------------

test_that("create_mock_bids stub writes derivative dataset_description.json (#82)", {
  m <- make_showcase_mock()
  on.exit(unlink(dirname(m$stub), recursive = TRUE), add = TRUE)

  deriv_desc <- file.path(m$stub, "derivatives", "fmriprep", "dataset_description.json")
  expect_true(file.exists(deriv_desc))

  dd <- expect_no_warning(read_dataset_description(dirname(deriv_desc)))
  expect_equal(dataset_type(dd), "derivative")
  expect_true(length(generated_by(dd)) >= 1)

  expect_no_warning(bids_project(m$stub, fmriprep = TRUE))
})

test_that("create_mock_bids writes confound_data contents to the stub (#82, #83)", {
  m <- make_showcase_mock()
  on.exit(unlink(dirname(m$stub), recursive = TRUE), add = TRUE)
  f <- file.path(m$stub, names(m$conf)[1])
  expect_gt(file.size(f), 0)
  back <- readr::read_tsv(f, na = "n/a", show_col_types = FALSE)
  expect_equal(names(back), names(m$conf[[1]]))
})

test_that("create_mock_bids stub without derivatives writes no derivative description (#82)", {
  stub <- file.path(tempfile("rawonly_"), "ds")
  on.exit(unlink(dirname(stub), recursive = TRUE), add = TRUE)
  fs <- data.frame(
    subid = "01", datatype = "func", task = "rest", run = "01",
    suffix = "bold.nii.gz", fmriprep = FALSE
  )
  create_mock_bids("R", "01", fs, create_stub = TRUE, stub_path = stub)
  expect_false(dir.exists(file.path(stub, "derivatives")))
})

# ---------------------------------------------------------------------------
# #83: one definition and one documented default for bids_check_compliance()
# ---------------------------------------------------------------------------

rd_text <- function(topic) {
  # Source tree (devtools::test) first, then the installed help database
  # (R CMD check).
  src <- testthat::test_path("..", "..", "man", paste0(topic, ".Rd"))
  rd <- if (file.exists(src)) {
    tools::parse_Rd(src)
  } else {
    root <- find.package("bidser")
    tools::Rd_db("bidser", lib.loc = dirname(root))[[paste0(topic, ".Rd")]]
  }
  paste(as.character(rd), collapse = "")
}

test_that("bids_check_compliance has a single definition and consistent docs (#83)", {
  f <- bidser::bids_check_compliance
  default <- formals(f)$schema_version
  expect_equal(default, "1.10.0")
  expect_true(default %in% bids_schema_versions())
  expect_false(any(grepl("UseMethod", deparse(body(f)))))

  txt <- rd_text("bids_check_compliance")
  # exactly one usage entry, and no stale default anywhere in the help page
  usage_hits <- gregexpr("bids_check_compliance(x, schema_check = TRUE",
    txt,
    fixed = TRUE
  )[[1]]
  expect_equal(sum(usage_hits > 0), 1L)
  expect_false(grepl("1.10.1", txt, fixed = TRUE))
  expect_true(grepl("schema_checked", txt, fixed = TRUE))
})

test_that("bids_summary has a single usage entry (#83)", {
  txt <- rd_text("bids_summary")
  usage_hits <- gregexpr("bids_summary(x)", txt, fixed = TRUE)[[1]]
  expect_equal(sum(usage_hits > 0), 1L)
})
