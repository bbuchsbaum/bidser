library(testthat)
library(bidser)

create_inheritance_fixture <- function(with_derivatives = FALSE) {
  tmp <- tempfile("bidser_inherit_")
  dir.create(tmp, recursive = TRUE)
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  dir.create(file.path(tmp, "sub-01", "anat"), recursive = TRUE)

  utils::write.table(
    data.frame(participant_id = "sub-01"),
    file = file.path(tmp, "participants.tsv"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  jsonlite::write_json(
    list(Name = "inherit-fixture", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  run1_bold <- file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz")
  run2_bold <- file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-02_bold.nii.gz")
  run3_bold <- file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-03_bold.nii.gz")
  anat_t1w <- file.path(tmp, "sub-01", "anat", "sub-01_T1w.nii.gz")
  file.create(run1_bold)
  file.create(run2_bold)
  file.create(run3_bold)
  file.create(anat_t1w)

  # Dataset-level sidecar
  jsonlite::write_json(
    list(
      Manufacturer = "RootVendor",
      RepetitionTime = 2.5,
      FlipAngle = 90,
      RootOnly = TRUE
    ),
    file.path(tmp, "task-rest_bold.json"),
    auto_unbox = TRUE
  )

  jsonlite::write_json(
    list(Manufacturer = "MemoryVendor", RootOnly = FALSE),
    file.path(tmp, "task-memory_bold.json"),
    auto_unbox = TRUE
  )

  # Subject-level sidecar
  jsonlite::write_json(
    list(
      RepetitionTime = 2.1,
      SliceTiming = c(0, 1, 2),
      SubjectLevel = TRUE,
      PeerConflict = "task-entity"
    ),
    file.path(tmp, "sub-01", "func", "sub-01_task-rest_bold.json"),
    auto_unbox = TRUE
  )

  # Same-depth, same-specificity sidecar to exercise deterministic conflict ordering
  jsonlite::write_json(
    list(PeerConflict = "run-entity"),
    file.path(tmp, "sub-01", "func", "sub-01_run-01_bold.json"),
    auto_unbox = TRUE
  )

  # Run-level sidecar (run-01 only)
  jsonlite::write_json(
    list(RepetitionTime = 2.0, EchoTime = 0.03, RunLevel = TRUE),
    file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.json"),
    auto_unbox = TRUE
  )

  deriv_bold <- NULL
  if (isTRUE(with_derivatives)) {
    deriv_root <- file.path(tmp, "derivatives", "mockprep")
    dir.create(file.path(deriv_root, "sub-01", "func"), recursive = TRUE)

    jsonlite::write_json(
      list(Name = "mockprep", BIDSVersion = "1.8.0", DatasetType = "derivative"),
      file.path(deriv_root, "dataset_description.json"),
      auto_unbox = TRUE,
      pretty = TRUE
    )

    deriv_bold <- file.path(
      deriv_root, "sub-01", "func",
      "sub-01_task-rest_run-01_space-MNI_desc-preproc_bold.nii.gz"
    )
    file.create(deriv_bold)

    jsonlite::write_json(
      list(RawScopeLeak = "raw-root", SharedAcrossScopes = "raw"),
      file.path(tmp, "task-rest_run-01_bold.json"),
      auto_unbox = TRUE
    )

    jsonlite::write_json(
      list(DerivativeRoot = TRUE, SharedAcrossScopes = "derivative"),
      file.path(deriv_root, "task-rest_bold.json"),
      auto_unbox = TRUE
    )

    jsonlite::write_json(
      list(DerivativeLevel = TRUE, RepetitionTime = 1.5),
      file.path(
        deriv_root, "sub-01", "func",
        "sub-01_task-rest_run-01_space-MNI_desc-preproc_bold.json"
      ),
      auto_unbox = TRUE
    )
  }

  list(
    path = tmp,
    run1 = run1_bold,
    run2 = run2_bold,
    run3 = run3_bold,
    anat = anat_t1w,
    deriv = deriv_bold
  )
}

test_that("get_metadata applies inheritance with nearest-file precedence", {
  fixture <- create_inheritance_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture$path)
  meta <- get_metadata(proj, fixture$run1, inherit = TRUE)

  expect_equal(meta$Manufacturer, "RootVendor")
  expect_true(isTRUE(meta$SubjectLevel))
  expect_true(isTRUE(meta$RunLevel))
  expect_equal(meta$EchoTime, 0.03)
  expect_equal(meta$RepetitionTime, 2.0)
  expect_equal(meta$SliceTiming, c(0, 1, 2))
  expect_equal(meta$FlipAngle, 90)
})

test_that("get_metadata falls back to less specific sidecars when run-level is absent", {
  fixture <- create_inheritance_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture$path)
  meta <- get_metadata(proj, fixture$run2, inherit = TRUE)

  expect_equal(meta$Manufacturer, "RootVendor")
  expect_true(isTRUE(meta$SubjectLevel))
  expect_null(meta$RunLevel)
  expect_equal(meta$RepetitionTime, 2.1)
})

test_that("get_metadata returns only the direct sidecar when inherit is FALSE", {
  fixture <- create_inheritance_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture$path)
  meta <- get_metadata(proj, fixture$run1, inherit = FALSE)

  expect_equal(meta$RepetitionTime, 2.0)
  expect_equal(meta$EchoTime, 0.03)
  expect_true(isTRUE(meta$RunLevel))
  expect_null(meta$Manufacturer)
  expect_null(meta$FlipAngle)
})

test_that("get_metadata returns an empty list when the direct sidecar is missing", {
  fixture <- create_inheritance_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture$path)
  meta <- get_metadata(proj, fixture$run2, inherit = FALSE)

  expect_equal(meta, list())
})

test_that("get_metadata preserves inherited keys when more specific sidecars omit them", {
  fixture <- create_inheritance_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture$path)
  meta <- get_metadata(proj, fixture$run3, inherit = TRUE)

  expect_equal(meta$Manufacturer, "RootVendor")
  expect_equal(meta$FlipAngle, 90)
  expect_true(isTRUE(meta$RootOnly))
  expect_true(isTRUE(meta$SubjectLevel))
  expect_equal(meta$RepetitionTime, 2.1)
})

test_that("get_metadata resolves peer conflicts deterministically at equal specificity", {
  fixture <- create_inheritance_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture$path)
  meta <- get_metadata(proj, fixture$run1, inherit = TRUE)

  expect_equal(meta$PeerConflict, "task-entity")
})

test_that("get_metadata ignores unrelated sidecars when no candidate matches", {
  fixture <- create_inheritance_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture$path)
  meta <- get_metadata(proj, fixture$anat, inherit = TRUE)

  expect_equal(meta, list())
})

test_that("get_metadata auto scope for derivatives stays within derivatives ancestry", {
  fixture <- create_inheritance_fixture(with_derivatives = TRUE)
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture$path, fmriprep = TRUE, prep_dir = "derivatives/mockprep")
  rel_deriv <- file.path(
    "derivatives", "mockprep", "sub-01", "func",
    "sub-01_task-rest_run-01_space-MNI_desc-preproc_bold.nii.gz"
  )

  meta <- get_metadata(proj, rel_deriv, inherit = TRUE, scope = "auto")

  expect_true(isTRUE(meta$DerivativeRoot))
  expect_true(isTRUE(meta$DerivativeLevel))
  expect_equal(meta$SharedAcrossScopes, "derivative")
  expect_null(meta$RawScopeLeak)
  expect_equal(meta$RepetitionTime, 1.5)
})

test_that("get_metadata all scope can merge raw and derivatives ancestry", {
  fixture <- create_inheritance_fixture(with_derivatives = TRUE)
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture$path, fmriprep = TRUE, prep_dir = "derivatives/mockprep")
  meta <- get_metadata(proj, fixture$deriv, inherit = TRUE, scope = "all")

  expect_equal(meta$RawScopeLeak, "raw-root")
  expect_true(isTRUE(meta$DerivativeRoot))
  expect_true(isTRUE(meta$DerivativeLevel))
  expect_equal(meta$SharedAcrossScopes, "derivative")
})

test_that("read_sidecar supports inheritance mode", {
  fixture <- create_inheritance_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture$path)

  direct <- read_sidecar(
    proj,
    subid = "01",
    task = "rest",
    run = "01",
    inherit = FALSE,
    full_path = TRUE
  )

  inherited <- read_sidecar(
    proj,
    subid = "01",
    task = "rest",
    run = "01",
    inherit = TRUE,
    full_path = TRUE
  )

  expect_equal(nrow(direct), 1)
  expect_equal(nrow(inherited), 1)

  expect_false("Manufacturer" %in% names(direct))
  expect_true("Manufacturer" %in% names(inherited))
  expect_equal(inherited$Manufacturer[[1]], "RootVendor")
  expect_equal(inherited$RepetitionTime[[1]], 2.0)
})
