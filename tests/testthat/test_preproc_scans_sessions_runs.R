context("preproc_scans run/session filtering")
library(testthat)
library(bidser)
library(tibble)

## Helper function to create mock dataset with multiple runs and sessions
participants_df <- tibble(participant_id = "01")
file_structure_df <- tibble::tribble(
  ~subid, ~session, ~datatype, ~task,  ~run, ~suffix, ~fmriprep, ~desc,    ~space,
  "01",  "pre",  "func",    "taskA", "01", "bold", FALSE,      NA,      NA,
  "01",  "pre",  "func",    "taskA", "02", "bold", FALSE,      NA,      NA,
  "01",  "post", "func",    "taskA", "01", "bold", FALSE,      NA,      NA,
  "01",  "post", "func",    "taskA", "02", "bold", FALSE,      NA,      NA,
  # Derivatives
  "01",  "pre",  "func",    "taskA", "01", "bold", TRUE,       "preproc", "MNI",
  "01",  "pre",  "func",    "taskA", "02", "bold", TRUE,       "preproc", "MNI",
  "01",  "post", "func",    "taskA", "01", "bold", TRUE,       "preproc", "MNI",
  "01",  "post", "func",    "taskA", "02", "bold", TRUE,       "preproc", "MNI"
)

fs_for_create <- file_structure_df %>%
  mutate(suffix_ext = ifelse(suffix == "bold", "bold.nii.gz", suffix))

mock_proj <- create_mock_bids(
  project_name = "RunSessProj",
  participants = participants_df,
  file_structure = fs_for_create %>% select(-suffix) %>% rename(suffix = suffix_ext),
  prep_dir = "derivatives/mockprep"
)

expected_fn <- generate_bids_filename(
  subid = "01", session = "pre", task = "taskA", run = "02",
  desc = "preproc", space = "MNI", suffix = "bold.nii.gz"
)
expected_path <- file.path("derivatives", "mockprep", "sub-01", "ses-pre", "func", expected_fn)

test_that("run and session filters return correct file", {
  pscans <- preproc_scans(mock_proj, subid = "01", task = "taskA", run = "02",
                           session = "pre", full_path = FALSE)
  expect_equal(length(pscans), 1)
  expect_equal(pscans, expected_path)
})

# Dataset without derivatives
fs_no_prep <- file_structure_df %>%
  filter(fmriprep == FALSE) %>%
  mutate(suffix_ext = ifelse(suffix == "bold", "bold.nii.gz", suffix))
mock_proj_none <- create_mock_bids(
  project_name = "RunSessNoPrep",
  participants = participants_df,
  file_structure = fs_no_prep %>% select(-suffix) %>% rename(suffix = suffix_ext),
  prep_dir = "derivatives/mockprep"
)

test_that("message and NULL when derivatives missing", {
  expect_message(res <- preproc_scans(mock_proj_none), "derivatives enabled")
  expect_null(res)
})
