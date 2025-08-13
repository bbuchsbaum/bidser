test_that("pack_bids creates tar.gz archive with stub files", {
  skip_if_offline()
  
  # Create a test BIDS project
  proj <- tryCatch({
    ds_path <- get_example_bids_dataset("ds001")
    bids_project(ds_path)
  }, error = function(e) {
    skip("Could not download example dataset")
  })
  
  # Pack the project
  output_file <- tempfile(fileext = ".tar.gz")
  result <- pack_bids(proj, output_file = output_file, verbose = FALSE)
  
  # Check that archive was created
  expect_true(file.exists(result))
  expect_equal(normalizePath(result), normalizePath(output_file))
  
  # Check archive contents
  contents <- list_pack_bids(result, verbose = FALSE)
  expect_s3_class(contents, "data.frame")
  expect_true(nrow(contents) > 0)
  
  # Check that imaging files are stubs
  imaging_files <- contents[contents$type == "imaging", ]
  if (nrow(imaging_files) > 0) {
    expect_true(all(imaging_files$size == 0))
    expect_true(all(imaging_files$is_stub))
  }
  
  # Check that metadata files exist (not all may have content)
  non_imaging_files <- contents[contents$type != "imaging", ]
  expect_true(nrow(non_imaging_files) > 0)
  
  # Clean up
  unlink(output_file)
  unlink(dirname(proj$path), recursive = TRUE)
})

test_that("pack_bids creates zip archive", {
  skip_if_offline()
  
  # Create a test BIDS project
  proj <- tryCatch({
    ds_path <- get_example_bids_dataset("ds001")
    bids_project(ds_path)
  }, error = function(e) {
    skip("Could not download example dataset")
  })
  
  # Pack as zip
  output_file <- tempfile(fileext = ".zip")
  result <- pack_bids(proj, output_file = output_file, format = "zip", verbose = FALSE)
  
  # Check that archive was created
  expect_true(file.exists(result))
  expect_equal(normalizePath(result), normalizePath(output_file))
  
  # Check it's a valid zip file
  zip_info <- unzip(result, list = TRUE)
  expect_true(nrow(zip_info) > 0)
  
  # Clean up
  unlink(output_file)
  unlink(dirname(proj$path), recursive = TRUE)
})

test_that("pack_bids handles exclude derivatives option", {
  # Create a simple mock project with derivatives
  participants_df <- tibble::tibble(participant_id = c("01", "02"))
  
  file_structure_df <- tibble::tribble(
    ~subid, ~session, ~datatype, ~suffix,       ~fmriprep,
    "01",   NA,       "anat",    "T1w.nii.gz",  FALSE,
    "01",   NA,       "func",    "bold.nii.gz", FALSE,
    "01",   NA,       "func",    "bold.nii.gz", TRUE
  )
  
  # Create temporary directory for mock BIDS project
  temp_bids_dir <- tempfile("mock_bids_")
  
  proj <- create_mock_bids(
    project_name = "TestProject",
    participants = participants_df,
    file_structure = file_structure_df,
    create_stub = TRUE,
    stub_path = temp_bids_dir
  )
  
  # Pack without derivatives
  output_file <- tempfile(fileext = ".tar.gz")
  result <- pack_bids(proj, output_file = output_file, 
                     include_derivatives = FALSE, verbose = FALSE)
  
  expect_true(file.exists(result))
  
  # Check contents don't include derivatives
  contents <- list_pack_bids(result, verbose = FALSE)
  expect_false(any(grepl("derivatives/", contents$file)))
  
  # Clean up
  unlink(output_file)
  unlink(temp_bids_dir, recursive = TRUE)
})

test_that("pack_bids validates input", {
  # Test with non-bids_project object
  expect_error(pack_bids(list()), "must be a bids_project or mock_bids_project object")
  
  # Test with invalid format - create minimal mock project
  participants_df <- tibble::tibble(participant_id = "01")
  file_structure_df <- tibble::tribble(
    ~subid, ~datatype, ~suffix,       ~fmriprep,
    "01",   "anat",    "T1w.nii.gz",  FALSE
  )
  
  # Create temporary directory for mock BIDS project
  temp_bids_dir <- tempfile("mock_bids_")
  
  proj <- create_mock_bids(
    project_name = "TestProject",
    participants = participants_df,
    file_structure = file_structure_df,
    create_stub = TRUE,
    stub_path = temp_bids_dir
  )
  
  expect_error(pack_bids(proj, format = "invalid"), "Format must be")
  
  # Clean up
  unlink(temp_bids_dir, recursive = TRUE)
})

test_that("pack_bids handles cleanup option", {
  # Create minimal mock project
  participants_df <- tibble::tibble(participant_id = "01")
  file_structure_df <- tibble::tribble(
    ~subid, ~datatype, ~suffix,       ~fmriprep,
    "01",   "func",    "bold.nii.gz", FALSE
  )
  
  # Create temporary directory for mock BIDS project
  temp_bids_dir <- tempfile("mock_bids_")
  
  proj <- create_mock_bids(
    project_name = "TestProject",
    participants = participants_df,
    file_structure = file_structure_df,
    create_stub = TRUE,
    stub_path = temp_bids_dir
  )
  
  # Create with cleanup = FALSE
  temp_dir <- tempfile("pack_test_")
  dir.create(temp_dir)
  
  output_file <- tempfile(fileext = ".tar.gz")
  result <- pack_bids(proj, output_file = output_file, 
                     temp_dir = temp_dir, cleanup = FALSE, verbose = FALSE)
  
  # Check that temp files exist
  temp_contents <- list.files(temp_dir, recursive = TRUE)
  expect_true(length(temp_contents) > 0)
  
  # Clean up manually
  unlink(output_file)
  unlink(temp_dir, recursive = TRUE)
  unlink(temp_bids_dir, recursive = TRUE)
})

test_that("list_pack_bids provides accurate summary", {
  skip_if_offline()
  
  proj <- tryCatch({
    ds_path <- get_example_bids_dataset("ds001")
    bids_project(ds_path)
  }, error = function(e) {
    skip("Could not download example dataset")
  })
  
  # Create archive
  output_file <- tempfile(fileext = ".tar.gz")
  pack_bids(proj, output_file = output_file, verbose = FALSE)
  
  # Capture verbose output
  output <- capture.output(contents <- list_pack_bids(output_file, verbose = TRUE))
  
  # Check output contains expected information
  expect_true(any(grepl("Total files:", output)))
  expect_true(any(grepl("Stub imaging files:", output)))
  
  # Clean up
  unlink(output_file)
  unlink(dirname(proj$path), recursive = TRUE)
})

test_that("pack_bids preserves directory structure", {
  # Create a mock project with specific structure
  participants_df <- tibble::tibble(participant_id = c("01", "02"))
  
  file_structure_df <- tibble::tribble(
    ~subid, ~session, ~datatype, ~task,   ~run, ~suffix,       ~fmriprep,
    "01",   "test",   "anat",    NA,      NA,   "T1w.nii.gz",  FALSE,
    "01",   "test",   "func",    "rest",  "01", "bold.nii.gz", FALSE,
    "02",   "test",   "anat",    NA,      NA,   "T1w.nii.gz",  FALSE,
    "02",   "test",   "func",    "task",  "01", "bold.nii.gz", FALSE
  )
  
  # Create temporary directory for mock BIDS project
  temp_bids_dir <- tempfile("mock_bids_")
  
  proj <- create_mock_bids(
    project_name = "TestProject",
    participants = participants_df,
    file_structure = file_structure_df,
    create_stub = TRUE,
    stub_path = temp_bids_dir
  )
  
  output_file <- tempfile(fileext = ".tar.gz")
  pack_bids(proj, output_file = output_file, verbose = FALSE)
  
  # Extract and check structure
  temp_extract <- tempfile("extract_")
  dir.create(temp_extract)
  untar(output_file, exdir = temp_extract)
  
  # Check that files exist with expected structure
  extracted_files <- list.files(temp_extract, recursive = TRUE)
  expect_true(any(grepl("sub-01", extracted_files)))
  expect_true(any(grepl("sub-02", extracted_files)))
  expect_true(any(grepl("ses-test", extracted_files)))
  
  # Clean up
  unlink(output_file)
  unlink(temp_extract, recursive = TRUE)
  unlink(temp_bids_dir, recursive = TRUE)
})

test_that("pack_bids handles empty projects gracefully", {
  # Create minimal project with just participants
  participants_df <- tibble::tibble(participant_id = "01")
  
  # Minimal file structure
  file_structure_df <- tibble::tribble(
    ~subid, ~datatype, ~suffix,       ~fmriprep,
    "01",   "anat",    "T1w.nii.gz",  FALSE
  )
  
  # Create temporary directory for mock BIDS project
  temp_bids_dir <- tempfile("mock_bids_")
  
  proj <- create_mock_bids(
    project_name = "MinimalProject",
    participants = participants_df,
    file_structure = file_structure_df,
    create_stub = TRUE,
    stub_path = temp_bids_dir
  )
  
  output_file <- tempfile(fileext = ".tar.gz")
  result <- pack_bids(proj, output_file = output_file, verbose = FALSE)
  
  expect_true(file.exists(result))
  
  # Should have some files
  contents <- list_pack_bids(result, verbose = FALSE)
  expect_true(nrow(contents) > 0)
  
  unlink(output_file)
  unlink(temp_bids_dir, recursive = TRUE)
})

test_that("pack_bids with downsampling creates reduced resolution files", {
  skip_if_not_installed("neuroim2")
  
  # Create a simple mock project
  participants_df <- tibble::tibble(participant_id = c("01", "02"))
  
  file_structure_df <- tibble::tribble(
    ~subid, ~session, ~datatype, ~suffix,       ~fmriprep,
    "01",   NA,       "anat",    "T1w.nii.gz",  FALSE,
    "01",   NA,       "func",    "bold.nii.gz", FALSE,
    "02",   NA,       "anat",    "T1w.nii.gz",  FALSE
  )
  
  # Create temporary directory for mock BIDS project
  temp_bids_dir <- tempfile("mock_bids_")
  
  proj <- create_mock_bids(
    project_name = "TestProject",
    participants = participants_df,
    file_structure = file_structure_df,
    create_stub = TRUE,
    stub_path = temp_bids_dir
  )
  
  # Pack with downsampling
  output_file <- tempfile(fileext = ".tar.gz")
  result <- pack_bids(proj, output_file = output_file, 
                     downsample_factor = 0.5, 
                     verbose = FALSE)
  
  expect_true(file.exists(result))
  
  # Check contents have resolution tags
  contents <- list_pack_bids(result, verbose = FALSE)
  
  # Check for downsampled files with res- tag
  downsampled_files <- contents[contents$is_downsampled, ]
  expect_true(all(grepl("_res-", downsampled_files$file)))
  
  # Check that downsampled files have non-zero size (but smaller than original)
  expect_true(all(downsampled_files$size > 0))
  
  # Clean up
  unlink(output_file)
  unlink(temp_bids_dir, recursive = TRUE)
})

test_that("pack_bids downsampling validates factor parameter", {
  participants_df <- tibble::tibble(participant_id = "01")
  file_structure_df <- tibble::tribble(
    ~subid, ~datatype, ~suffix,       ~fmriprep,
    "01",   "anat",    "T1w.nii.gz",  FALSE
  )
  
  temp_bids_dir <- tempfile("mock_bids_")
  
  proj <- create_mock_bids(
    project_name = "TestProject",
    participants = participants_df,
    file_structure = file_structure_df,
    create_stub = TRUE,
    stub_path = temp_bids_dir
  )
  
  # Test invalid factor values
  expect_error(pack_bids(proj, downsample_factor = 0), 
               "downsample_factor must be between 0 .* and 1")
  expect_error(pack_bids(proj, downsample_factor = 1.5), 
               "downsample_factor must be between 0 .* and 1")
  expect_error(pack_bids(proj, downsample_factor = "invalid"), 
               "downsample_factor must be a single numeric value")
  
  # Clean up
  unlink(temp_bids_dir, recursive = TRUE)
})

test_that("pack_bids parallel processing works with downsampling", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  
  # Create a project with multiple files
  participants_df <- tibble::tibble(participant_id = c("01", "02", "03"))
  
  file_structure_df <- tibble::tribble(
    ~subid, ~datatype, ~suffix,       ~fmriprep,
    "01",   "anat",    "T1w.nii.gz",  FALSE,
    "01",   "func",    "bold.nii.gz", FALSE,
    "02",   "anat",    "T1w.nii.gz",  FALSE,
    "02",   "func",    "bold.nii.gz", FALSE,
    "03",   "anat",    "T1w.nii.gz",  FALSE,
    "03",   "func",    "bold.nii.gz", FALSE
  )
  
  temp_bids_dir <- tempfile("mock_bids_")
  
  proj <- create_mock_bids(
    project_name = "TestProject",
    participants = participants_df,
    file_structure = file_structure_df,
    create_stub = TRUE,
    stub_path = temp_bids_dir
  )
  
  # Pack with parallel downsampling
  output_file <- tempfile(fileext = ".tar.gz")
  result <- pack_bids(proj, output_file = output_file, 
                     downsample_factor = 0.25,
                     ncores = 2,
                     verbose = FALSE)
  
  expect_true(file.exists(result))
  
  # Check that files were processed
  contents <- list_pack_bids(result, verbose = FALSE)
  expect_true(any(contents$is_downsampled))
  
  # Clean up
  unlink(output_file)
  unlink(temp_bids_dir, recursive = TRUE)
})

test_that("resolution tag naming is correct for different factors", {
  skip_if_not_installed("neuroim2")
  
  participants_df <- tibble::tibble(participant_id = "01")
  file_structure_df <- tibble::tribble(
    ~subid, ~datatype, ~suffix,       ~fmriprep,
    "01",   "anat",    "T1w.nii.gz",  FALSE
  )
  
  temp_bids_dir <- tempfile("mock_bids_")
  
  proj <- create_mock_bids(
    project_name = "TestProject",
    participants = participants_df,
    file_structure = file_structure_df,
    create_stub = TRUE,
    stub_path = temp_bids_dir
  )
  
  # Test with factor 0.25 (4x reduction)
  output_file <- tempfile(fileext = ".tar.gz")
  result <- pack_bids(proj, output_file = output_file, 
                     downsample_factor = 0.25,
                     verbose = FALSE)
  
  contents <- list_pack_bids(result, verbose = FALSE)
  downsampled <- contents[contents$is_downsampled, ]
  expect_true(any(grepl("_res-low4x_", downsampled$file)))
  
  unlink(output_file)
  
  # Test with factor 0.5 (2x reduction)
  output_file <- tempfile(fileext = ".tar.gz")
  result <- pack_bids(proj, output_file = output_file, 
                     downsample_factor = 0.5,
                     verbose = FALSE)
  
  contents <- list_pack_bids(result, verbose = FALSE)
  downsampled <- contents[contents$is_downsampled, ]
  expect_true(any(grepl("_res-low2x_", downsampled$file)))
  
  # Clean up
  unlink(output_file)
  unlink(temp_bids_dir, recursive = TRUE)
})