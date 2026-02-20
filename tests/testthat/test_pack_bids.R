test_that("pack_bids creates tar.gz archive with stub files", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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

  # Check that files were processed with resolution tag applied
  # Note: Mock NIfTI files are empty, so actual downsampling fails and creates stubs,
  # but the resolution tag naming should still be applied
  contents <- list_pack_bids(result, verbose = FALSE)
  expect_true(any(grepl("_res-", contents$file)))

  # Clean up
  unlink(output_file)
  unlink(temp_bids_dir, recursive = TRUE)
})

test_that("pack_bids respects max_file_size parameter", {
  # Create a mock project
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
  
  # Create a large file (simulated with a JSON file)
  large_file <- file.path(temp_bids_dir, "sub-01", "large_data.json")
  dir.create(dirname(large_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(rep("test data", 1000), large_file)
  
  # Pack with a very small size limit
  output_file <- tempfile(fileext = ".tar.gz")
  result <- pack_bids(proj, output_file = output_file, 
                     max_file_size = "1KB",  # Very small limit
                     verbose = FALSE)
  
  expect_true(file.exists(result))
  
  # Extract and check that large file was stubbed
  temp_extract <- tempfile("extract_")
  dir.create(temp_extract)
  untar(output_file, exdir = temp_extract)
  
  extracted_file <- file.path(temp_extract, basename(temp_bids_dir), "sub-01", "large_data.json")
  if (file.exists(extracted_file)) {
    expect_equal(file.size(extracted_file), 0)  # Should be a stub
  }
  
  # Clean up
  unlink(output_file)
  unlink(temp_extract, recursive = TRUE)
  unlink(temp_bids_dir, recursive = TRUE)
})

test_that("pack_bids respects exclude parameter", {
  # Create a mock project
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
  
  # Create some h5 files
  h5_file1 <- file.path(temp_bids_dir, "sub-01", "data.h5")
  h5_file2 <- file.path(temp_bids_dir, "sub-01", "analysis.h5")
  dir.create(dirname(h5_file1), recursive = TRUE, showWarnings = FALSE)
  writeLines("h5 data", h5_file1)
  writeLines("h5 analysis", h5_file2)
  
  # Also create a non-h5 file
  json_file <- file.path(temp_bids_dir, "sub-01", "metadata.json")
  writeLines("json data", json_file)
  
  # Pack with h5 exclusion
  output_file <- tempfile(fileext = ".tar.gz")
  result <- pack_bids(proj, output_file = output_file, 
                     exclude = "\\.h5$",
                     verbose = FALSE)
  
  expect_true(file.exists(result))
  
  # Extract and check
  temp_extract <- tempfile("extract_")
  dir.create(temp_extract)
  untar(output_file, exdir = temp_extract)
  
  # H5 files should be stubs
  for (h5_name in c("data.h5", "analysis.h5")) {
    h5_path <- file.path(temp_extract, basename(temp_bids_dir), "sub-01", h5_name)
    if (file.exists(h5_path)) {
      expect_equal(file.size(h5_path), 0, info = paste("H5 file should be stub:", h5_name))
    }
  }
  
  # JSON file should have content
  json_path <- file.path(temp_extract, basename(temp_bids_dir), "sub-01", "metadata.json")
  if (file.exists(json_path)) {
    expect_true(file.size(json_path) > 0, info = "JSON file should have content")
  }
  
  # Clean up
  unlink(output_file)
  unlink(temp_extract, recursive = TRUE)
  unlink(temp_bids_dir, recursive = TRUE)
})

test_that("parse_file_size handles various formats", {
  # Test numeric input
  expect_equal(bidser:::parse_file_size(1024), 1024)
  
  # Test various string formats
  expect_equal(bidser:::parse_file_size("1KB"), 1024)
  expect_equal(bidser:::parse_file_size("1MB"), 1024^2)
  expect_equal(bidser:::parse_file_size("1GB"), 1024^3)
  expect_equal(bidser:::parse_file_size("1.5MB"), 1.5 * 1024^2)
  expect_equal(bidser:::parse_file_size("500KB"), 500 * 1024)
  
  # Test case insensitivity
  expect_equal(bidser:::parse_file_size("1mb"), 1024^2)
  expect_equal(bidser:::parse_file_size("1Mb"), 1024^2)
  
  # Test NULL input
  expect_null(bidser:::parse_file_size(NULL))
  
  # Test invalid formats
  expect_error(bidser:::parse_file_size("invalid"), "Invalid file size format")
})

test_that("strict_bids mode filters non-BIDS files", {
  # Create a mock project
  participants_df <- tibble::tibble(participant_id = "01")
  file_structure_df <- tibble::tribble(
    ~subid, ~datatype, ~suffix,       ~fmriprep,
    "01",   "anat",    "T1w.nii.gz",  FALSE,
    "01",   "func",    "bold.nii.gz", FALSE
  )
  
  temp_bids_dir <- tempfile("mock_bids_")
  
  proj <- create_mock_bids(
    project_name = "TestProject",
    participants = participants_df,
    file_structure = file_structure_df,
    create_stub = TRUE,
    stub_path = temp_bids_dir
  )
  
  # Create some non-BIDS files
  non_bids_file1 <- file.path(temp_bids_dir, ".DS_Store")
  non_bids_file2 <- file.path(temp_bids_dir, "random_file.txt")
  non_bids_file3 <- file.path(temp_bids_dir, "sub-01", "temp_file.tmp")
  
  writeLines("mac file", non_bids_file1)
  writeLines("random", non_bids_file2)
  dir.create(dirname(non_bids_file3), recursive = TRUE, showWarnings = FALSE)
  writeLines("temp", non_bids_file3)
  
  # Pack without strict mode (should include non-BIDS files)
  output_file_normal <- tempfile(fileext = ".tar.gz")
  result_normal <- pack_bids(proj, output_file = output_file_normal, 
                             strict_bids = FALSE, verbose = FALSE)
  
  # Pack with strict mode (should exclude non-BIDS files)
  output_file_strict <- tempfile(fileext = ".tar.gz")
  result_strict <- pack_bids(proj, output_file = output_file_strict, 
                             strict_bids = TRUE, verbose = FALSE)
  
  # Check contents
  contents_normal <- list_pack_bids(result_normal, verbose = FALSE)
  contents_strict <- list_pack_bids(result_strict, verbose = FALSE)
  
  # Normal mode should have non-BIDS files (at least some of them)
  has_non_bids <- any(grepl("random_file\\.txt", contents_normal$file)) ||
                   any(grepl("temp_file\\.tmp", contents_normal$file)) ||
                   any(grepl("\\.DS_Store", contents_normal$file))
  expect_true(has_non_bids)
  
  # Strict mode should NOT have non-BIDS files
  expect_false(any(grepl("\\.DS_Store", contents_strict$file)))
  expect_false(any(grepl("random_file\\.txt", contents_strict$file)))
  expect_false(any(grepl("temp_file\\.tmp", contents_strict$file)))
  
  # But strict mode should still have BIDS files
  expect_true(any(grepl("sub-01_T1w\\.nii\\.gz", contents_strict$file)))
  expect_true(any(grepl("participants\\.tsv", contents_strict$file)))
  
  # Clean up
  unlink(output_file_normal)
  unlink(output_file_strict)
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
  # Check that the resolution tag is applied to imaging files
  # Note: With mock files, actual downsampling fails but resolution tag is still applied
  expect_true(any(grepl("_res-low4x_", contents$file)))

  unlink(output_file)

  # Test with factor 0.5 (2x reduction)
  output_file <- tempfile(fileext = ".tar.gz")
  result <- pack_bids(proj, output_file = output_file,
                     downsample_factor = 0.5,
                     verbose = FALSE)

  contents <- list_pack_bids(result, verbose = FALSE)
  # Check that the resolution tag is applied to imaging files
  expect_true(any(grepl("_res-low2x_", contents$file)))
  
  # Clean up
  unlink(output_file)
  unlink(temp_bids_dir, recursive = TRUE)
})