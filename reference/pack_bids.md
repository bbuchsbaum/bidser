# Pack BIDS Project with Stub or Downsampled Imaging Files

This function creates a compressed archive (tar.gz or zip) of a BIDS
project, either replacing large imaging files (.nii, .nii.gz) with
0-byte stub files or downsampling them to lower resolution while
preserving all metadata files (JSON, TSV, etc.) with their full content.
This is useful for sharing BIDS project structure and metadata without
the large imaging data.

## Usage

``` r
pack_bids(
  x,
  output_file = NULL,
  format = NULL,
  include_derivatives = TRUE,
  downsample_factor = NULL,
  downsample_method = "box",
  ncores = 1,
  max_file_size = "10MB",
  exclude = NULL,
  strict_bids = FALSE,
  verbose = TRUE,
  temp_dir = NULL,
  cleanup = TRUE
)
```

## Arguments

- x:

  A `bids_project` object created by
  [`bids_project`](https://bbuchsbaum.github.io/bidser/reference/bids_project.md).

- output_file:

  Character string specifying the output archive filename. Should end
  with ".tar.gz" or ".zip". If not specified, defaults to
  "project_name_metadata.tar.gz" in the current directory.

- format:

  Character string specifying the archive format. Can be "tar.gz"
  (default) or "zip". If not specified, inferred from output_file
  extension.

- include_derivatives:

  Logical. Whether to include fMRIPrep derivatives if available. Default
  is TRUE.

- downsample_factor:

  Numeric value between 0 and 1 specifying the downsampling factor for
  imaging files. If NULL (default), creates stub files. A value of 0.25
  reduces dimensions by 4x (e.g., 64x64x64 becomes 16x16x16). Time
  dimension is preserved for 4D files.

- downsample_method:

  Character string specifying the downsampling method. Currently only
  "box" (box averaging) is supported. Default is "box".

- ncores:

  Integer specifying the number of cores for parallel processing during
  downsampling. Default is 1 (sequential). Values \> 1 enable parallel
  processing if the 'future' package is available.

- max_file_size:

  Character string or numeric value specifying the maximum file size for
  non-imaging files to include. Files larger than this will be replaced
  with stub files. Can be specified as "1MB", "500KB", "1.5GB" or as
  numeric bytes. Default is "10MB".

- exclude:

  Character string with a regular expression pattern to exclude files.
  Files matching this pattern will be replaced with stub files. For
  example, "\\h5\$" to exclude HDF5 files. Default is NULL (no
  exclusion).

- strict_bids:

  Logical. If TRUE, only include files that match BIDS naming
  conventions and standard BIDS metadata files. Non-BIDS files like
  .DS_Store, temporary files, or other non-standard files will be
  excluded. Default is FALSE (include all files).

- verbose:

  Logical. Whether to print progress messages. Default is TRUE.

- temp_dir:

  Character string specifying the temporary directory for creating the
  archive. If NULL (default), uses tempdir().

- cleanup:

  Logical. Whether to clean up the temporary directory after creating
  the archive. Default is TRUE.

## Value

Character string containing the path to the created archive file.
Returns NULL if the operation fails.

## Details

The function works by:

1.  Creating a temporary copy of the BIDS project structure

2.  Replacing all .nii and .nii.gz files with 0-byte stub files

3.  Preserving all other files (JSON, TSV, TXT, etc.) with full content

4.  Creating a compressed archive of the modified structure

This allows researchers to share BIDS dataset structure and metadata
without the large imaging files, which is useful for:

- Sharing dataset organization and metadata for review

- Creating lightweight references for dataset structure

- Testing BIDS tools without full datasets

## Examples

``` r
# \donttest{
# Create a BIDS project and pack it
tryCatch({
  ds_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds_path)
  
  # Pack with default settings (tar.gz with stub files)
  archive_path <- pack_bids(proj)
  
  # Pack with size limit and exclusion pattern
  archive_filtered <- pack_bids(proj,
                                max_file_size = "1MB",
                                exclude = "\\.h5$",
                                output_file = "ds001_filtered.tar.gz")
  
  # Pack with downsampling (4x reduction)
  archive_downsampled <- pack_bids(proj, 
                                   downsample_factor = 0.25,
                                   output_file = "ds001_low4x.tar.gz")
  
  # Pack with downsampling using parallel processing
  if (requireNamespace("future", quietly = TRUE)) {
    archive_parallel <- pack_bids(proj, 
                                 downsample_factor = 0.5,
                                 ncores = 2,
                                 output_file = "ds001_low2x.tar.gz")
  }
  
  # Pack as zip file
  zip_path <- pack_bids(proj, output_file = "ds001_metadata.zip")
  
  # Pack without derivatives
  archive_no_deriv <- pack_bids(proj, include_derivatives = FALSE)
  
  # Pack with strict BIDS mode (exclude non-BIDS files)
  archive_strict <- pack_bids(proj, strict_bids = TRUE,
                              output_file = "ds001_strict.tar.gz")
  
  # Clean up
  unlink(c(archive_path, archive_filtered, archive_downsampled, zip_path, 
           archive_no_deriv, archive_strict))
  if (exists("archive_parallel")) unlink(archive_parallel)
  unlink(ds_path, recursive = TRUE)
}, error = function(e) {
  message("Example failed: ", e$message)
})
#> Maximum file size for non-imaging files: 10.00 MB
#> 
#> === Starting pack_bids ===
#> Project: bids_example_ds001
#> Output: bids_example_ds001_metadata.tar.gz
#> Mode: Creating stub files for imaging data
#> BIDS validation: OFF (all files included)
#> Max file size: 10.00 MB
#> 
#> Creating temporary copy of BIDS project...
#> 
#> Found 135 total files to process
#>   - 80 imaging files (.nii/.nii.gz)
#>   - 55 metadata/other files
#> 
#> Processing metadata and supporting files...
#> Warning: Failed to create archive: file can not be copied both 'from' and 'to'
#> Maximum file size for non-imaging files: 1.00 MB
#> 
#> === Starting pack_bids ===
#> Project: bids_example_ds001
#> Output: ds001_filtered.tar.gz
#> Mode: Creating stub files for imaging data
#> BIDS validation: OFF (all files included)
#> Max file size: 1.00 MB
#> Exclude pattern: \.h5$
#> 
#> Creating temporary copy of BIDS project...
#>   - 0 imaging files (.nii/.nii.gz)
#>   - 0 metadata/other files
#> 
#> Creating archive...
#> 
#> === pack_bids Complete ===
#> Archive created: ds001_filtered.tar.gz
#> Archive size: 0.00 MB
#> 
#> Timing:
#>   Archive creation: 0.0 seconds
#>   Total time: 0.0 seconds
#> Temporary files cleaned up
#> Maximum file size for non-imaging files: 10.00 MB
#> 
#> === Starting pack_bids ===
#> Project: bids_example_ds001
#> Output: ds001_low4x.tar.gz
#> Downsampling: 4.00x reduction
#> BIDS validation: OFF (all files included)
#> Max file size: 10.00 MB
#> 
#> Creating temporary copy of BIDS project...
#>   - 0 imaging files (.nii/.nii.gz)
#>   - 0 metadata/other files
#> 
#> Creating archive...
#> 
#> === pack_bids Complete ===
#> Archive created: ds001_low4x.tar.gz
#> Archive size: 0.00 MB
#> Expected size reduction: ~64.0x for 3D, ~64.0x for 4D
#> 
#> Timing:
#>   Archive creation: 0.0 seconds
#>   Total time: 0.0 seconds
#> Temporary files cleaned up
#> Maximum file size for non-imaging files: 10.00 MB
#> Using parallel processing with 2 cores for downsampling
#> 
#> === Starting pack_bids ===
#> Project: bids_example_ds001
#> Output: ds001_low2x.tar.gz
#> Downsampling: 2.00x reduction
#> Parallel processing: 2 cores
#> BIDS validation: OFF (all files included)
#> Max file size: 10.00 MB
#> 
#> Creating temporary copy of BIDS project...
#>   - 0 imaging files (.nii/.nii.gz)
#>   - 0 metadata/other files
#> 
#> Creating archive...
#> 
#> === pack_bids Complete ===
#> Archive created: ds001_low2x.tar.gz
#> Archive size: 0.00 MB
#> Expected size reduction: ~8.0x for 3D, ~8.0x for 4D
#> 
#> Timing:
#>   Archive creation: 0.0 seconds
#>   Total time: 0.0 seconds
#> Temporary files cleaned up
#> Maximum file size for non-imaging files: 10.00 MB
#> 
#> === Starting pack_bids ===
#> Project: bids_example_ds001
#> Output: ds001_metadata.zip
#> Mode: Creating stub files for imaging data
#> BIDS validation: OFF (all files included)
#> Max file size: 10.00 MB
#> 
#> Creating temporary copy of BIDS project...
#>   - 0 imaging files (.nii/.nii.gz)
#>   - 0 metadata/other files
#> 
#> Creating archive...
#> Warning: No files found to archive
#> Maximum file size for non-imaging files: 10.00 MB
#> 
#> === Starting pack_bids ===
#> Project: bids_example_ds001
#> Output: bids_example_ds001_metadata.tar.gz
#> Mode: Creating stub files for imaging data
#> BIDS validation: OFF (all files included)
#> Max file size: 10.00 MB
#> 
#> Creating temporary copy of BIDS project...
#>   - 0 imaging files (.nii/.nii.gz)
#>   - 0 metadata/other files
#> 
#> Creating archive...
#> 
#> === pack_bids Complete ===
#> Archive created: bids_example_ds001_metadata.tar.gz
#> Archive size: 0.00 MB
#> 
#> Timing:
#>   Archive creation: 0.0 seconds
#>   Total time: 0.0 seconds
#> Temporary files cleaned up
#> Maximum file size for non-imaging files: 10.00 MB
#> 
#> === Starting pack_bids ===
#> Project: bids_example_ds001
#> Output: ds001_strict.tar.gz
#> Mode: Creating stub files for imaging data
#> BIDS validation: STRICT (only BIDS-compliant files)
#> Max file size: 10.00 MB
#> 
#> Creating temporary copy of BIDS project...
#> Warning: Failed to create archive: invalid subscript type 'list'
# }
```
