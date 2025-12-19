# List Contents of Packed BIDS Archive

This function lists the contents of a BIDS archive created by
[`pack_bids`](https://bbuchsbaum.github.io/bidser/reference/pack_bids.md),
showing file sizes and identifying which files are stubs.

## Usage

``` r
list_pack_bids(archive_path, verbose = TRUE)
```

## Arguments

- archive_path:

  Character string specifying the path to the archive file.

- verbose:

  Logical. Whether to print summary statistics. Default is TRUE.

## Value

A data frame with columns:

- file:

  Relative file path within the archive

- size:

  File size in bytes

- is_stub:

  Logical indicating if the file is a 0-byte stub

- is_downsampled:

  Logical indicating if the file is a downsampled image

- type:

  File type based on extension (imaging, imaging_stub,
  imaging_downsampled, json, tsv, etc.)

## Examples

``` r
# \donttest{
# Create and inspect a packed BIDS archive
tryCatch({
  ds_path <- get_example_bids_dataset("ds001") 
  proj <- bids_project(ds_path)
  archive_path <- pack_bids(proj, verbose = FALSE)
  
  # List contents
  contents <- list_pack_bids(archive_path)
  
  # Show stub files
  stub_files <- contents[contents$is_stub, ]
  print(head(stub_files))
  
  # Clean up
  unlink(archive_path)
  unlink(ds_path, recursive = TRUE)
}, error = function(e) {
  message("Example failed: ", e$message)
})
#> Warning: Failed to create archive: file can not be copied both 'from' and 'to'
#> Example failed: invalid 'file' argument
# }
```
