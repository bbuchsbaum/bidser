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
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-01/func/sub-01_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-01/func/sub-01_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-01/func/sub-01_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-02/func/sub-02_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-02/func/sub-02_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-02/func/sub-02_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-03/func/sub-03_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-03/func/sub-03_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-03/func/sub-03_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-04/func/sub-04_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-04/func/sub-04_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-04/func/sub-04_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-05/func/sub-05_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-05/func/sub-05_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-05/func/sub-05_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-06/func/sub-06_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-06/func/sub-06_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-06/func/sub-06_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-07/func/sub-07_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-07/func/sub-07_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-07/func/sub-07_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-08/func/sub-08_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-08/func/sub-08_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-08/func/sub-08_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-09/func/sub-09_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-09/func/sub-09_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-09/func/sub-09_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-10/func/sub-10_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-10/func/sub-10_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-10/func/sub-10_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-11/func/sub-11_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-11/func/sub-11_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-11/func/sub-11_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-12/func/sub-12_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-12/func/sub-12_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-12/func/sub-12_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-13/func/sub-13_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-13/func/sub-13_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-13/func/sub-13_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-14/func/sub-14_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-14/func/sub-14_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-14/func/sub-14_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-15/func/sub-15_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-15/func/sub-15_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-15/func/sub-15_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-16/func/sub-16_task-balloonanalogrisktask_run-01_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-16/func/sub-16_task-balloonanalogrisktask_run-02_bold.nii.gz’
#> Warning: storing paths of more than 100 bytes is not portable:
#>   ‘bids_example_ds001_pack_21b427729b33/sub-16/func/sub-16_task-balloonanalogrisktask_run-03_bold.nii.gz’
#> Archive contents summary:
#>   Total files: 135
#>   Stub imaging files: 80
#>   Downsampled imaging files: 0
#>   Metadata files (JSON): 3
#>   Data files (TSV): 49
#>   Other files: 3
#>   Total size: 0.40 MB
#>                                                                                                     file
#> 7                                     bids_example_ds001_pack_21b427729b33/sub-01/anat/sub-01_T1w.nii.gz
#> 8                               bids_example_ds001_pack_21b427729b33/sub-01/anat/sub-01_inplaneT2.nii.gz
#> 9  bids_example_ds001_pack_21b427729b33/sub-01/func/sub-01_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 11 bids_example_ds001_pack_21b427729b33/sub-01/func/sub-01_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 13 bids_example_ds001_pack_21b427729b33/sub-01/func/sub-01_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 15                                    bids_example_ds001_pack_21b427729b33/sub-02/anat/sub-02_T1w.nii.gz
#>    size is_stub is_downsampled         type
#> 7     0    TRUE          FALSE imaging_stub
#> 8     0    TRUE          FALSE imaging_stub
#> 9     0    TRUE          FALSE imaging_stub
#> 11    0    TRUE          FALSE imaging_stub
#> 13    0    TRUE          FALSE imaging_stub
#> 15    0    TRUE          FALSE imaging_stub
# }
```
