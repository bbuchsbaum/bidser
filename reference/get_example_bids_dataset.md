# Download Example BIDS Dataset

Downloads and extracts an example BIDS dataset for testing and
demonstration purposes. The datasets are sourced from the official BIDS
examples repository on GitHub.

## Usage

``` r
get_example_bids_dataset(dataset_name = "ds001")
```

## Arguments

- dataset_name:

  Character string specifying which dataset to download. Common options
  include "ds001", "ds002", "ds007", "phoneme_stripped", etc.

## Value

Character string containing the path to the downloaded dataset
directory.

## Details

This function requires an internet connection to download data from
GitHub. The datasets are cached in the temporary directory AND in memory
for the session, so repeated calls with the same dataset_name will reuse
the already downloaded data. Note: Don't call
[`unlink()`](https://rdrr.io/r/base/unlink.html) on the returned path in
examples, as this defeats the caching mechanism and forces re-downloads.

## Examples

``` r
# \donttest{
tryCatch({
  ds_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds_path)
  print(participants(proj))
  
  # Note: Don't unlink the path - it's cached for performance
  # unlink(ds_path, recursive=TRUE)  # DON'T DO THIS
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#>  [1] "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" "13" "14" "15"
#> [16] "16"
# }
```
