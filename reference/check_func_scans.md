# Check Functional Scans in a BIDS Project

This function performs a comprehensive inspection of functional scans
within a BIDS project, providing detailed summaries of scan counts and
file sizes per subject and task. It helps identify potential issues such
as missing scans, inconsistent file sizes, or unexpected variations in
the data.

## Usage

``` r
check_func_scans(x)
```

## Arguments

- x:

  A `bids_project` object created by
  [`bids_project()`](https://bbuchsbaum.github.io/bidser/reference/bids_project.md).

## Value

A list containing:

- `scans`: A tibble with details of all functional scans, including:

  - Subject ID

  - Task name

  - Run number

  - File size

  - Full file path

- `tasklist`: A vector of unique tasks found in the project

- `scans_per_subject`: A summary tibble showing the number of scans per
  subject

If multiple tasks are present, also includes:

- `scans_per_task`: Summary of scan counts by task

- `scans_per_task_subject`: Summary of scan counts by subject and task

- `size_per_task`: Tibble with file size statistics by task

If only one task is present:

- `size_per_subject`: Tibble with file size statistics by subject

## Examples

``` r
# \donttest{
# Check functional scans in a BIDS dataset
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  scan_check <- check_func_scans(proj)
  print(scan_check)
  
  # Filter for specific subjects
  sub01_check <- check_func_scans(proj, subid="01")
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> $scans
#> # A tibble: 48 × 8
#>    file                                size subid task  run   kind  suffix type 
#>    <chr>                              <fs:> <chr> <chr> <chr> <chr> <chr>  <chr>
#>  1 sub-01_task-balloonanalogrisktask…     0 01    ball… 01    bold  nii.gz func…
#>  2 sub-01_task-balloonanalogrisktask…     0 01    ball… 02    bold  nii.gz func…
#>  3 sub-01_task-balloonanalogrisktask…     0 01    ball… 03    bold  nii.gz func…
#>  4 sub-02_task-balloonanalogrisktask…     0 02    ball… 01    bold  nii.gz func…
#>  5 sub-02_task-balloonanalogrisktask…     0 02    ball… 02    bold  nii.gz func…
#>  6 sub-02_task-balloonanalogrisktask…     0 02    ball… 03    bold  nii.gz func…
#>  7 sub-03_task-balloonanalogrisktask…     0 03    ball… 01    bold  nii.gz func…
#>  8 sub-03_task-balloonanalogrisktask…     0 03    ball… 02    bold  nii.gz func…
#>  9 sub-03_task-balloonanalogrisktask…     0 03    ball… 03    bold  nii.gz func…
#> 10 sub-04_task-balloonanalogrisktask…     0 04    ball… 01    bold  nii.gz func…
#> # ℹ 38 more rows
#> 
#> $tasklist
#> [1] "balloonanalogrisktask"
#> 
#> $scans_per_subject
#> # A tibble: 16 × 2
#>    subid nscans
#>    <chr>  <int>
#>  1 01         3
#>  2 02         3
#>  3 03         3
#>  4 04         3
#>  5 05         3
#>  6 06         3
#>  7 07         3
#>  8 08         3
#>  9 09         3
#> 10 10         3
#> 11 11         3
#> 12 12         3
#> 13 13         3
#> 14 14         3
#> 15 15         3
#> 16 16         3
#> 
#> $size_per_subject
#> # A tibble: 48 × 9
#>    file                     size subid task  run   kind  suffix type  size_delta
#>    <chr>                   <fs:> <chr> <chr> <chr> <chr> <chr>  <chr> <fs::byte>
#>  1 sub-01_task-balloonana…     0 01    ball… 01    bold  nii.gz func…          0
#>  2 sub-01_task-balloonana…     0 01    ball… 02    bold  nii.gz func…          0
#>  3 sub-01_task-balloonana…     0 01    ball… 03    bold  nii.gz func…          0
#>  4 sub-02_task-balloonana…     0 02    ball… 01    bold  nii.gz func…          0
#>  5 sub-02_task-balloonana…     0 02    ball… 02    bold  nii.gz func…          0
#>  6 sub-02_task-balloonana…     0 02    ball… 03    bold  nii.gz func…          0
#>  7 sub-03_task-balloonana…     0 03    ball… 01    bold  nii.gz func…          0
#>  8 sub-03_task-balloonana…     0 03    ball… 02    bold  nii.gz func…          0
#>  9 sub-03_task-balloonana…     0 03    ball… 03    bold  nii.gz func…          0
#> 10 sub-04_task-balloonana…     0 04    ball… 01    bold  nii.gz func…          0
#> # ℹ 38 more rows
#> 
#> attr(,"class")
#> [1] "check"            "check_func_scans"
#> Example requires internet connection: unused argument (subid = "01")
# }
```
