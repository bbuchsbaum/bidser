# Get "flat" representation of BIDS Project

This function returns a flattened (non-hierarchical) representation of a
BIDS project formatted as a data frame. It extracts file paths or file
names from the BIDS tree structure, filtering for entries that start
with "sub-" to focus on subject-level data.

## Usage

``` r
flat_list(x, ...)

# S3 method for class 'bids_project'
flat_list(x, full_path = TRUE, ...)
```

## Arguments

- x:

  the `bids_project` object

- ...:

  extra args passed to methods

- full_path:

  If TRUE, return full paths to files; if FALSE, return just file names
  (default: TRUE)

## Value

A data frame containing either full paths to files (if `full_path=TRUE`)
or just the file names (if `full_path=FALSE`). Each row represents one
file in the BIDS project.

## Examples

``` r
# \donttest{
# Get flat representation with full paths
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  flat_list(proj)
  
  # Get flat representation with just file names
  flat_list(proj, full_path=FALSE)
  
  # Clean up
  # Example datasets are cached; leave the cache in place.
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#>                                                     name
#> 1   sub-01_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 2    sub-01_task-balloonanalogrisktask_run-01_events.tsv
#> 3   sub-01_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 4    sub-01_task-balloonanalogrisktask_run-02_events.tsv
#> 5   sub-01_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 6    sub-01_task-balloonanalogrisktask_run-03_events.tsv
#> 7                                      sub-01_T1w.nii.gz
#> 8                                sub-01_inplaneT2.nii.gz
#> 9   sub-02_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 10   sub-02_task-balloonanalogrisktask_run-01_events.tsv
#> 11  sub-02_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 12   sub-02_task-balloonanalogrisktask_run-02_events.tsv
#> 13  sub-02_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 14   sub-02_task-balloonanalogrisktask_run-03_events.tsv
#> 15                                     sub-02_T1w.nii.gz
#> 16                               sub-02_inplaneT2.nii.gz
#> 17  sub-03_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 18   sub-03_task-balloonanalogrisktask_run-01_events.tsv
#> 19  sub-03_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 20   sub-03_task-balloonanalogrisktask_run-02_events.tsv
#> 21  sub-03_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 22   sub-03_task-balloonanalogrisktask_run-03_events.tsv
#> 23                                     sub-03_T1w.nii.gz
#> 24                               sub-03_inplaneT2.nii.gz
#> 25  sub-04_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 26   sub-04_task-balloonanalogrisktask_run-01_events.tsv
#> 27  sub-04_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 28   sub-04_task-balloonanalogrisktask_run-02_events.tsv
#> 29  sub-04_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 30   sub-04_task-balloonanalogrisktask_run-03_events.tsv
#> 31                                     sub-04_T1w.nii.gz
#> 32                               sub-04_inplaneT2.nii.gz
#> 33  sub-05_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 34   sub-05_task-balloonanalogrisktask_run-01_events.tsv
#> 35  sub-05_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 36   sub-05_task-balloonanalogrisktask_run-02_events.tsv
#> 37  sub-05_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 38   sub-05_task-balloonanalogrisktask_run-03_events.tsv
#> 39                                     sub-05_T1w.nii.gz
#> 40                               sub-05_inplaneT2.nii.gz
#> 41  sub-06_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 42   sub-06_task-balloonanalogrisktask_run-01_events.tsv
#> 43  sub-06_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 44   sub-06_task-balloonanalogrisktask_run-02_events.tsv
#> 45  sub-06_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 46   sub-06_task-balloonanalogrisktask_run-03_events.tsv
#> 47                                     sub-06_T1w.nii.gz
#> 48                               sub-06_inplaneT2.nii.gz
#> 49  sub-07_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 50   sub-07_task-balloonanalogrisktask_run-01_events.tsv
#> 51  sub-07_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 52   sub-07_task-balloonanalogrisktask_run-02_events.tsv
#> 53  sub-07_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 54   sub-07_task-balloonanalogrisktask_run-03_events.tsv
#> 55                                     sub-07_T1w.nii.gz
#> 56                               sub-07_inplaneT2.nii.gz
#> 57  sub-08_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 58   sub-08_task-balloonanalogrisktask_run-01_events.tsv
#> 59  sub-08_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 60   sub-08_task-balloonanalogrisktask_run-02_events.tsv
#> 61  sub-08_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 62   sub-08_task-balloonanalogrisktask_run-03_events.tsv
#> 63                                     sub-08_T1w.nii.gz
#> 64                               sub-08_inplaneT2.nii.gz
#> 65  sub-09_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 66   sub-09_task-balloonanalogrisktask_run-01_events.tsv
#> 67  sub-09_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 68   sub-09_task-balloonanalogrisktask_run-02_events.tsv
#> 69  sub-09_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 70   sub-09_task-balloonanalogrisktask_run-03_events.tsv
#> 71                                     sub-09_T1w.nii.gz
#> 72                               sub-09_inplaneT2.nii.gz
#> 73  sub-10_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 74   sub-10_task-balloonanalogrisktask_run-01_events.tsv
#> 75  sub-10_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 76   sub-10_task-balloonanalogrisktask_run-02_events.tsv
#> 77  sub-10_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 78   sub-10_task-balloonanalogrisktask_run-03_events.tsv
#> 79                                     sub-10_T1w.nii.gz
#> 80                               sub-10_inplaneT2.nii.gz
#> 81  sub-11_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 82   sub-11_task-balloonanalogrisktask_run-01_events.tsv
#> 83  sub-11_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 84   sub-11_task-balloonanalogrisktask_run-02_events.tsv
#> 85  sub-11_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 86   sub-11_task-balloonanalogrisktask_run-03_events.tsv
#> 87                                     sub-11_T1w.nii.gz
#> 88                               sub-11_inplaneT2.nii.gz
#> 89  sub-12_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 90   sub-12_task-balloonanalogrisktask_run-01_events.tsv
#> 91  sub-12_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 92   sub-12_task-balloonanalogrisktask_run-02_events.tsv
#> 93  sub-12_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 94   sub-12_task-balloonanalogrisktask_run-03_events.tsv
#> 95                                     sub-12_T1w.nii.gz
#> 96                               sub-12_inplaneT2.nii.gz
#> 97  sub-13_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 98   sub-13_task-balloonanalogrisktask_run-01_events.tsv
#> 99  sub-13_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 100  sub-13_task-balloonanalogrisktask_run-02_events.tsv
#> 101 sub-13_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 102  sub-13_task-balloonanalogrisktask_run-03_events.tsv
#> 103                                    sub-13_T1w.nii.gz
#> 104                              sub-13_inplaneT2.nii.gz
#> 105 sub-14_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 106  sub-14_task-balloonanalogrisktask_run-01_events.tsv
#> 107 sub-14_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 108  sub-14_task-balloonanalogrisktask_run-02_events.tsv
#> 109 sub-14_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 110  sub-14_task-balloonanalogrisktask_run-03_events.tsv
#> 111                                    sub-14_T1w.nii.gz
#> 112                              sub-14_inplaneT2.nii.gz
#> 113 sub-15_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 114  sub-15_task-balloonanalogrisktask_run-01_events.tsv
#> 115 sub-15_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 116  sub-15_task-balloonanalogrisktask_run-02_events.tsv
#> 117 sub-15_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 118  sub-15_task-balloonanalogrisktask_run-03_events.tsv
#> 119                                    sub-15_T1w.nii.gz
#> 120                              sub-15_inplaneT2.nii.gz
#> 121 sub-16_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 122  sub-16_task-balloonanalogrisktask_run-01_events.tsv
#> 123 sub-16_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 124  sub-16_task-balloonanalogrisktask_run-02_events.tsv
#> 125 sub-16_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 126  sub-16_task-balloonanalogrisktask_run-03_events.tsv
#> 127                                    sub-16_T1w.nii.gz
#> 128                              sub-16_inplaneT2.nii.gz
# }
```
