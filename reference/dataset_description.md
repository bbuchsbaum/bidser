# Get the dataset_description object from a BIDS project

Get the dataset_description object from a BIDS project

## Usage

``` r
dataset_description(x, ...)

# S3 method for class 'bids_project'
dataset_description(x, ...)
```

## Arguments

- x:

  A `bids_project` object.

- ...:

  Additional arguments passed to methods.

## Value

A `bids_dataset_description` object, or NULL.

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  desc <- dataset_description(proj)
  print(desc)
  unlink(ds001_path, recursive = TRUE)
}, error = function(e) message("Example requires internet: ", e$message))
#> <bids_dataset_description> 
#>   Name:          Balloon Analog Risk-taking Task 
#>   BIDSVersion:   1.0.0 
#>   DatasetType:   raw 
#>   License:       (none) 
#>   GeneratedBy:   (none) 
#>   DatasetLinks:  (none) 
# }
```
