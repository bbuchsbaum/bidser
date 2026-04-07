# Read the dataset_description.json for a BIDS project

Read the dataset_description.json for a BIDS project

## Usage

``` r
read_dataset_description(x, ...)

# S3 method for class 'character'
read_dataset_description(x, ...)

# S3 method for class 'bids_project'
read_dataset_description(x, ...)
```

## Arguments

- x:

  A `bids_project` object or a character path.

- ...:

  Additional arguments passed to methods.

## Value

A `bids_dataset_description` object, or NULL if the file is absent.

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  desc <- read_dataset_description(ds001_path)
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
