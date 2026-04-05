# Apply a transformation to BIDS files

This function orchestrates the process of selecting files from a BIDS
project, applying a transformation to each file, and saving the output
in a new BIDS derivative directory. It leverages the existing bidser
parsing and search infrastructure.

## Usage

``` r
bids_transform(x, transformer, pipeline_name, ...)
```

## Arguments

- x:

  A `bids_project` object.

- transformer:

  A function that performs the transformation. It must take the input
  file path and return the output file path. The transformer is
  responsible for creating the output file.

- pipeline_name:

  The name for the new derivative pipeline.

- ...:

  Additional arguments passed to
  [`search_files`](https://bbuchsbaum.github.io/bidser/reference/search_files.md)
  to select files (e.g., `subid = "01"`, `task = "rest"`).

## Value

A character vector of paths to the newly created files.

## Examples

``` r
# \donttest{
tryCatch({
  ds_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds_path)

  # Create a simple transformer that adds a description
  add_desc_transformer <- function(infile) {
    entities <- encode(basename(infile))
    entities$desc <- if (is.null(entities$desc)) "smooth6mm" else 
                     paste(entities$desc, "smooth6mm", sep="")
    
    # Generate new filename
    new_name <- decode_bids_entities(entities)
    outfile <- file.path(dirname(infile), new_name)
    
    # For demo, just copy the file (real transformer would process it)
    file.copy(infile, outfile)
    return(outfile)
  }

  # Apply transformation to functional files for subject 01
  new_files <- bids_transform(proj, add_desc_transformer, "smoothed",
                              subid = "01", suffix = "bold.nii.gz")
  print(length(new_files))

}, error = function(e) {
  message("Example failed: ", e$message)
})
#> No files found matching the selection criteria.
#> [1] 0
# }
```
