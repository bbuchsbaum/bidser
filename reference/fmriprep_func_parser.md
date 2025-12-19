# fMRIPrep functional parser constructor

fMRIPrep functional parser constructor

## Usage

``` r
fmriprep_func_parser()
```

## Value

An fMRIPrep functional parser object for parsing preprocessed functional
files

## Examples

``` r
# Create an fMRIPrep functional parser
parser <- fmriprep_func_parser()

# Parse a preprocessed functional file
result <- parse(parser, "sub-01_task-rest_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz")
```
