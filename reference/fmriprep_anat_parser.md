# fMRIPrep anatomical parser constructor

fMRIPrep anatomical parser constructor

## Usage

``` r
fmriprep_anat_parser()
```

## Value

An fMRIPrep anatomical parser object for parsing preprocessed anatomical
files

## Examples

``` r
# Create an fMRIPrep anatomical parser
parser <- fmriprep_anat_parser()

# Parse a preprocessed anatomical file
result <- parse(parser, "sub-01_space-MNI152NLin2009cAsym_desc-preproc_T1w.nii.gz")
```
