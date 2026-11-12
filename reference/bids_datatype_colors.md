# Colours for BIDS datatypes

A fixed, colour-blind-aware palette so that a datatype has the same
colour in every bidser figure. Unknown datatypes fall back to a neutral
grey.

## Usage

``` r
bids_datatype_colors(types = NULL)
```

## Arguments

- types:

  Optional character vector of datatypes to look up.

## Value

A named character vector of hex colours.

## Examples

``` r
bids_datatype_colors()
#>      anat      func      fmap       dwi      perf       beh       eeg       meg 
#> "#2F5D7C" "#C2502E" "#7B5EA7" "#2E7D5B" "#C98B1D" "#8A6D52" "#21808D" "#21808D" 
#>      ieeg      nirs       pet      micr     other 
#> "#21808D" "#21808D" "#A23B72" "#5C6B73" "#7D8288" 
bids_datatype_colors(c("anat", "func", "unknown"))
#>      anat      func   unknown 
#> "#2F5D7C" "#C2502E" "#7D8288" 
```
