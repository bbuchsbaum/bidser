# Regex-based BIDS filename parsers

This module provides regex-based parsers for BIDS filenames as a
replacement for the Combin8R parser combinators. It maintains the same
interface while using only standard R regex functionality. Extract BIDS
components using simpler regex patterns

## Usage

``` r
extract_bids_components(filename, spec)
```

## Arguments

- filename:

  The filename to parse

- spec:

  The specification object

## Value

A list of extracted components or NULL
