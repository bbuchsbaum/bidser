# Infer TR (Repetition Time) from a BOLD file or sidecar

Given a path to a BOLD NIfTI file (`*.nii` or `*.nii.gz`) or its JSON
sidecar (`*.json`), this function locates the appropriate sidecar JSON
and returns the TR (in seconds). It prefers the JSON `RepetitionTime`
field (BIDS-compliant). If that is not available, it falls back to
computing TR as the median difference of `VolumeTiming` (if present).
Optionally, when the sidecar cannot be found or is missing both fields,
the function attempts to read TR from the NIfTI header (pixdim\[4\]) if
an appropriate reader is installed.

## Usage

``` r
infer_tr(x, ...)

# S3 method for class 'character'
infer_tr(
  x,
  prefer = c("json", "nifti"),
  fallback = TRUE,
  coerce_units = c("strict", "auto"),
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  A character path to a BOLD `.nii[.gz]` file or its `.json` sidecar, or
  a `bids_project` object.

- ...:

  Additional arguments passed to methods.

- prefer:

  Preferred source of TR: `"json"` (default) or `"nifti"`.

- fallback:

  If TRUE (default), attempt NIfTI header fallback when JSON is not
  available or incomplete.

- coerce_units:

  Unit handling for non-compliant values. `"strict"` (default) assumes
  seconds as per BIDS and returns values as-is. `"auto"` will convert
  clearly millisecond-like values to seconds (divide by 1000) and
  annotate the conversion in the return value's attributes.

- verbose:

  If TRUE, print informative messages when falling back or when
  encountering special cases (e.g., SBRef files).

## Value

Numeric TR in seconds, or `NA_real_` if it cannot be determined. The
return value includes attributes: `source` (e.g., `json:RepetitionTime`,
`json:VolumeTiming`, `nifti:pixdim4`), `path` (the file used), and
optionally `variable = TRUE` if `VolumeTiming` indicates non-constant
TR; a `unit = "ms->s"` attribute is added if units were auto-converted.

## Details

For NIfTI inputs, the JSON sidecar is resolved by replacing the
`*.nii`/`*.nii.gz` suffix with `.json` in the same directory. If that
file is not found, the function searches the directory for a `.json`
file with the same stem (filename without the NIfTI extension).

## Examples

``` r
tmp_json <- tempfile(fileext = ".json")
writeLines('{"RepetitionTime": 2}', tmp_json)
infer_tr(tmp_json)
#> [1] 2
#> attr(,"source")
#> [1] "json:RepetitionTime"
#> attr(,"path")
#> [1] "/tmp/Rtmp7ykeGk/file24d61ea4f02d.json"
unlink(tmp_json)

tmp_json2 <- tempfile(fileext = ".json")
writeLines('{"VolumeTiming": [0, 2, 4, 6]}', tmp_json2)
infer_tr(tmp_json2)
#> [1] 2
#> attr(,"source")
#> [1] "json:VolumeTiming"
#> attr(,"path")
#> [1] "/tmp/Rtmp7ykeGk/file24d671487b58.json"
unlink(tmp_json2)
```
