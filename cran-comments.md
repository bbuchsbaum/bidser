## Resubmission

This is a resubmission addressing feedback from Benjamin Altmann:

1. **Software names in single quotes**: 'BIDS' and 'fMRIPrep' are now
   consistently quoted in the Title and Description fields.
2. **Commented-out example code**: Replaced commented-out examples in
   `infer_tr.Rd` with executable toy examples using temporary files.
3. **`\dontrun{}` removed**: Replaced `\dontrun{}` in
   `create_preproc_mask.mock_bids_project` with a runnable `try()` call.

## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

* **Possibly misspelled words in DESCRIPTION**: The words "Gorgolewski",
  "fMRIPrep", and "neuroimaging" are domain-specific terms from the
  neuroimaging field. Gorgolewski is the first author of the BIDS
  specification paper cited in the DESCRIPTION. fMRIPrep is a widely-used
  preprocessing pipeline. "neuroimaging" is the standard term for brain
  imaging data analysis.

* **`parse` masking `base::parse`**: The package exports an S3 generic
 `parse()` used to parse BIDS filenames via parser combinator objects. It
  does not interfere with `base::parse()` because the S3 method dispatch
  only applies to parser class objects.

## Test environments

* local macOS (aarch64-apple-darwin), R 4.4.x
* GitHub Actions: ubuntu-latest (release), macOS-latest (release), windows-latest (release)

## Downstream dependencies

This is a new submission. There are currently no downstream dependencies.
