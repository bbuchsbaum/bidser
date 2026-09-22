## Release summary

This is an update from CRAN version 0.5.0. It improves indexed BIDS queries,
metadata and event inheritance, derivative discovery, DWI support, and
cross-platform path handling. See `NEWS.md` for the complete changes in 0.5.1
and 0.5.2.

## R CMD check results

0 errors | 0 warnings | 2 notes

## Notes

* **Future file timestamps**: The local check host could not verify the current
  time. No package file was reported as having a future timestamp.
* **HTML manual validation**: The local HTML Tidy installation was too old, so
  HTML validation was skipped. The PDF manual built successfully.

All examples (including `--run-donttest`), tests, vignettes, and vignette
rebuilds passed.

## Test environments

* local macOS Sonoma 14.3 (aarch64-apple-darwin20), R 4.5.1,
  `R CMD check --as-cran`
* GitHub Actions: Ubuntu (devel, release, and oldrel-1), macOS (release), and
  Windows (release)
* R-hub: Linux, macOS, and Windows (R-devel)

## Downstream dependencies

There are currently no reverse `Depends`, `Imports`, or `LinkingTo`
dependencies on CRAN.
