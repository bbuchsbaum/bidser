# Changelog

## bidser 0.4.0

- Add
  [`query_files()`](https://bbuchsbaum.github.io/bidser/reference/query_files.md)
  as the recommended explicit query API for new workflows, with
  `match_mode`, `require_entity`, and raw/derivatives `scope` controls.
- Add
  [`get_metadata()`](https://bbuchsbaum.github.io/bidser/reference/get_metadata.md)
  for inheritance-aware BIDS metadata resolution with deterministic
  nearest-sidecar precedence.
- Extend
  [`read_sidecar()`](https://bbuchsbaum.github.io/bidser/reference/read_sidecar.md)
  with inheritance-aware resolution via `inherit` and `inherit_scope`.
- Add permissive `bids_project(..., strict_participants = FALSE)`
  loading for datasets that omit `participants.tsv`, while keeping
  compliance reporting explicit.
- Add derivative discovery via `bids_project(..., derivatives = "auto")`
  and
  [`derivative_pipelines()`](https://bbuchsbaum.github.io/bidser/reference/derivative_pipelines.md),
  while preserving legacy `fmriprep` helpers when an fMRIPrep pipeline
  is present.
- Add persistent file indexing with
  [`bids_index()`](https://bbuchsbaum.github.io/bidser/reference/bids_index.md)
  and indexed
  [`query_files()`](https://bbuchsbaum.github.io/bidser/reference/query_files.md)
  lookups.
- Add
  [`variables_table()`](https://bbuchsbaum.github.io/bidser/reference/variables_table.md)
  and
  [`bids_report_data()`](https://bbuchsbaum.github.io/bidser/reference/bids_report_data.md)
  /
  [`bids_report()`](https://bbuchsbaum.github.io/bidser/reference/bids_report.md)
  for tidy run-level inventory and lightweight dataset reporting.
- Expand acceptance coverage for metadata inheritance, missing-sidecar
  cases, conflict handling, and derivative-scope behavior.
- Keep
  [`search_files()`](https://bbuchsbaum.github.io/bidser/reference/search_files.md)
  available for backward compatibility while steering new examples and
  docs toward
  [`query_files()`](https://bbuchsbaum.github.io/bidser/reference/query_files.md).

## bidser 0.3.0

- Fix
  [`load_all_events()`](https://bbuchsbaum.github.io/bidser/reference/load_all_events-method.md)
  and
  [`read_events()`](https://bbuchsbaum.github.io/bidser/reference/read_events.md)
  to use tab delimiter for BIDS TSV event files instead of space
  delimiter.
- Fix
  [`read_sidecar()`](https://bbuchsbaum.github.io/bidser/reference/read_sidecar.md)
  to handle JSON sidecars containing vector-valued fields
  (e.g. `SliceTiming`) by storing them as list-columns instead of
  failing with a “differing number of rows” error.

## bidser 0.2.0

CRAN release: 2026-02-19

- TBD.

## bidser 0.1.0

- Initial CRAN release.
- Read, validate, and query BIDS-compliant neuroimaging projects.
- Support for fMRIPrep derivatives including confound tables,
  preprocessed scans, and brain masks.
- Mock BIDS project creation for testing
  ([`create_mock_bids()`](https://bbuchsbaum.github.io/bidser/reference/create_mock_bids.md)).
- Confound processing with PCA reduction and canonical variable
  aliasing.
- Visualization of PCA confound scores and loadings
  ([`plot.bids_confounds()`](https://bbuchsbaum.github.io/bidser/reference/plot.bids_confounds.md)).
- Search and filter BIDS files by subject, session, task, run, space,
  and other entities.
