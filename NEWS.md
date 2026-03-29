# bidser 0.4.0

* Add `query_files()` as the recommended explicit query API for new workflows,
  with `match_mode`, `require_entity`, and raw/derivatives `scope` controls.
* Add `get_metadata()` for inheritance-aware BIDS metadata resolution with
  deterministic nearest-sidecar precedence.
* Extend `read_sidecar()` with inheritance-aware resolution via `inherit` and
  `inherit_scope`.
* Add permissive `bids_project(..., strict_participants = FALSE)` loading for
  datasets that omit `participants.tsv`, while keeping compliance reporting
  explicit.
* Add derivative discovery via `bids_project(..., derivatives = "auto")` and
  `derivative_pipelines()`, while preserving legacy `fmriprep` helpers when an
  fMRIPrep pipeline is present.
* Add persistent file indexing with `bids_index()` and indexed `query_files()`
  lookups.
* Add `variables_table()` and `bids_report_data()` / `bids_report()` for tidy
  run-level inventory and lightweight dataset reporting.
* Expand acceptance coverage for metadata inheritance, missing-sidecar cases,
  conflict handling, and derivative-scope behavior.
* Keep `search_files()` available for backward compatibility while steering new
  examples and docs toward `query_files()`.

# bidser 0.3.0

* Fix `load_all_events()` and `read_events()` to use tab delimiter for BIDS TSV
  event files instead of space delimiter.
* Fix `read_sidecar()` to handle JSON sidecars containing vector-valued fields
  (e.g. `SliceTiming`) by storing them as list-columns instead of failing with
  a "differing number of rows" error.

# bidser 0.2.0

* TBD.

# bidser 0.1.0

* Initial CRAN release.
* Read, validate, and query BIDS-compliant neuroimaging projects.
* Support for fMRIPrep derivatives including confound tables, preprocessed
  scans, and brain masks.
* Mock BIDS project creation for testing (`create_mock_bids()`).
* Confound processing with PCA reduction and canonical variable aliasing.
* Visualization of PCA confound scores and loadings (`plot.bids_confounds()`).
* Search and filter BIDS files by subject, session, task, run, space,
  and other entities.
