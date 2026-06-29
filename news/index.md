# Changelog

## bidser 0.4.0

- Add `confound_set("legacy_default")`, a public, version-robust handle
  for the 26 canonical confound names historically exposed only as the
  unexported `bidser:::DEFAULT_CVARS2`. Code that depended on
  `bidser:::DEFAULT_CVARS2` should switch to
  `confound_set("legacy_default")`, which returns the identical set.
  `DEFAULT_CVARS2` is retained (unexported) for now but is superseded.
- Express the
  [`read_confounds()`](https://bbuchsbaum.github.io/bidser/reference/read_confounds.md)
  `cvars` default via `confound_set("legacy_default")` and remove the
  internal `DEFAULT_CVARS` constant. The default now resolves through
  canonical confound names; the set of columns returned for a no-`cvars`
  call is unchanged in practice on fMRIPrep outputs.
  [`?read_confounds`](https://bbuchsbaum.github.io/bidser/reference/read_confounds.md)
  now documents the recommended modern default
  `confound_strategy("pcabasic80")` (noting it is *not* equivalent to
  the legacy default) and cross-references the
  [`confound_set()`](https://bbuchsbaum.github.io/bidser/reference/confound_set.md)/[`confound_strategy()`](https://bbuchsbaum.github.io/bidser/reference/confound_strategy.md)/[`list_confound_sets()`](https://bbuchsbaum.github.io/bidser/reference/list_confound_sets.md)/
  [`list_confound_strategies()`](https://bbuchsbaum.github.io/bidser/reference/list_confound_strategies.md)
  helpers.
- Change `read_preproc_scans()` to return a file-ordered list of
  `NeuroVec` objects, one per matched preprocessed scan, instead of
  collapsing multiple files into a single container.
- Fix derivative discovery and legacy fMRIPrep helpers for datasets that
  place derivative subject folders directly under `derivatives/` with
  `prep_dir = "derivatives"`.
- Make
  [`read_confounds()`](https://bbuchsbaum.github.io/bidser/reference/read_confounds.md)
  raise informative, selection-aware errors when no matching
  participants, confound files, or usable requested confounds are found.
- Add run-level confound diagnostics and cleaning via
  [`check_confounds()`](https://bbuchsbaum.github.io/bidser/reference/check_confounds.md),
  [`clean_confounds()`](https://bbuchsbaum.github.io/bidser/reference/check_confounds.md),
  and `read_confounds(..., clean = )`, with zero-variance confounds
  dropped by default and recorded in a `confound_diagnostics` attribute.
- Fix
  [`create_preproc_mask()`](https://bbuchsbaum.github.io/bidser/reference/create_preproc_mask.md)
  to ignore matching JSON sidecars and only read actual NIfTI mask
  images.
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
