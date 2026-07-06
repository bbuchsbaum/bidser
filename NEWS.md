# bidser 0.5.0

* Windows: normalize project and resolved paths with forward slashes
  (`winslash = "/"`) so full paths returned by `query_files(full_path = TRUE)`,
  `func_scans()`, `n_volumes()`, and `resolve_bids_uri()` use a consistent
  separator instead of mixing `\` and `/`. Fixes several `R CMD check` test
  failures on Windows.

* Make `query_files()` reuse the index built when the project was constructed
  instead of re-scanning the filesystem on every call. Repeated queries are now
  roughly two orders of magnitude faster (an indexed lookup rather than a full
  `stat` of the dataset on each query). The default query surface is a snapshot;
  use `refresh = TRUE` to re-scan the manifest and pick up additions, content
  changes, and removals without rebuilding the project object.
  `query_files()` also now honours `index = "none"` and prefers each project's
  own construction-time snapshot, so two project objects sharing a path no
  longer perturb each other's query results.
* Build the query manifest from a direct filesystem path scan instead of only
  the parsed data tree. This keeps bidser's tree-focused accessors intact while
  making `query_files()` and broad `search_files()` see pybids-visible layout
  files such as `README`, `CHANGES`, `dataset_description.json`,
  `participants.tsv`, `*_scans.tsv`, JSON sidecars, root `.bval`/`.bvec` files,
  and extensionless files under subject datatypes. On `ds005`, the raw
  file-count parity surface is now 142/142 against vendored pybids.
* Make indexed `query_files()` honour `strict = TRUE` for missing entity filters,
  so broad subject/task queries do not accidentally include root layout files.
* Fix wildcard entity filters such as `task = ".*"` in fallback/no-index
  `query_files()` calls so they match indexed query semantics, and remove the
  unused tree-derived manifest builder left behind by the filesystem-backed
  manifest path.
* Fix `require_entity = TRUE` being silently ignored on the indexed
  `query_files()` path when an entity filter used the `.*` wildcard (e.g.
  `task = ".*"`). It now requires the entity to be present as documented,
  matching the non-indexed path, so entity-less files (such as anatomical
  scans, which lack `task`) are correctly excluded.
* Add raw DWI files as a built-in datatype, so `bids_project()` and
  `query_files()` index `dwi/` files (`.nii.gz`, `.nii`, `.json`, `.bval`,
  `.bvec`) without user-side `register_datatype()` setup.
* Speed up index construction by deriving manifest/sidecar entities directly
  from paths and by lazily ingesting JSON sidecar contents only when metadata
  APIs need them. This keeps query-oriented project construction focused on the
  file manifest while preserving indexed metadata inheritance on demand.
* Speed up `bids_project()` construction by replacing generic tree/table
  conversions and repeated tree searches with leaf-specific extractors, by using
  vectorized manifest row construction, and by trimming parser/directory-listing
  overhead in constructor hot paths.
* Further speed up indexed `query_files()` by avoiding per-query manifest
  re-finalization and by applying indexed filters with a single logical mask.
* Add `tools/benchmark-pybids-parity.R`, a repeatable local harness for
  comparing bidser construction/query timings and basic file-count parity
  against the vendored pybids checkout via `uv`.

# bidser 0.4.0

* Add `bids_entities()` for vectorized path-to-entity parsing, returning one
  typed tibble row per path.
* Add `n_volumes()` for reading functional scan volume counts from NIfTI headers,
  with a `bids_project` method for raw or preprocessed scan discovery.
* Make `search_files()` fall back to a filesystem regex scan when the parsed BIDS
  tree has no match, so ad hoc regex searches can find valid but currently
  unparsed files such as masks.
* Expose common filtering arguments on the `read_events()` and
  `read_confounds()` generics, and have `read_events()` return bare metadata
  aliases alongside the legacy dotted columns.
* Change `confound_set("dvars")` to select only `std_dvars`, with explicit
  `std_dvars`, `raw_dvars`, `non_std_dvars`, and `vx_wisestd_dvars` selectors
  for callers that want a specific variant.
* Add `confound_set("legacy_default")`, a public, version-robust handle for the
  26 canonical confound names historically exposed only as the unexported
  `bidser:::DEFAULT_CVARS2`. Code that depended on `bidser:::DEFAULT_CVARS2`
  should switch to `confound_set("legacy_default")`, which returns the identical
  set. `DEFAULT_CVARS2` is retained (unexported) for now but is superseded.
* Express the `read_confounds()` `cvars` default via
  `confound_set("legacy_default")` and remove the internal `DEFAULT_CVARS`
  constant. The default now resolves through canonical confound names; the set
  of columns returned for a no-`cvars` call is unchanged in practice on
  fMRIPrep outputs. `?read_confounds` now documents the recommended modern
  default `confound_strategy("pcabasic80")` (noting it is *not* equivalent to
  the legacy default) and cross-references the
  `confound_set()`/`confound_strategy()`/`list_confound_sets()`/
  `list_confound_strategies()` helpers.
* Change `read_preproc_scans()` to return a file-ordered list of `NeuroVec`
  objects, one per matched preprocessed scan, instead of collapsing multiple
  files into a single container.
* Fix derivative discovery and legacy fMRIPrep helpers for datasets that place
  derivative subject folders directly under `derivatives/` with
  `prep_dir = "derivatives"`.
* Make `read_confounds()` raise informative, selection-aware errors when no
  matching participants, confound files, or usable requested confounds are
  found.
* Add run-level confound diagnostics and cleaning via `check_confounds()`,
  `clean_confounds()`, and `read_confounds(..., clean = )`, with zero-variance
  confounds dropped by default and recorded in a `confound_diagnostics`
  attribute.
* Fix `create_preproc_mask()` to ignore matching JSON sidecars and only read
  actual NIfTI mask images.
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
