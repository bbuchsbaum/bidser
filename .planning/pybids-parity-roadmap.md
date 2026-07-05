# bidser → pybids parity roadmap

_Date: 2026-07-03. Grounded in a head-to-head benchmark of bidser 0.4.0 vs pybids
0.22.0 on `pybids/tests/data/ds005` (375 files), plus full capability inventories
of both libraries._

## TL;DR

bidser is already broad (querying, metadata inheritance, confounds, events,
mock data, packing, plotting, BIDS URIs). The two things standing between it and
"on par but superior" are:

1. **Speed.** As of 0.4.0 bidser was **12× slower to construct and ~70–84×
   slower per query** than pybids. The fast infrastructure (a `data.table`
   manifest index) already existed but was not the live path.
2. **The modeling stack.** bidser lacks pybids' differentiators: a real
   variable-collection / design-matrix engine, the transformation framework, and
   the BIDS Stats Models execution graph. R's formula/modeling ergonomics make
   this the area where bidser can *beat* pybids, not just match it.

## Benchmark (ds005, 375 files)

| Operation | pybids 0.22 | bidser 0.4.0 | bidser after Phase 0 |
|---|---|---|---|
| construct | 167 ms | 2074 ms | 2074 ms (unchanged) |
| query subject=01, cold | 1.4 ms | 87 ms | ~1.3 ms |
| query subject=01, warm | 1.0 ms | 103 ms | **1.3 ms** |

Construction scales at ~4.8 ms/file (data.tree build); a 30k-file fMRIPrep tree
≈ **2.5 min** just to build the project object. The `data.table` index builds at
~0.42 ms/file (~10× faster).

### Follow-up benchmark harness (2026-07-05)

`tools/benchmark-pybids-parity.R --reps 11` now compares the live checkout against
the vendored pybids source through `uv run --with ./pybids`.

| Operation | pybids 0.21.0.post0.dev67 | bidser after DWI + constructor hot-path work |
|---|---:|---:|
| construct (no index) | 118 ms | **60 ms** |
| construct + build query index | n/a | **92 ms** |
| construct from cached index | n/a | **70 ms** |
| query subject=01 bold | 1.3 ms | 2.0 ms |
| query DWI | 1.2 ms | 2.0 ms |
| indexed raw file count | 142 | 130 |

The query surface is close, and ds005 construction is now faster than pybids even
when bidser also builds the query index. The improvement came from avoiding a
generic `search_files()` tree walk during manifest construction, vectorizing
manifest rows and file signatures, avoiding repeated finalization copies, using a
leaf-specific `tbl` extractor instead of `data.tree::ToDataFrameTypeCol()`, and
trimming parser/directory-listing overhead in constructor hot paths. Remaining
quality/parity gap: bidser now indexes DWI files by default, but still omits
pybids-visible root/scan metadata files, hence 130 vs 142 files on ds005.

## Parity map (what bidser already has)

| pybids crown jewel | bidser status |
|---|---|
| Metadata inheritance (`get_metadata`/`get_nearest`) | **Have** — `get_metadata()`, `read_sidecar(inherit=)`, resolved-meta memo table |
| `.get()` query engine | **Have** — `query_files()` (match_mode, scope, pipeline, require_entity, return); `search_files()` |
| Persisted index for instant reopen | **Partial** — RDS cache + incremental mtime refresh (not SQLite; no `load()` fast-open story) |
| Confounds / regressors | **Have and arguably ahead** — `read_confounds()`, `confound_set()`, `confound_strategy()`, diagnostics/cleaning |
| Sparse→dense variables + design matrix | **Missing** — `variables_table()` is a thin nested tibble |
| Transformation framework (24 transforms) | **Missing** — only `create_smooth_transformer()` |
| BIDS Stats Models graph | **Missing** |
| `build_path` / path-pattern writer | **Missing** (internal helpers only) |
| Auto methods-section report | **Partial** — `bids_report()` is a stat block, not prose |
| Non-MRI modalities (EEG/MEG/PET/DWI/ASL/physio…) | **Missing** — MRI-only parsers |
| Remote/S3 datasets | **Missing** |
| Schema-driven config/entities | **Partial** — schema bundled, used only for compliance, not to drive parsing |

## Phases

### Phase 0 — Query performance (SHIPPED)
`query_files()` now reuses the construction-time index instead of re-`stat`-ing
the dataset on every call. Snapshot semantics (like pybids); `refresh = TRUE`
re-stats known files (content changes / removals). Warm queries **103 ms → ~1.3
ms** (~78×), matching pybids. Suite green (1678 pass / 0 fail); three regression
tests added.

An independent review caught two correctness regressions in the first cut, both
now fixed: (a) two project objects on the same path share a path-keyed index
cache — `query_files()` now prefers each project's own carried `index_state` so a
sibling project can't corrupt results; (b) `query_files()` now honours
`index = "none"` and won't read a stale leftover RDS. Known limitation carried
into Phase 1: `refresh = TRUE` cannot detect *newly added* files because the
manifest's file list is still derived from the construction-time `data.tree`
leaves. Phase 1's manifest-from-FS-walk removes this limitation for free.

### Phase 1 — Construction performance (PARTLY SHIPPED; HIGH value, MEDIUM–HIGH risk)
The first construction-speed slice cut ds005 construction below pybids while
preserving the public `bids_project` shape. Remaining Phase 1 work is now about
correctness/completeness and scaling on larger trees: build the `data.table`
manifest directly from a single recursive filesystem walk + vectorized entity
parse, **decoupled from the data.tree**. Then either (a) build the `data.tree`
lazily on first access by a tree-consuming accessor, or (b) reimplement the hot
accessors (`func_scans`, `sessions`, `tasks`, `flat_list`, `preproc_scans`,
`build_subject_graph`) on the manifest and demote the tree.
- Risk: the `data.tree` is reached into directly by many methods — needs a careful
  audit and a fresh-context review pass before merge.
- Target: preserve the ds005 pybids construction win and close the 130-vs-142
  file-count gap without regressing query timings.
- Suggested: land (a) lazy tree first (lower risk), measure, then migrate
  accessors opportunistically.

### Phase 2 — Variables & design matrices (HIGH value, MEDIUM–HIGH complexity)
Port pybids' `variables` layer with idiomatic R:
- `SparseRunVariable` (events), `DenseRunVariable` (physio/confounds/regressors),
  `SimpleVariable` (participants/sessions/scans).
- `to_dense(sampling_rate)`: GCD-of-onsets binning + anti-aliased resample
  (Butterworth + interp) → densified regressors; `resample('TR')` to volume count.
- `bids_variable_collection` with `to_df(format=, sampling_rate=)` producing a
  tidy/wide design matrix. This upgrades the existing `variables_table()`.
- Leverage R strengths: `stats::fft`/`signal`, `model.matrix`, list-columns.

### Phase 3 — Transformations (MEDIUM–HIGH)
The 24 BSM transforms (Convolve, Scale, Demean, Orthogonalize, Factor, Filter,
Split, Threshold, Sum, Product, Lag, Assign, Rename, Replace, Select, …) over the
variable model from Phase 2. HRF convolution (`spm`/`glover` + derivatives, FIR).
R's S3/S7 + formulas can be cleaner than pybids' `__new__`-dispatch metaprogramming.

### Phase 4 — BIDS Stats Models execution graph (HIGH differentiation, HIGH)
Parse a BSM JSON → node graph (Run/Session/Subject/Dataset) → group-by fan-out →
per-group design matrix + `ContrastInfo` propagation up edges. `auto_model()` to
synthesize a default spec. Interop target: FitLins-compatible output. Builds the
design/contrast spec; estimation stays downstream (nilearn/fmrireg/lme4).
Natural bridge to the user's `fmrireg`/`fmri*` packages.

### Phase 5 — `build_path` writer (MEDIUM, self-contained)
Public path-pattern DSL (`{entity<allowed>|default}`, `[optional]`) +
`build_path(entities, patterns, strict)` + `write_to_file(conflict=)`. Reuse the
internal `generate_bids_path` helpers. High value for emitting derivatives with
correct names; unblocks a real `bids_transform` writer.

### Phase 6 — Schema-driven parsing & validation (MEDIUM)
Drive entity/suffix/datatype recognition from the bundled `bids-schema`
(1.10.0) instead of hardcoded parser regexes, so new BIDS versions and modalities
come "for free." Strengthen `bids_check_compliance()` toward real validation
(required metadata, cross-file rules) — optionally shell out to `bids-validator`
when present.

### Phase 7 — Modalities & remote (MEDIUM each, do as demand appears)
- Modalities: register parsers/readers for EEG/MEG/iEEG, PET, ASL/perfusion, DWI
  (`get_bval`/`get_bvec`), physio (`.tsv.gz`). Schema-driven parsing (Phase 6)
  makes most of this indexing "free"; the work is typed readers + accessors.
- Remote: an S3/`https` reader behind the existing `resolve_bids_uri()` +
  index-from-manifest so datasets need not be fully local (paws/aws.s3/arrow).

## Sequencing recommendation

Phase 0 (done) → Phase 1 (construction speed; makes "faster" true end-to-end) →
Phase 5 (`build_path`; small, unblocks writing) → Phase 2 → 3 → 4 (the modeling
stack, the real differentiator) → Phase 6/7 as demand appears. Each phase ships
independently, keeps the suite green, and gets a fresh-context review before merge.

## Design principles for "superior, not just equal"
- **One backend.** Make the `data.table` manifest the single source of truth;
  the tree becomes a lazily-derived view, not a parallel structure to keep in sync.
- **Snapshot + explicit refresh** (Phase 0 pattern) everywhere — no hidden
  per-call `stat` storms.
- **Vectorized parsing** over per-file `vapply` rescans (several remain in
  `query.R`: extension/datatype/scope/`require_entity`).
- **Tidy outputs** (tibbles, list-columns) beat pybids' object soup for R users.
- **Interop with the fmri* ecosystem** as the north star for the modeling stack.
