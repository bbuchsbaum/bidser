# AGENTS.md

Guidance for AI coding agents working in the **bidser** repository. This is the
tool-agnostic entry point; the detailed, authoritative project instructions live
in **[CLAUDE.md](./CLAUDE.md)** and apply to every agent regardless of which tool
you are — read it first.

## What this is

`bidser` is an R package (currently v0.4.0) for reading, validating, and querying
[BIDS](https://bids.neuroimaging.io/)-formatted neuroimaging datasets, including
fMRIPrep derivatives. It uses S3 classes, parser combinators (Combin8R) for
filename parsing, and a `data.tree` representation of the BIDS hierarchy. See the
"Architecture Overview" in [CLAUDE.md](./CLAUDE.md) for the component map.

## Start here

1. **Read [CLAUDE.md](./CLAUDE.md)** — the canonical instructions: dev commands,
   architecture, key design patterns, dependencies, and the **EcoOracle
   (Oracle-first)** rule for ecosystem lookups. Everything below is a summary or
   an agent-specific addition, not a replacement.
2. Skim `README.md` and `bidser_cheatsheet.md` for the public API surface.

## Common commands

Detailed versions are in [CLAUDE.md](./CLAUDE.md#development-commands). The
essentials, run from an R session at the repo root:

```r
devtools::load_all()                       # fast iteration (preferred over install)
devtools::test()                           # run all tests
testthat::test_file("tests/testthat/test_query_files.R")  # one file
devtools::test(filter = "query_files")     # tests matching a pattern
devtools::document()                       # regenerate man/ from roxygen
devtools::check()                          # R CMD check
covr::package_coverage()                   # test coverage
```

Notes for agents:
- Prefer `devtools::load_all(".")` for verifying internal changes — it avoids the
  install step. Reinstall (`devtools::install(quick = TRUE)`) only when a test
  needs a newly *exported* symbol.
- After editing roxygen comments, run `devtools::document()` and commit the
  regenerated `man/*.Rd` and `NAMESPACE`.
- Edited R files are auto-formatted by an `air`-based hook; do not hand-reformat.

## Conventions & quality bar

- **CRAN-clean is the target.** `R CMD check` and `lintr::lint_package()` should
  be clean; R-universe runs lintr as a quality gate.
- **Verify before claiming done.** Run the tests / check / build and cite the
  result. Evidence over assertion.
- **Never weaken a test to make it pass.** If a test surfaces a bug in library
  code, fix the library. If a test is genuinely wrong, say so explicitly.
- Match the surrounding code: S3 dispatch, tibble-returning queries, tidyverse
  idioms, and regex-based filtering are the house style.
- Keep the two query paths in sync: `query_files()` (indexed) and its
  `search_files()` / `use_index = "never"` fallback should return the same
  results for the same arguments.

## Git & PR hygiene

- Commit or push only when asked. Don't commit directly to `master` for
  substantial work — branch first.
- Confirm outward-facing or hard-to-reverse actions before taking them.
