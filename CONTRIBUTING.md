# Contributing to bidser

Thank you for your interest in contributing to **bidser**!

## Getting Started

1. Fork the repository and clone your fork locally.
2. Install package dependencies in R:

```R
# install.packages("devtools")
devtools::install_deps()
```

3. Make sure the package builds and checks without errors:

```R
devtools::check()
```

## Making Changes

- Use feature branches for your work and keep commits focused.
- Add or update unit tests in `tests/testthat/` when appropriate.
- Generate documentation with `devtools::document()` and ensure examples run.

## Submitting Pull Requests

1. Ensure `R CMD check` passes with no errors or warnings:

```R
devtools::check()
```

2. Open a pull request on GitHub and describe your changes.
3. Be responsive to review feedback and update your branch as needed.

We appreciate all contributions that help improve **bidser**.
