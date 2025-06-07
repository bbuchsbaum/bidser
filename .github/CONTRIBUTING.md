# Contributing to bidser

Thank you for your interest in contributing to bidser! This document outlines the development workflow and continuous integration setup.

## Development Workflow

1. **Fork** the repository and create a new branch for your feature/fix
2. **Make changes** following the existing code style
3. **Add tests** for any new functionality
4. **Update documentation** if needed
5. **Submit a pull request** with a clear description

## Continuous Integration

This package uses GitHub Actions for continuous integration with the following workflows:

### Core Workflows

- **R-CMD-check.yaml**: Comprehensive package checking across multiple operating systems (Ubuntu, macOS, Windows) and R versions (devel, release, oldrel-1)
- **test-coverage.yaml**: Runs test suite and uploads coverage reports to Codecov
- **lint.yaml**: Code style checking using the `lintr` package

### Documentation Workflows

- **pkgdown.yaml**: Builds and deploys the package website to GitHub Pages
- **render-readme.yaml**: Automatically updates README.md when README.Rmd changes

### Quality Assurance

- **pkgcheck.yaml**: Additional package validation using rOpenSci tools

## Code Style

- We use `lintr` for code style checking (configured in `.lintr`)
- Line length limit: 120 characters
- Follow existing naming conventions in the codebase

## Testing

- All new functions should have corresponding tests in `tests/testthat/`
- Tests are run automatically on all platforms via GitHub Actions
- Aim for good test coverage (current coverage tracked via Codecov)

## Local Development

```r
# Install development dependencies
devtools::install_dev_deps()

# Run tests locally
devtools::test()

# Check package
devtools::check()

# Check code style
lintr::lint_package()
```

## Example Data

The package uses BIDS example datasets for testing and documentation. These are cached to improve performance:

- Don't call `unlink()` on dataset paths in examples
- Use `get_example_bids_dataset()` which has intelligent caching
- Clear cache if needed with `clear_example_bids_cache()`

## Documentation

- All functions must be documented with roxygen2
- Include `@examples` for all exported functions
- Use `\donttest{}` for examples requiring internet
- Vignettes should demonstrate real-world usage

## Questions?

Feel free to open an issue for questions about contributing! 