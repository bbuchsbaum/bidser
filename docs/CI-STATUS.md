# GitHub Actions Workflows Summary

The following CI workflows are configured for this package:

✅ **R-CMD-check**: Tests on Ubuntu/macOS/Windows with multiple R
versions  
✅ **test-coverage**: Runs tests and reports coverage to Codecov  
✅ **lint**: Code style checking with lintr  
✅ **pkgdown**: Documentation website deployment  
✅ **render-readme**: Auto-updates README.md from README.Rmd  
✅ **pkgcheck**: Additional package validation

All workflows run on push/PR to main/master branches.

## Workflow Details

### R-CMD-check.yaml

- **Platforms**: Ubuntu, macOS, Windows
- **R Versions**: devel, release, oldrel-1
- **Purpose**: Comprehensive package validation

### test-coverage.yaml

- **Platform**: Ubuntu  
- **Purpose**: Test execution and coverage reporting to Codecov

### lint.yaml

- **Platform**: Ubuntu
- **Purpose**: Code style validation using lintr

### pkgdown.yaml

- **Platform**: Ubuntu  
- **Purpose**: Build and deploy documentation website

### render-readme.yaml

- **Platform**: Ubuntu
- **Trigger**: Changes to README.Rmd
- **Purpose**: Auto-generate README.md

### pkgcheck.yaml

- **Platform**: Ubuntu
- **Purpose**: rOpenSci package validation tools
