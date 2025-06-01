# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Building and Installation
```R
# Install package dependencies
devtools::install_deps()

# Build and install the package
devtools::install()

# Build and check the package
devtools::check()

# Run R CMD check (comprehensive)
devtools::check(cran = TRUE)
```

### Testing
```R
# Run all tests
devtools::test()

# Run tests with coverage
covr::package_coverage()

# Run a specific test file
testthat::test_file("tests/testthat/test_bids_subject.R")

# Run tests matching a pattern
devtools::test(filter = "mock_bids")
```

### Documentation
```R
# Generate documentation from roxygen comments
devtools::document()

# Build package website
pkgdown::build_site()

# Check examples in documentation
devtools::run_examples()
```

## Architecture Overview

bidser is an R package for working with Brain Imaging Data Structure (BIDS) formatted neuroimaging datasets. It provides tools for reading, validating, and querying BIDS-compliant projects, including support for fMRIPrep derivatives.

### Core Components

1. **Parser System** (`R/matchers.R`, `R/specs.R`)
   - Uses the Combin8R parser combinator library to parse BIDS filenames
   - Parsers extract metadata (subject, session, task, run, etc.) from standardized BIDS naming conventions
   - Key parsers: `bids_parser()`, `func_parser()`, `anat_parser()`, `fmriprep_func_parser()`, `fmriprep_anat_parser()`

2. **BIDS Project Object** (`R/bids.R`)
   - Main entry point: `bids_project()` creates a project object from a BIDS directory
   - Scans directory structure and builds a hierarchical data.tree representation
   - Supports both raw BIDS data and fMRIPrep derivatives
   - S3 class with methods for querying and extracting data

3. **Generic Functions** (`R/all_generic.R`)
   - Defines S3 generics for common operations across BIDS objects
   - Key generics: `parse()`, `encode()`, `sessions()`, `tasks()`, `participants()`, `func_scans()`, `event_files()`, `confound_files()`, `read_events()`, `read_confounds()`

4. **Mock BIDS Creation** (`R/mock_bids.R`)
   - Tools for creating mock/test BIDS datasets
   - Useful for package testing and demonstrating functionality

5. **Visualization** (`R/plot_bids.R`, `R/plot_bids_project.R`)
   - Functions for creating visual summaries of BIDS datasets
   - Heatmaps, completeness plots, task distributions

### Key Design Patterns

- **S3 Object System**: Package uses S3 classes and methods extensively
- **Parser Combinators**: Filename parsing uses functional programming approach with Combin8R
- **Tree Structure**: BIDS hierarchy represented using data.tree package
- **Tibble-based Output**: Most data returns use tibbles from tidyverse
- **Regex Filtering**: Most query functions accept regex patterns for flexible filtering

### Dependencies

Critical dependencies:
- `Combin8R`: Parser combinator library (GitHub: SWotherspoon/Combin8R)
- `neuroim2`: Neuroimaging data structures (GitHub: bbuchsbaum/neuroim2)
- `data.tree`: Hierarchical data representation
- Tidyverse packages: `dplyr`, `tidyr`, `purrr`, `readr`, `tibble`

### Common Workflows

1. **Loading a BIDS project**:
   ```R
   proj <- bids_project("/path/to/bids/dataset", fmriprep = TRUE)
   ```

2. **Querying data**:
   ```R
   # Get functional scans for subject 01
   func_scans(proj, subid = "01")
   
   # Read event files for a specific task
   read_events(proj, task = "rest")
   ```

3. **Working with preprocessed data**:
   ```R
   # Get preprocessed scans
   preproc_scans(proj, space = "MNI152NLin2009cAsym")
   
   # Read confound files
   read_confounds(proj, subid = "01", task = "rest")
   ```