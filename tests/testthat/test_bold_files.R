library(bidser)

# Create BIDS project
proj_path <- system.file("extdata/ds001", package="bidser")
proj <- bids_project(proj_path)

# List files in the first subject func directory
cat("Files in subject 01 func folder:\n")
func_files <- list.files(file.path(proj_path, "sub-01", "func"))
cat(paste(func_files, collapse="\n"), "\n\n")

# Try to find bold files with direct search_files
cat("search_files with regex='bold':\n")
bold_files <- search_files(proj, regex="bold", full_path=FALSE)
cat(paste(bold_files %||% "No files found", collapse="\n"), "\n\n")

# Try with explicit kind parameter
cat("search_files with kind='bold':\n")
kind_files <- search_files(proj, kind="bold", full_path=FALSE)
cat(paste(kind_files %||% "No files found", collapse="\n"), "\n\n")

# Try with both
cat("search_files with regex='bold' and kind='bold':\n")
both_files <- search_files(proj, regex="bold", kind="bold", full_path=FALSE)
cat(paste(both_files %||% "No files found", collapse="\n"), "\n\n")

# Try using func_scans
cat("func_scans:\n")
func_files <- func_scans(proj, full_path=FALSE)
cat(paste(func_files %||% "No files found", collapse="\n"), "\n\n")

# Check flat list output
cat("flat_list:\n")
fl <- flat_list(proj, full_path=FALSE)
cat("Number of files:", nrow(fl), "\n")
cat("Sample file names:\n")
if (nrow(fl) > 0) {
  cat(paste(head(fl$name), collapse="\n"), "\n")
} 