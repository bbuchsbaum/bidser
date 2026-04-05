# data-raw/update_bids_schema.R
#
# Downloads the compiled BIDS JSON schema from the official specification docs
# and vendors it into inst/bids-schema/.
#
# Run manually before a bidser release when a new BIDS version is available:
#
#   source("data-raw/update_bids_schema.R")
#   # or: Rscript data-raw/update_bids_schema.R 1.10.0
#
# The schema is published at:
#   https://bids-specification.readthedocs.io/en/v<version>/schema.json
#
# After running, commit the new schema file and update the `version` default
# in bids_schema() (R/schema.R) and bids_check_compliance() (R/bids.R).

args <- commandArgs(trailingOnly = TRUE)
version <- if (length(args) >= 1L) args[[1L]] else "1.10.0"

url  <- sprintf("https://bids-specification.readthedocs.io/en/v%s/schema.json", version)
dest <- file.path("inst", "bids-schema", sprintf("schema-%s.json", version))

if (file.exists(dest)) {
  message("Schema already present: ", dest, " — delete it first to re-download.")
  quit(status = 0L)
}

message("Downloading BIDS schema ", version, " from:\n  ", url)
tryCatch(
  download.file(url, dest, mode = "wb", quiet = FALSE),
  error = function(e) stop("Download failed: ", e$message, call. = FALSE)
)

# Quick sanity check
s <- jsonlite::read_json(dest)
actual_version <- s[["bids_version"]] %||% "(unknown)"
message("\nVendored successfully:")
message("  file:          ", dest)
message("  bids_version:  ", actual_version)
message("  schema_version:", s[["schema_version"]] %||% "(unknown)")

if (!grepl(version, actual_version, fixed = TRUE)) {
  warning(
    "bids_version in schema (", actual_version, ") ",
    "does not match requested version (", version, "). ",
    "Check the URL is correct for this release."
  )
}

message(
  "\nNext steps:\n",
  "  1. git add ", dest, "\n",
  "  2. Update default `version` arg in bids_schema()      [R/schema.R]\n",
  "  3. Update default `schema_version` in bids_check_compliance() [R/bids.R]\n",
  "  4. Run devtools::test() to confirm no regressions\n",
  "  5. Commit with message: 'chore: vendor BIDS schema ", version, "'"
)
