#' Helper function to downsample a single neuroimaging file
#' @keywords internal
#' @noRd
downsample_single_file <- function(from_file, to_file, factor, method = "box", verbose = FALSE) {
  tryCatch({
    # Try to read the file
    img <- NULL
    
    # First try to read as 4D (functional)
    tryCatch({
      img <- neuroim2::read_vec(from_file)
      is_4d <- TRUE
    }, error = function(e) {
      # If 4D fails, try 3D
      img <<- neuroim2::read_vol(from_file)
      is_4d <<- FALSE
    })
    
    if (is.null(img)) {
      if (verbose) message(sprintf("  Could not read %s, creating stub instead", basename(from_file)))
      file.create(to_file)
      return(list(success = FALSE, type = "stub"))
    }
    
    # Downsample the image
    img_down <- neuroim2::downsample(img, factor = factor, method = method)
    
    # Write the downsampled image
    neuroim2::write_vol(img_down, to_file)
    
    if (verbose) {
      orig_dims <- dim(img)
      new_dims <- dim(img_down)
      dims_str <- if (is_4d) {
        sprintf("%dx%dx%dx%d -> %dx%dx%dx%d", 
                orig_dims[1], orig_dims[2], orig_dims[3], orig_dims[4],
                new_dims[1], new_dims[2], new_dims[3], new_dims[4])
      } else {
        sprintf("%dx%dx%d -> %dx%dx%d",
                orig_dims[1], orig_dims[2], orig_dims[3],
                new_dims[1], new_dims[2], new_dims[3])
      }
      message(sprintf("  Downsampled %s (%s)", basename(from_file), dims_str))
    }
    
    return(list(success = TRUE, type = "downsampled", is_4d = is_4d))
    
  }, error = function(e) {
    if (verbose) {
      message(sprintf("  Error downsampling %s: %s", basename(from_file), e$message))
      message("  Creating stub file instead")
    }
    file.create(to_file)
    return(list(success = FALSE, type = "stub", error = e$message))
  })
}

#' Add resolution tag to BIDS filename
#' @keywords internal
#' @noRd
add_resolution_tag <- function(filename, factor) {
  # Parse the filename to get entities
  base_name <- basename(filename)
  dir_name <- dirname(filename)
  
  # Determine resolution label based on factor
  res_label <- if (factor == 0.25) {
    "low4x"
  } else if (factor == 0.5) {
    "low2x"
  } else {
    sprintf("low%dx", as.integer(1/factor))
  }
  
  # Check if filename already has res- tag
  if (grepl("_res-", base_name)) {
    # Replace existing res tag
    new_name <- gsub("_res-[^_]+", paste0("_res-", res_label), base_name)
  } else {
    # Add res tag before the suffix (last underscore part)
    # Find the last underscore before the file extension
    parts <- strsplit(base_name, "_")[[1]]
    suffix_with_ext <- parts[length(parts)]
    
    # Insert res tag before suffix
    if (length(parts) > 1) {
      parts <- c(parts[-length(parts)], paste0("res-", res_label), suffix_with_ext)
      new_name <- paste(parts, collapse = "_")
    } else {
      # Single part filename (shouldn't happen with BIDS but handle it)
      new_name <- gsub("\\.", paste0("_res-", res_label, "."), base_name)
    }
  }
  
  file.path(dir_name, new_name)
}

#' Pack BIDS Project with Stub or Downsampled Imaging Files
#'
#' This function creates a compressed archive (tar.gz or zip) of a BIDS project,
#' either replacing large imaging files (.nii, .nii.gz) with 0-byte stub files
#' or downsampling them to lower resolution while preserving all metadata files 
#' (JSON, TSV, etc.) with their full content. This is useful for sharing BIDS 
#' project structure and metadata without the large imaging data.
#'
#' @param x A `bids_project` object created by \code{\link{bids_project}}.
#' @param output_file Character string specifying the output archive filename.
#'   Should end with ".tar.gz" or ".zip". If not specified, defaults to
#'   "{project_name}_metadata.tar.gz" in the current directory.
#' @param format Character string specifying the archive format. Can be "tar.gz"
#'   (default) or "zip". If not specified, inferred from output_file extension.
#' @param include_derivatives Logical. Whether to include fMRIPrep derivatives
#'   if available. Default is TRUE.
#' @param downsample_factor Numeric value between 0 and 1 specifying the 
#'   downsampling factor for imaging files. If NULL (default), creates stub files.
#'   A value of 0.25 reduces dimensions by 4x (e.g., 64x64x64 becomes 16x16x16).
#'   Time dimension is preserved for 4D files.
#' @param downsample_method Character string specifying the downsampling method.
#'   Currently only "box" (box averaging) is supported. Default is "box".
#' @param ncores Integer specifying the number of cores for parallel processing
#'   during downsampling. Default is 1 (sequential). Values > 1 enable parallel
#'   processing if the 'future' package is available.
#' @param verbose Logical. Whether to print progress messages. Default is TRUE.
#' @param temp_dir Character string specifying the temporary directory for
#'   creating the archive. If NULL (default), uses tempdir().
#' @param cleanup Logical. Whether to clean up the temporary directory after
#'   creating the archive. Default is TRUE.
#'
#' @return Character string containing the path to the created archive file.
#'   Returns NULL if the operation fails.
#'
#' @details
#' The function works by:
#' \enumerate{
#'   \item Creating a temporary copy of the BIDS project structure
#'   \item Replacing all .nii and .nii.gz files with 0-byte stub files
#'   \item Preserving all other files (JSON, TSV, TXT, etc.) with full content
#'   \item Creating a compressed archive of the modified structure
#' }
#'
#' This allows researchers to share BIDS dataset structure and metadata
#' without the large imaging files, which is useful for:
#' \itemize{
#'   \item Sharing dataset organization and metadata for review
#'   \item Creating lightweight references for dataset structure
#'   \item Testing BIDS tools without full datasets
#' }
#'
#' @examples
#' \donttest{
#' # Create a BIDS project and pack it
#' tryCatch({
#'   ds_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds_path)
#'   
#'   # Pack with default settings (tar.gz with stub files)
#'   archive_path <- pack_bids(proj)
#'   
#'   # Pack with downsampling (4x reduction)
#'   archive_downsampled <- pack_bids(proj, 
#'                                    downsample_factor = 0.25,
#'                                    output_file = "ds001_low4x.tar.gz")
#'   
#'   # Pack with downsampling using parallel processing
#'   if (requireNamespace("future", quietly = TRUE)) {
#'     archive_parallel <- pack_bids(proj, 
#'                                  downsample_factor = 0.5,
#'                                  ncores = 2,
#'                                  output_file = "ds001_low2x.tar.gz")
#'   }
#'   
#'   # Pack as zip file
#'   zip_path <- pack_bids(proj, output_file = "ds001_metadata.zip")
#'   
#'   # Pack without derivatives
#'   archive_no_deriv <- pack_bids(proj, include_derivatives = FALSE)
#'   
#'   # Clean up
#'   unlink(c(archive_path, archive_downsampled, zip_path, archive_no_deriv))
#'   if (exists("archive_parallel")) unlink(archive_parallel)
#'   unlink(ds_path, recursive = TRUE)
#' }, error = function(e) {
#'   message("Example failed: ", e$message)
#' })
#' }
#'
#' @export
#' @importFrom utils tar zip
pack_bids <- function(x, 
                      output_file = NULL,
                      format = NULL,
                      include_derivatives = TRUE,
                      downsample_factor = NULL,
                      downsample_method = "box",
                      ncores = 1,
                      verbose = TRUE,
                      temp_dir = NULL,
                      cleanup = TRUE) {
  
  # Validate input
  if (!inherits(x, "bids_project") && !inherits(x, "mock_bids_project")) {
    stop("Input must be a bids_project or mock_bids_project object")
  }
  
  # Validate downsampling parameters
  if (!is.null(downsample_factor)) {
    if (!is.numeric(downsample_factor) || length(downsample_factor) != 1) {
      stop("downsample_factor must be a single numeric value")
    }
    if (downsample_factor <= 0 || downsample_factor > 1) {
      stop("downsample_factor must be between 0 (exclusive) and 1 (inclusive)")
    }
    if (downsample_method != "box") {
      stop("Only 'box' downsampling method is currently supported")
    }
    # Check for neuroim2 package
    if (!requireNamespace("neuroim2", quietly = TRUE)) {
      stop("The 'neuroim2' package is required for downsampling. Please install it.")
    }
  }
  
  # Validate ncores
  if (!is.numeric(ncores) || length(ncores) != 1 || ncores < 1) {
    stop("ncores must be a positive integer")
  }
  ncores <- as.integer(ncores)
  
  # Check for parallel processing capability
  use_parallel <- FALSE
  if (!is.null(downsample_factor) && ncores > 1) {
    if (requireNamespace("future", quietly = TRUE) && 
        requireNamespace("future.apply", quietly = TRUE)) {
      use_parallel <- TRUE
      if (verbose) {
        message(sprintf("Using parallel processing with %d cores for downsampling", ncores))
      }
    } else if (verbose) {
      message("Parallel processing requested but 'future' or 'future.apply' package not available.")
      message("Using sequential processing. Install these packages for parallel support.")
    }
  }
  
  # Determine project name and paths
  project_name <- x$name
  project_path <- x$path
  
  # Determine output file and format
  if (is.null(output_file)) {
    output_file <- paste0(project_name, "_metadata.tar.gz")
  }
  
  # Infer format from extension if not specified
  if (is.null(format)) {
    if (grepl("\\.zip$", output_file, ignore.case = TRUE)) {
      format <- "zip"
    } else if (grepl("\\.tar\\.gz$|\\.tgz$", output_file, ignore.case = TRUE)) {
      format <- "tar.gz"
    } else {
      format <- "tar.gz"
      # Add extension if missing
      if (!grepl("\\.tar\\.gz$|\\.tgz$", output_file)) {
        output_file <- paste0(output_file, ".tar.gz")
      }
    }
  }
  
  # Validate format
  if (!format %in% c("tar.gz", "zip")) {
    stop("Format must be 'tar.gz' or 'zip'")
  }
  
  # Create temporary directory
  if (is.null(temp_dir)) {
    temp_dir <- tempdir()
  }
  
  # Use shorter directory name to avoid path length issues
  temp_project_dir <- file.path(temp_dir, project_name)
  
  if (verbose) {
    message("Creating temporary copy of BIDS project...")
  }
  
  # Create the temporary directory
  dir.create(temp_project_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Function to process files during copy
  process_and_copy <- function(from_path, to_path) {
    # Get all files recursively
    all_files <- list.files(from_path, recursive = TRUE, all.files = FALSE, 
                           full.names = FALSE, include.dirs = FALSE)
    
    # Filter files based on include_derivatives setting
    if (!include_derivatives && x$has_fmriprep) {
      # Remove derivative files
      deriv_pattern <- paste0("^", x$prep_dir, "/")
      all_files <- all_files[!grepl(deriv_pattern, all_files)]
    }
    
    total_files <- length(all_files)
    if (verbose && total_files > 0) {
      message(sprintf("Processing %d files...", total_files))
    }
    
    # Separate imaging and non-imaging files
    is_imaging <- grepl("\\.nii(\\.gz)?$", all_files, ignore.case = TRUE)
    imaging_files <- all_files[is_imaging]
    non_imaging_files <- all_files[!is_imaging]
    
    # Process non-imaging files (always copy)
    for (rel_path in non_imaging_files) {
      from_file <- file.path(from_path, rel_path)
      to_file <- file.path(to_path, rel_path)
      
      # Create directory if needed
      to_dir <- dirname(to_file)
      if (!dir.exists(to_dir)) {
        dir.create(to_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Copy the actual file
      file.copy(from_file, to_file, overwrite = TRUE)
    }
    
    if (verbose && length(non_imaging_files) > 0) {
      message(sprintf("  Copied %d non-imaging files", length(non_imaging_files)))
    }
    
    # Process imaging files
    if (length(imaging_files) > 0) {
      if (!is.null(downsample_factor)) {
        # Downsampling mode
        if (verbose) {
          message(sprintf("  Downsampling %d imaging files (factor: %.2f)...", 
                         length(imaging_files), downsample_factor))
        }
        
        # Setup parallel processing if applicable
        if (use_parallel && length(imaging_files) > 1) {
          # Set up future plan
          old_plan <- future::plan()
          on.exit(future::plan(old_plan), add = TRUE)
          future::plan(future::multisession, workers = ncores)
          
          # Process files in parallel
          results <- future.apply::future_lapply(imaging_files, function(rel_path) {
            from_file <- file.path(from_path, rel_path)
            # Add resolution tag to output filename
            to_file_with_res <- bidser:::add_resolution_tag(file.path(to_path, rel_path), downsample_factor)
            
            # Create directory if needed
            to_dir <- dirname(to_file_with_res)
            if (!dir.exists(to_dir)) {
              dir.create(to_dir, recursive = TRUE, showWarnings = FALSE)
            }
            
            # Downsample the file
            bidser:::downsample_single_file(from_file, to_file_with_res, 
                                 factor = downsample_factor, 
                                 method = downsample_method,
                                 verbose = FALSE)
          }, future.seed = TRUE)
          
        } else {
          # Sequential processing
          results <- lapply(imaging_files, function(rel_path) {
            from_file <- file.path(from_path, rel_path)
            # Add resolution tag to output filename
            to_file_with_res <- add_resolution_tag(file.path(to_path, rel_path), downsample_factor)
            
            # Create directory if needed
            to_dir <- dirname(to_file_with_res)
            if (!dir.exists(to_dir)) {
              dir.create(to_dir, recursive = TRUE, showWarnings = FALSE)
            }
            
            # Downsample the file
            downsample_single_file(from_file, to_file_with_res, 
                                 factor = downsample_factor, 
                                 method = downsample_method,
                                 verbose = verbose)
          })
        }
        
        # Summary of results
        if (verbose) {
          n_downsampled <- sum(sapply(results, function(r) r$type == "downsampled"))
          n_stubs <- sum(sapply(results, function(r) r$type == "stub"))
          message(sprintf("  Successfully downsampled: %d files", n_downsampled))
          if (n_stubs > 0) {
            message(sprintf("  Created stubs for %d files (downsampling failed)", n_stubs))
          }
        }
        
      } else {
        # Stub file mode (original behavior)
        for (rel_path in imaging_files) {
          to_file <- file.path(to_path, rel_path)
          
          # Create directory if needed
          to_dir <- dirname(to_file)
          if (!dir.exists(to_dir)) {
            dir.create(to_dir, recursive = TRUE, showWarnings = FALSE)
          }
          
          # Create 0-byte stub file
          file.create(to_file)
        }
        
        if (verbose) {
          message(sprintf("  Created %d stub files for imaging data", length(imaging_files)))
        }
      }
    }
  }
  
  # Process and copy files
  tryCatch({
    process_and_copy(project_path, temp_project_dir)
    
    if (verbose) {
      message("Creating archive...")
    }
    
    # Create archive based on format
    # Ensure output_path is an absolute path
    if (!grepl("^/", output_file) && !grepl("^[A-Za-z]:", output_file)) {
      output_path <- file.path(getwd(), output_file)
    } else {
      output_path <- output_file
    }
    
    if (format == "tar.gz") {
      # Change to parent directory for cleaner archive paths
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(temp_dir)
      
      # Create tar.gz archive with basename to avoid full path issues
      temp_archive <- paste0(basename(temp_project_dir), ".tar.gz")
      tar(temp_archive, files = basename(temp_project_dir), 
          compression = "gzip", tar = "internal")
      
      # Move to final location
      file.rename(temp_archive, output_path)
      
    } else if (format == "zip") {
      # For zip, we need to get all files in the temp directory
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(temp_dir)
      
      # Get the project directory name
      proj_dir_name <- basename(temp_project_dir)
      
      # Get all files relative to the project directory
      files_to_zip <- list.files(proj_dir_name, recursive = TRUE, all.files = FALSE)
      
      if (length(files_to_zip) > 0) {
        # Prepend the project directory name to each file
        files_with_dir <- file.path(proj_dir_name, files_to_zip)
        zip(output_path, files = files_with_dir)
      } else {
        warning("No files found to archive")
        return(NULL)
      }
    }
    
    if (verbose) {
      # Get archive size
      size_mb <- file.size(output_path) / (1024^2)
      message(sprintf("Archive created successfully: %s (%.2f MB)", 
                     output_path, size_mb))
      
      # Count files in archive
      if (format == "tar.gz") {
        # Note: Can't easily count files in tar.gz without external tools
        message("Archive contains project structure with stub imaging files")
      }
    }
    
    # Cleanup if requested
    if (cleanup && dir.exists(temp_project_dir)) {
      unlink(temp_project_dir, recursive = TRUE)
      if (verbose) {
        message("Temporary files cleaned up")
      }
    }
    
    return(output_path)
    
  }, error = function(e) {
    warning("Failed to create archive: ", e$message)
    # Cleanup on error
    if (cleanup && dir.exists(temp_project_dir)) {
      unlink(temp_project_dir, recursive = TRUE)
    }
    return(NULL)
  })
}

#' List Contents of Packed BIDS Archive
#'
#' This function lists the contents of a BIDS archive created by \code{\link{pack_bids}},
#' showing file sizes and identifying which files are stubs.
#'
#' @param archive_path Character string specifying the path to the archive file.
#' @param verbose Logical. Whether to print summary statistics. Default is TRUE.
#'
#' @return A data frame with columns:
#'   \item{file}{Relative file path within the archive}
#'   \item{size}{File size in bytes}
#'   \item{is_stub}{Logical indicating if the file is a 0-byte stub}
#'   \item{is_downsampled}{Logical indicating if the file is a downsampled image}
#'   \item{type}{File type based on extension (imaging, imaging_stub, imaging_downsampled, json, tsv, etc.)}
#'
#' @examples
#' \donttest{
#' # Create and inspect a packed BIDS archive
#' tryCatch({
#'   ds_path <- get_example_bids_dataset("ds001") 
#'   proj <- bids_project(ds_path)
#'   archive_path <- pack_bids(proj, verbose = FALSE)
#'   
#'   # List contents
#'   contents <- list_pack_bids(archive_path)
#'   
#'   # Show stub files
#'   stub_files <- contents[contents$is_stub, ]
#'   print(head(stub_files))
#'   
#'   # Clean up
#'   unlink(archive_path)
#'   unlink(ds_path, recursive = TRUE)
#' }, error = function(e) {
#'   message("Example failed: ", e$message)
#' })
#' }
#'
#' @export
#' @importFrom utils untar unzip
list_pack_bids <- function(archive_path, verbose = TRUE) {
  if (!file.exists(archive_path)) {
    stop("Archive file not found: ", archive_path)
  }
  
  # Determine archive type
  is_zip <- grepl("\\.zip$", archive_path, ignore.case = TRUE)
  is_tar <- grepl("\\.tar\\.gz$|\\.tgz$", archive_path, ignore.case = TRUE)
  
  if (!is_zip && !is_tar) {
    stop("Unknown archive format. Expected .zip or .tar.gz/.tgz")
  }
  
  # List archive contents
  if (is_tar) {
    # For tar files, untar with list = TRUE returns a character vector
    file_list <- untar(archive_path, list = TRUE)
    
    # We need to extract to temp dir to get file sizes
    temp_extract <- tempfile("bids_list_")
    dir.create(temp_extract)
    on.exit(unlink(temp_extract, recursive = TRUE))
    
    untar(archive_path, exdir = temp_extract)
    
    # Get file info
    file_info <- do.call(rbind, lapply(file_list, function(f) {
      full_path <- file.path(temp_extract, f)
      if (file.exists(full_path) && !dir.exists(full_path)) {
        data.frame(
          file = f,
          size = file.size(full_path),
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    }))
    
  } else {
    # For zip files
    zip_contents <- unzip(archive_path, list = TRUE)
    file_info <- data.frame(
      file = zip_contents$Name,
      size = zip_contents$Length,
      stringsAsFactors = FALSE
    )
    # Remove directories
    file_info <- file_info[!grepl("/$", file_info$file), ]
  }
  
  if (is.null(file_info) || nrow(file_info) == 0) {
    warning("No files found in archive")
    return(data.frame(file = character(), size = numeric(), 
                     is_stub = logical(), type = character()))
  }
  
  # Add stub and type information
  file_info$is_stub <- file_info$size == 0 & grepl("\\.nii(\\.gz)?$", file_info$file)
  
  # Check for downsampled files (have res- tag and are imaging files)
  file_info$is_downsampled <- grepl("_res-", file_info$file) & 
                              grepl("\\.nii(\\.gz)?$", file_info$file) & 
                              file_info$size > 0
  
  # Determine file types
  file_info$type <- sapply(file_info$file, function(f) {
    if (grepl("\\.nii(\\.gz)?$", f)) {
      if (grepl("_res-", f) && file_info$size[file_info$file == f] > 0) {
        "imaging_downsampled"
      } else if (file_info$size[file_info$file == f] == 0) {
        "imaging_stub"
      } else {
        "imaging"
      }
    }
    else if (grepl("\\.json$", f)) "json"
    else if (grepl("\\.tsv$", f)) "tsv"
    else if (grepl("\\.txt$", f)) "text"
    else if (grepl("\\.(md|rst)$", f)) "documentation"
    else "other"
  })
  
  # Sort by file path
  file_info <- file_info[order(file_info$file), ]
  
  if (verbose) {
    # Print summary
    cat("Archive contents summary:\n")
    cat(sprintf("  Total files: %d\n", nrow(file_info)))
    cat(sprintf("  Stub imaging files: %d\n", sum(file_info$is_stub)))
    cat(sprintf("  Downsampled imaging files: %d\n", sum(file_info$is_downsampled)))
    cat(sprintf("  Metadata files (JSON): %d\n", sum(file_info$type == "json")))
    cat(sprintf("  Data files (TSV): %d\n", sum(file_info$type == "tsv")))
    cat(sprintf("  Other files: %d\n", sum(file_info$type == "other")))
    cat(sprintf("  Total size: %.2f MB\n", sum(file_info$size) / (1024^2)))
  }
  
  return(file_info)
}