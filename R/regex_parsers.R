#' Regex-based BIDS filename parsers
#' 
#' This module provides regex-based parsers for BIDS filenames as a replacement
#' for the Combin8R parser combinators. It maintains the same interface while
#' using only standard R regex functionality.

#' Extract BIDS components using simpler regex patterns
#' 
#' @param filename The filename to parse
#' @param spec The specification object
#' @return A list of extracted components or NULL
#' @keywords internal
extract_bids_components <- function(filename, spec) {
  # Remove file extension first to work with the base filename
  base_name <- tools::file_path_sans_ext(filename)
  if (grepl("\\.nii$", base_name)) {
    base_name <- tools::file_path_sans_ext(base_name)  # Remove .nii from .nii.gz
  }
  
  # Determine the kind and suffix
  kind_suffix <- extract_kind_and_suffix(filename, spec$kinds)
  if (is.null(kind_suffix)) {
    return(NULL)
  }
  
  # Remove the kind from the base name to get the entity part
  entity_part <- stringr::str_remove(base_name, paste0("_", kind_suffix$kind, "$"))
  
  # Extract BIDS entities
  entities <- extract_bids_entities(entity_part, spec$keystruc)
  if (is.null(entities)) {
    return(NULL)
  }
  
  # Combine results
  result <- c(entities, kind_suffix, list(type = spec$type))
  
  # Clean up - remove NULL values and ensure proper names
  result[!sapply(result, is.null)]
}

#' Extract kind and suffix from filename
#' 
#' @param filename The filename
#' @param kinds_spec The kinds specification
#' @return List with kind and suffix or NULL
#' @keywords internal
extract_kind_and_suffix <- function(filename, kinds_spec) {
  for (i in seq_len(nrow(kinds_spec))) {
    kind <- kinds_spec$kind[i]
    suffixes <- kinds_spec$suffix[i]
    
    if (is.list(suffixes)) {
      suffixes <- unlist(suffixes)
    }
    
    for (suffix in suffixes) {
      # Create pattern to match _kind.suffix at the end
      pattern <- sprintf("_%s\\%s$", kind, suffix)
      if (stringr::str_detect(filename, pattern)) {
        # Extract the actual suffix (remove leading dot if present)
        clean_suffix <- stringr::str_remove(suffix, "^\\.")
        return(list(kind = kind, suffix = clean_suffix))
      }
    }
  }
  NULL
}

#' Extract BIDS entities from the entity part of filename
#' 
#' @param entity_part The part of filename before the kind
#' @param keystruc_spec The key structure specification
#' @return Named list of entities or NULL
#' @keywords internal
extract_bids_entities <- function(entity_part, keystruc_spec) {
  entities <- list()
  
  # Start with the first (mandatory) key - subject
  first_key <- keystruc_spec$key[[1]]
  first_pattern <- keystruc_spec$pattern[[1]]
  first_name <- keystruc_spec$name[[1]]
  
  # Pattern for first key: key-value (no leading underscore)
  sub_pattern <- sprintf("^%s-(%s)", first_key, first_pattern)
  sub_match <- stringr::str_match(entity_part, sub_pattern)
  
  if (is.na(sub_match[1])) {
    return(NULL)  # Must have subject
  }
  
  entities[[first_name]] <- sub_match[2]
  
  # Remove the subject part to get remaining entities
  remaining <- stringr::str_remove(entity_part, sprintf("^%s-%s", first_key, sub_match[2]))
  
  # Process remaining keys (optional, with leading underscores)
  for (i in 2:nrow(keystruc_spec)) {
    key <- keystruc_spec$key[[i]]
    pattern <- keystruc_spec$pattern[[i]]
    name <- keystruc_spec$name[[i]]
    optional <- keystruc_spec$optional[[i]]
    
    if (is.null(pattern)) {
      # Literal key (like modality)
      if (is.list(key)) {
        key_alt <- paste(unlist(key), collapse = "|")
        entity_pattern <- sprintf("_((%s))(?=_|$)", key_alt)
      } else {
        entity_pattern <- sprintf("_(%s)(?=_|$)", key)
      }
    } else {
      # Key-value pair
      if (is.list(key)) {
        key_alt <- paste(unlist(key), collapse = "|")
        entity_pattern <- sprintf("_(?:%s)-(%s)(?=_|$)", key_alt, pattern)
      } else {
        entity_pattern <- sprintf("_%s-(%s)(?=_|$)", key, pattern)
      }
    }
    
    entity_match <- stringr::str_match(remaining, entity_pattern)
    
    if (!is.na(entity_match[1])) {
      if (is.null(pattern)) {
        # For literal keys, use the matched literal
        entities[[name]] <- entity_match[2]
      } else {
        # For key-value pairs, use the value part
        entities[[name]] <- entity_match[2]
      }
    } else if (!optional) {
      # Required key not found
      return(NULL)
    }
  }
  
  entities
}

#' Parse a BIDS filename using regex
#' 
#' @param filename The filename to parse
#' @param spec The specification object
#' @return A list with parsed components or NULL if no match
#' @keywords internal
parse_with_regex <- function(filename, spec) {
  result <- extract_bids_components(filename, spec)
  
  if (is.null(result)) {
    return(NULL)
  }
  
  # Return in the expected format
  list(
    result = result,
    remaining = ""
  )
}

#' Create a regex-based parser from a specification
#' 
#' @param spec A specification object
#' @return A parser function
#' @keywords internal
create_regex_parser <- function(spec) {
  function(filename) {
    parse_with_regex(filename, spec)
  }
} 