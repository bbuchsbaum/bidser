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
  # Determine the kind and suffix
  kind_suffix <- extract_kind_and_suffix(filename, spec$kinds)
  if (is.null(kind_suffix)) {
    return(NULL)
  }

  # Remove the matched suffix and kind to get the entity part. This handles
  # multi-part suffixes such as .nii.gz and .lv.h5.
  base_name <- basename(filename)
  suffix_pattern <- paste0("\\.", .bidser_regex_escape(kind_suffix$suffix), "$")
  base_name <- sub(suffix_pattern, "", base_name, perl = TRUE)

  kind_pattern <- paste0("_", .bidser_regex_escape(kind_suffix$kind), "$")
  if (!grepl(kind_pattern, base_name, perl = TRUE)) {
    return(NULL)
  }
  entity_part <- sub(kind_pattern, "", base_name, perl = TRUE)
  
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

#' @keywords internal
#' @noRd
.bidser_regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x, perl = TRUE)
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
      tail <- paste0("_", kind, suffix)
      if (endsWith(filename, tail)) {
        # Extract the actual suffix (remove leading dot if present)
        clean_suffix <- sub("^\\.", "", suffix)
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

  if (!is.character(entity_part) || length(entity_part) != 1L || !nzchar(entity_part)) {
    return(NULL)
  }

  tokens <- strsplit(entity_part, "_", fixed = TRUE)[[1]]
  if (length(tokens) == 0L || any(!nzchar(tokens))) {
    return(NULL)
  }

  key_values <- function(x) {
    if (is.list(x)) {
      unlist(x, use.names = FALSE)
    } else {
      as.character(x)
    }
  }

  used_names <- character(0)
  last_order <- -Inf

  for (token in tokens) {
    is_key_value <- grepl("-", token, fixed = TRUE)

    if (is_key_value) {
      key <- sub("-.*$", "", token)
      value <- sub("^[^-]+-", "", token)
      if (!nzchar(key) || !nzchar(value)) {
        return(NULL)
      }

      candidates <- which(vapply(seq_len(nrow(keystruc_spec)), function(i) {
        pattern <- keystruc_spec$pattern[[i]]
        if (is.null(pattern) || !key %in% key_values(keystruc_spec$key[[i]])) {
          return(FALSE)
        }
        grepl(paste0("^(", pattern, ")$"), value, perl = TRUE)
      }, logical(1)))
    } else {
      value <- token
      candidates <- which(vapply(seq_len(nrow(keystruc_spec)), function(i) {
        pattern <- keystruc_spec$pattern[[i]]
        is.null(pattern) && value %in% key_values(keystruc_spec$key[[i]])
      }, logical(1)))
    }

    if (length(candidates) == 0L) {
      return(NULL)
    }

    candidates <- candidates[vapply(candidates, function(i) {
      order <- keystruc_spec$order[[i]]
      name <- keystruc_spec$name[[i]]
      order >= last_order && !name %in% used_names
    }, logical(1))]

    if (length(candidates) == 0L) {
      return(NULL)
    }

    candidates <- candidates[order(
      vapply(candidates, function(i) keystruc_spec$order[[i]], numeric(1)),
      candidates
    )]
    i <- candidates[[1]]
    name <- keystruc_spec$name[[i]]

    entities[[name]] <- value
    used_names <- c(used_names, name)
    last_order <- keystruc_spec$order[[i]]
  }

  required <- keystruc_spec$name[!as.logical(keystruc_spec$optional)]
  if (any(!required %in% names(entities))) {
    return(NULL)
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
