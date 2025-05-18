## convenience methods for matching different patterns

#' Extract `type` and `suffix` fields from a parsed value
#'
#' @param x A parsed value (likely from a parser combinator)
#' @keywords internal
extractor <- function(x) {
  list(type = x[[1]][[1]], suffix = x[[2]][[1]])
}

#' Alternate extractor that expects a slightly different structure
#'
#' @param x A parsed value (likely from a parser combinator)
#' @keywords internal
alt_extractor <- function(x) {
  list(type = x[[1]][[1]], suffix = x[[2]]$value)
}


#' Create a parser for an optional BIDS key
#'
#' @param label The label of the key to parse.
#' @param regex A regex pattern that the key's value should match. Must be a
#'   single character string.
#' @return A parser that matches zero or more occurrences of `_<label>-<id>`
#' @keywords internal
optional_key <- function(label, regex = "[A-Za-z0-9]+") {
  if (!is.character(label) || length(label) != 1) {
    stop("`label` must be a single character string.")
  }
  if (!is.character(regex) || length(regex) != 1) {
    stop("`regex` must be a single character string.")
  }
  pMany(
    paste0("has_", label),
    pSeq(
      function(value) { value[[4]]$value },
      pLiteral("_"), pLiteral(label), pLiteral("-"), pRegex("id", regex)
    )
  )
}

#' Create a parser for an optional BIDS literal
#'
#' This matches zero or one occurrence of `_<lit>`
#'
#' @param lit A literal to match (a fixed string).
#' @param label A label for the parser.
#' @return A parser that matches zero or one occurrence of `_<lit>`
#' @keywords internal
optional_literal <- function(lit, label) {
  if (!is.character(lit) || length(lit) != 1) {
    stop("`lit` must be a single character string.")
  }
  if (!is.character(label) || length(label) != 1) {
    stop("`label` must be a single character string.")
  }
  pMany(
    paste0("has_", label),
    pSeq(
      function(value) { value[[2]][[1]][[1]] },
      pLiteral("_"), pLiteral(lit)
    )
  )
}

#' Create a parser for a mandatory BIDS key
#'
#' Matches a pattern `_<label>-<id>` where `id` matches a given regex.
#'
#' @param label The label of the key to parse.
#' @param regex The regex for the key's value. Must be a single character string.
#' @return A parser that must match one occurrence of `_<label>-<id>`
#' @keywords internal
mandatory_key <- function(label, regex = "[A-Za-z0-9]+") {
  if (!is.character(label) || length(label) != 1) {
    stop("`label` must be a single character string.")
  }
  if (!is.character(regex) || length(regex) != 1) {
    stop("`regex` must be a single character string.")
  }
  pSeq(
    function(value) { value[[4]]$value },
    pLiteral("_"), pLiteral(label), pLiteral("-"), pRegex("id", regex)
  )
}

#' Create a parser for a starting BIDS key (no leading underscore)
#'
#' Matches `<label>-<id>` at the start of a filename.
#'
#' @param label The label of the key to parse.
#' @param regex The regex for the key's value. Must be a single character string.
#' @return A parser that matches `<label>-<id>` at the start
start_key <- function(label, regex = "[A-Za-z0-9]+") {
  if (!is.character(label) || length(label) != 1) {
    stop("`label` must be a single character string.")
  }
  if (!is.character(regex) || length(regex) != 1) {
    stop("`regex` must be a single character string.")
  }
  pSeq(
    function(value) { value[[3]]$value },
    pLiteral(label), pLiteral("-"), pRegex("id", regex)
  )
}

#' Match one of several possible labels
#'
#' Given a vector of labels, matches `_<label>` where <label> is one of them.
#'
#' @param labels A character vector of possible labels.
#' @return A parser that matches `_<one_of_labels>`
#' @keywords internal
one_of <- function(labels) {
  if (!is.character(labels) || length(labels) == 0) {
    stop("`labels` must be a non-empty character vector.")
  }
  
  literals <- lapply(labels, pLiteral)
  pSeq(
    function(value) { value[[2]][[1]] },
    pLiteral("_"),
    do.call(pAlt, c(literals, tag = function(x) { x }))
  )
}

#' Match zero or one of several labels
#'
#' Similar to `one_of` but matches zero or one occurrence.
#'
#' @param labels A character vector of possible labels.
#' @param label A label for the parser.
#' @return A parser that matches zero or one of `_<labels>`
#' @keywords internal
zero_or_one_of <- function(labels, label) {
  if (!is.character(labels) || length(labels) == 0) {
    stop("`labels` must be a non-empty character vector.")
  }
  if (!is.character(label) || length(label) != 1) {
    stop("`label` must be a single character string.")
  }
  
  literals <- lapply(labels, pLiteral)
  pMany(
    paste0("has_", label),
    pSeq(
      function(value) { value[[2]][[1]] },
      pLiteral("_"),
      do.call(pAlt, c(literals, tag = function(x) x))
    )
  )
}

#' Create a parser that matches a fixed pattern of type followed by suffix
#'
#' This function creates a parser that matches a fixed pattern of type followed by suffix,
#' used internally to match specific BIDS file patterns. The parser uses a provided extractor
#' function to extract matched fields from the filename.
#'
#' @param type A literal for the type (e.g., "bold", "T1w").
#' @param suffix A single-character string for the suffix (e.g., ".nii.gz", ".tsv").
#' @param extractor A function used to extract matched fields from the filename.
#'
#' @return A parser function that matches `type` followed by `suffix`. The parser returns:
#'   - A list with the matched components if successful
#'   - NULL if the pattern doesn't match
#'
#' @examples
#' # Create a parser for BOLD files
#' bold_parser <- gen_lit("bold", ".nii.gz", function(x) list(type="bold"))
#' bold_parser("sub-01_task-rest_bold.nii.gz")  # Returns list(type="bold")
#' bold_parser("sub-01_T1w.nii.gz")  # Returns NULL (no match)
#'
#' # Create a parser for T1w files
#' t1w_parser <- gen_lit("T1w", ".nii.gz", function(x) list(type="T1w"))
#' t1w_parser("sub-01_T1w.nii.gz")  # Returns list(type="T1w")
#'
#' # Create a parser for event files
#' event_parser <- gen_lit("events", ".tsv", function(x) list(type="events"))
#' event_parser("sub-01_task-rest_events.tsv")  # Returns list(type="events")
#'
#' @keywords internal
gen_lit <- function(type, suffix, extractor) {
  if (!is.character(type) || length(type) != 1) {
    stop("`type` must be a single character string.")
  }
  if (!is.character(suffix) || length(suffix) != 1) {
    stop("`suffix` must be a single character string.")
  }
  if (!is.function(extractor)) {
    stop("`extractor` must be a function.")
  }

  pSeq(extractor, pLiteral(type), pLiteral(suffix))
}

#' Multiple literal matchers
#'
#' Given multiple `types` and a single `suffix`, generates a list of parsers.
#'
#' @param types A character vector of types.
#' @param suffix A single-character string shared by all types.
#' @param extractor A function used to extract matched fields from each filename.
#' @return A list of parsers.
#' @keywords internal
gen_lits <- function(types, suffix, extractor) {
  if (!is.character(types) || length(types) == 0) {
    stop("`types` must be a non-empty character vector.")
  }
  if (!is.character(suffix) || length(suffix) != 1) {
    stop("`suffix` must be a single character string.")
  }
  if (!is.function(extractor)) {
    stop("`extractor` must be a function.")
  }
  lapply(types, function(t) gen_lit(t, suffix, extractor))
}
