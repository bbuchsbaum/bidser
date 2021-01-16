## convenience methods for matching different patterns

#' pull out the `type` and `suffix`
#' 
#' @keywords internal
extractor <- function(x) {list(type=x[[1]][[1]], suffix=x[[2]][[1]]) }


#' @keywords internal
alt_extractor <- function(x) { list(type=x[[1]][[1]], suffix=x[[2]]$value) }


#' create parser for optional BIDS key
optional_key <- function(label, regex="[A-Za-z0-9]+") {
  pMany(paste0("has_", label),
        pSeq(function(value) { value[[4]]$value}, pLiteral("_"), pLiteral(label), pLiteral("-"), pRegex("id", regex)))
}

#' create parser for an optional BIDS literal
optional_literal <- function(lit, label) {
  pMany(paste0("has_", label),
        pSeq(function(value) { value[[2]][[1]][[1]] }, pLiteral("_"), pLiteral(lit))
  )
}

#' create parser for a mandatory BIDS key
#' @keywords internal
mandatory_key <- function(label, regex="[A-Za-z0-9]+") {
  pSeq(function(value) { value[[4]]$value}, pLiteral("_"), pLiteral(label), pLiteral("-"), pRegex("id", regex))
}

start_key <- function(label, regex="[A-Za-z0-9]+") {
  pSeq(function(value) { value[[3]]$value}, pLiteral(label), pLiteral("-"), pRegex("id", regex))
}

#' match one of several labels
#' @keywords internal
one_of <- function(labels) {
  lits <- lapply(labels, pLiteral)
  pSeq(function(value) { value[[2]][[1]] }, pLiteral("_"), do.call(pAlt, c(lits, tag=function(x) { x})))
}

#' match zero or one of several labels
#' @keywords internal
zero_or_one_of <- function(labels, label) {
  lits <- lapply(labels, pLiteral)
  pMany(paste0("has_", label),
        pSeq(function(value) { value[[2]][[1]] }, pLiteral("_"), do.call(pAlt, c(lits, tag=function(x) { x})))
  )
}

#' a literal matcher
gen_lit <- function(type, suffix, extractor) {
  pSeq(extractor, pLiteral(type), pLiteral(suffix))
}

#' multiple literal matchers
#' @keywords internal
gen_lits <- function(types, suffix, extractor) {
  lapply(types, function(t) gen_lit(t,suffix,extractor))
  
}

