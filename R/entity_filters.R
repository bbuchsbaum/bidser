#' Split filter arguments into string filters and formula filters
#'
#' @param dots A named/unnamed list (from `list(...)`)
#' @return A list with `string_filters` (named list) and `formula_filters` (list of formulas)
#' @keywords internal
#' @noRd
.bidser_split_filters <- function(dots) {
  string_filters <- list()
  formula_filters <- list()

  for (i in seq_along(dots)) {
    el <- dots[[i]]
    if (inherits(el, "formula")) {
      formula_filters[[length(formula_filters) + 1L]] <- el
    } else {
      nm <- names(dots)[[i]]
      if (is.null(nm) || !nzchar(nm)) {
        stop("Non-formula filter arguments must be named (e.g. task = 'rest').")
      }
      string_filters[[nm]] <- el
    }
  }

  list(string_filters = string_filters, formula_filters = formula_filters)
}


#' Evaluate a single formula filter against a data.tree node
#'
#' @param node A data.tree Node object
#' @param fml A formula whose LHS is a single entity name symbol and RHS is an
#'   expression to evaluate in a child environment where the entity value is
#'   bound as a character variable.
#' @param envir The parent environment for formula evaluation (captures caller
#'   variables like closures).
#' @return Logical scalar; `FALSE` if the entity is missing or the formula
#'   errors.
#' @keywords internal
#' @noRd
.bidser_eval_entity_formula <- function(node, fml, envir) {
  tryCatch({
    lhs <- fml[[2]]
    if (!is.symbol(lhs)) {
      stop("Formula LHS must be a single entity name symbol, e.g. `run ~ as.integer(run) >= 2`.")
    }
    entity_name <- as.character(lhs)

    entity_val <- node[[entity_name]]
    if (is.null(entity_val) || (length(entity_val) == 1L && is.na(entity_val))) {
      return(FALSE)
    }

    # Bind entity value as character in a child environment
    eval_env <- new.env(parent = envir)
    assign(entity_name, as.character(entity_val), envir = eval_env)

    result <- eval(fml[[3]], envir = eval_env)
    isTRUE(result)
  }, error = function(e) {
    if (grepl("Formula LHS", conditionMessage(e))) {
      stop(conditionMessage(e))
    }
    FALSE
  })
}


#' Build a node matcher function from a list of formula filters
#'
#' @param formula_list A list of formulas (may be empty).
#' @param envir The environment in which to evaluate formula RHS expressions.
#' @return A `function(node)` returning `TRUE` iff all formulas evaluate `TRUE`.
#' @keywords internal
#' @noRd
.bidser_formula_matcher <- function(formula_list, envir) {
  if (length(formula_list) == 0L) {
    return(function(node) TRUE)
  }

  # Capture formula_list and envir in the closure
  force(formula_list)
  force(envir)

  function(node) {
    for (fml in formula_list) {
      lhs <- fml[[2]]
      if (!is.symbol(lhs)) {
        stop("Formula LHS must be a single entity name symbol, e.g. `run ~ as.integer(run) >= 2`.")
      }
      if (!isTRUE(.bidser_eval_entity_formula(node, fml, envir))) {
        return(FALSE)
      }
    }
    TRUE
  }
}
