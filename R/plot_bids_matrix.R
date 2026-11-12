# Acquisition-matrix and structure-tree figures for BIDS projects.
#
# Both figures are built from one normalised file table (bids_plot_frame()),
# so that colours, labels and ordering agree everywhere.

# ---------------------------------------------------------------------------
# File table
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
bids_file_extension <- function(name) {
  ext <- stringr::str_match(name, "(\\.[A-Za-z0-9]+(\\.gz)?)$")[, 2]
  ext[is.na(ext)] <- ""
  tolower(ext)
}

#' @keywords internal
#' @noRd
bids_primary_extensions <- function() {
  c(".nii", ".nii.gz", ".edf", ".bdf", ".vhdr", ".set", ".fif", ".snirf",
    ".mefd", ".nwb", ".mgz", ".gii", ".dtseries.nii", ".h5", ".tif",
    ".ome.tif", ".png", ".con", ".sqd", ".ds", ".mat")
}

#' Normalised file table used by the bidser figures
#'
#' @param x A `bids_project` or `mock_bids_project`.
#' @param include_derivatives Include files under `derivatives/`.
#' @return A tibble with one row per file and columns `path`, `file`,
#'   `subject`, `session`, `datatype`, `kind`, entity columns, `extension`,
#'   `scope`, `pipeline`, `size`, and `role` (`"primary"`, `"companion"`,
#'   `"sidecar"` or `"other"`).
#' @keywords internal
#' @noRd
bids_plot_frame <- function(x, include_derivatives = TRUE) {
  scope <- if (isTRUE(include_derivatives)) "all" else "raw"
  tb <- tryCatch(
    suppressWarnings(query_files(x, regex = ".*", return = "tibble", scope = scope)),
    error = function(e) NULL
  )
  if (is.null(tb) || !is.data.frame(tb) || nrow(tb) == 0) {
    return(tibble::tibble())
  }
  tb <- tibble::as_tibble(tb)
  col <- function(nm) if (nm %in% names(tb)) as.character(tb[[nm]]) else rep(NA_character_, nrow(tb))

  file <- basename(col("path"))
  ext <- bids_file_extension(file)
  kind <- col("kind")
  # Fall back to the trailing _<suffix> of the file name
  tail_suffix <- stringr::str_match(file, "_([A-Za-z0-9]+)\\.[^_]*$")[, 2]
  kind[is.na(kind) | kind == ""] <- tail_suffix[is.na(kind) | kind == ""]

  out <- tibble::tibble(
    path = col("path"),
    file = file,
    subject = col("subid"),
    session = col("session"),
    datatype = col("datatype"),
    kind = kind,
    task = col("task"),
    acq = col("acq"),
    ce = col("ce"),
    rec = col("rec"),
    dir = col("dir"),
    echo = col("echo"),
    run = col("run"),
    space = col("space"),
    desc = col("desc"),
    extension = ext,
    scope = col("scope"),
    pipeline = col("pipeline"),
    size = if ("size" %in% names(tb)) suppressWarnings(as.numeric(tb$size)) else NA_real_
  )
  out$scope[is.na(out$scope)] <- "raw"
  # Volume counts for 4-D series, from the NIfTI header (cheap, base R only)
  out$nvol <- NA_integer_
  root <- x$path %||% x$root %||% NULL
  is4d <- out$extension %in% c(".nii", ".nii.gz") &
    (out$kind %in% c("bold", "cbv", "phase", "asl", "dwi", "sbref") |
       out$datatype %in% c("func", "dwi", "perf"))
  if (!is.null(root) && is.character(root) && dir.exists(root) && any(is4d)) {
    full <- ifelse(startsWith(out$path[is4d], "/"), out$path[is4d], file.path(root, out$path[is4d]))
    out$nvol[is4d] <- nifti_nvols(full)
  }
  out$session[out$session == ""] <- NA_character_

  # Infer datatype from the folder when the parser did not supply one
  folder <- basename(dirname(out$path))
  miss_dt <- is.na(out$datatype) | out$datatype == ""
  out$datatype[miss_dt & folder %in% names(bids_datatype_colors())] <-
    folder[miss_dt & folder %in% names(bids_datatype_colors())]

  is_companion <- out$kind %in% c("events", "physio", "stim", "timeseries", "regressors") &
    out$extension %in% c(".tsv", ".tsv.gz")
  is_beh <- out$datatype %in% "beh" & out$kind %in% "beh"
  out$role <- dplyr::case_when(
    out$extension == ".json" ~ "sidecar",
    is_companion ~ "companion",
    out$extension %in% bids_primary_extensions() | is_beh ~ "primary",
    TRUE ~ "other"
  )
  out
}

#' Number of volumes (NIfTI dim[5]) read from the header alone
#'
#' Reads the first 348 bytes of a `.nii` or `.nii.gz` file with base R and
#' returns `dim[5]` for 4-D images (1 for 3-D images), or `NA` when the file
#' is empty or not a NIfTI-1 file. No imaging package is needed.
#' @keywords internal
#' @noRd
nifti_nvols <- function(paths) {
  vapply(paths, function(f) {
    if (is.na(f) || !file.exists(f) || file.size(f) == 0) return(NA_integer_)
    hdr <- tryCatch({
      con <- gzfile(f, "rb")
      on.exit(close(con), add = TRUE)
      readBin(con, "raw", 348)
    }, error = function(e) raw())
    if (length(hdr) < 348) return(NA_integer_)
    endian <- NULL
    for (en in c("little", "big")) {
      if (readBin(hdr[1:4], "integer", size = 4, endian = en) == 348L) endian <- en
    }
    if (is.null(endian)) return(NA_integer_)
    dims <- readBin(hdr[41:56], "integer", size = 2, n = 8, endian = endian)
    if (dims[1] < 1 || dims[1] > 7) return(NA_integer_)
    if (dims[1] >= 4) as.integer(dims[5]) else 1L
  }, integer(1), USE.NAMES = FALSE)
}

#' @keywords internal
#' @noRd
natural_order <- function(x) {
  x <- unique(x[!is.na(x)])
  num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", x)))
  if (all(!is.na(num))) x[order(num, x)] else sort(x)
}

#' Order session labels: numeric when possible, else common temporal words
#' (pre/baseline before post/follow-up), else alphabetical.
#' @keywords internal
#' @noRd
session_order <- function(x) {
  x <- unique(as.character(x[!is.na(x)]))
  bare <- tolower(sub("^ses-", "", x))
  num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", bare)))
  if (all(!is.na(num))) return(x[order(num, x)])
  words <- c("baseline", "base", "bl", "pre", "screen", "1", "first", "mid",
             "post", "followup", "fu", "follow", "retest", "second", "last")
  rank <- vapply(bare, function(b) {
    hit <- which(vapply(words, function(w) startsWith(b, w), logical(1)))
    if (length(hit)) hit[1] else length(words) + 1
  }, numeric(1))
  x[order(rank, x)]
}

#' @keywords internal
#' @noRd
pretty_run <- function(run) {
  n <- suppressWarnings(as.integer(run))
  ifelse(is.na(n), run, as.character(n))
}

# ---------------------------------------------------------------------------
# Acquisition matrix
# ---------------------------------------------------------------------------

#' Build the cell table for the acquisition matrix
#' @keywords internal
#' @noRd
bids_matrix_data <- function(frame, datatypes = NULL) {
  prim <- frame[frame$role == "primary" & !is.na(frame$subject) & frame$subject != "", ]
  if (!is.null(datatypes)) prim <- prim[prim$datatype %in% datatypes, ]
  if (nrow(prim) == 0) return(NULL)
  prim$datatype[is.na(prim$datatype) | prim$datatype == ""] <- "other"
  prim$session_lab <- ifelse(is.na(prim$session), "", paste0("ses-", prim$session))

  # Group (one facet strip): datatype for raw files, pipeline for derivatives,
  # split by task so that the columns underneath are just run-1, run-2, ...
  # The strip is two lines: datatype/pipeline over task-<label>.
  pipe <- ifelse(is.na(prim$pipeline) | prim$pipeline == "", "derivatives", prim$pipeline)
  base_group <- ifelse(prim$scope == "derivatives", pipe, prim$datatype)
  prim$group <- ifelse(is.na(prim$task), base_group,
                       paste0(base_group, "\n", "task-", prim$task))

  # Column label in BIDS entity syntax. Entities that are constant within a
  # group (one space, one phase-encoding direction) are left out.
  ent <- function(v, prefix) ifelse(is.na(v) | v == "", "", paste0(prefix, v))
  mod_cols <- c("acq", "ce", "rec", "dir", "echo", "space", "desc")
  mods <- lapply(mod_cols, function(nm) {
    v <- prim[[nm]]
    varies <- tapply(v, prim$group, function(z) length(unique(z)) > 1)
    ifelse(varies[prim$group], ent(v, paste0(nm, "-")), "")
  })
  modifiers <- do.call(paste, c(mods, sep = "_"))
  run_lab <- ent(prim$run, "run-")
  kinds_per_group <- tapply(prim$kind, prim$group, function(k) length(unique(k)))
  show_kind <- kinds_per_group[prim$group] > 1 | (run_lab == "" & gsub("_", "", modifiers) == "")
  col <- paste(modifiers, run_lab, ifelse(show_kind, prim$kind, ""), sep = "_")
  col <- gsub("_+", "_", col)
  col <- gsub("^_|_$", "", col)
  col[col == ""] <- prim$kind[col == ""]
  prim$column <- col

  # A file-name template for each column, so that problems can be reported
  # as copyable BIDS names (sub-07_task-x_run-03_bold).
  stem <- sub("\\.[^_]*$", "", prim$file)
  stem <- sub("^sub-[A-Za-z0-9]+_?", "", stem)
  stem <- sub("^ses-[A-Za-z0-9]+_?", "", stem)
  prim$stem <- stem

  # Companion files (events.tsv beside raw bold, confounds beside derivatives)
  comp <- frame[frame$role == "companion" & !is.na(frame$subject), ]
  key <- function(d) paste(d$scope, d$pipeline, d$subject, d$session, d$task, d$acq, d$run, sep = "|")
  prim$has_companion <- key(prim) %in% key(comp)
  comp_kind <- tapply(comp$kind, key(comp), function(k) k[1])
  group_has_comp <- tapply(prim$has_companion, prim$group, any)
  prim$expects_companion <- as.logical(group_has_comp[prim$group])
  prim$companion_kind <- as.character(unname(comp_kind[key(prim)]))

  # Raw counterpart key: a derivative is only expected where raw data exist
  rawkey <- function(d) paste(d$subject, d$session, d$datatype, d$task, d$acq, d$run, sep = "|")
  raw_keys <- rawkey(prim[prim$scope == "raw", ])

  cells <- prim %>%
    dplyr::group_by(.data$session_lab, .data$group, .data$column, .data$subject,
                    .data$datatype, .data$scope) %>%
    dplyr::summarise(
      n_files = dplyr::n(),
      size = if (all(is.na(.data$size))) NA_real_ else sum(.data$size, na.rm = TRUE),
      nvol = .data$nvol[1],
      has_companion = any(.data$has_companion),
      expects_companion = any(.data$expects_companion),
      file = .data$file[1],
      .groups = "drop"
    )

  grp <- prim %>%
    dplyr::distinct(.data$group, .data$datatype, .data$scope, .data$task) %>%
    dplyr::mutate(rank = match(.data$datatype, bids_datatype_order(prim$datatype)) +
                    100 * (.data$scope == "derivatives")) %>%
    dplyr::arrange(.data$rank, .data$group)
  group_levels <- unique(grp$group)

  col_tab <- prim %>%
    dplyr::group_by(.data$group, .data$column) %>%
    dplyr::summarise(kind = .data$kind[1], task = .data$task[1], run = .data$run[1],
                     acq = .data$acq[1], stem = .data$stem[1],
                     companion_kind = dplyr::first(stats::na.omit(.data$companion_kind)) %||% NA_character_,
                     .groups = "drop") %>%
    dplyr::mutate(runn = suppressWarnings(as.numeric(.data$run)),
                  g = match(.data$group, group_levels)) %>%
    dplyr::arrange(.data$g, .data$kind, .data$runn, .data$column)
  col_levels <- unique(paste(col_tab$group, col_tab$column, sep = "\u001f"))

  subjects <- natural_order(prim$subject)
  sessions <- session_order(prim$session_lab)
  if (length(sessions) == 0) sessions <- ""

  # Complete grid: every subject x every acquisition seen in that session
  acq_keys <- cells %>% dplyr::distinct(.data$session_lab, .data$group, .data$column,
                                        .data$datatype, .data$scope)
  grid <- tidyr::crossing(acq_keys, subject = subjects)
  cells <- dplyr::left_join(grid, cells,
                            by = c("session_lab", "group", "column", "datatype", "scope", "subject"))
  cells <- dplyr::left_join(cells, col_tab[, c("group", "column", "task", "run", "acq", "stem", "companion_kind")],
                            by = c("group", "column"))
  cells$present <- !is.na(cells$n_files)
  cells$n_files[is.na(cells$n_files)] <- 0L
  cells$has_companion[is.na(cells$has_companion)] <- FALSE
  cells$expects_companion <- stats::ave(
    dplyr::coalesce(cells$expects_companion, FALSE),
    cells$session_lab, cells$group, FUN = any
  ) > 0
  cells$session <- sub("^ses-", "", cells$session_lab)
  cells$session[cells$session == ""] <- NA_character_
  cells$raw_present <- cells$scope == "raw" | rawkey(cells) %in% raw_keys

  # Size outliers: a present file far from its column's median (truncation,
  # wrong protocol, duplicated volumes). Only when sizes are real.
  cells <- cells %>%
    dplyr::group_by(.data$session_lab, .data$group, .data$column) %>%
    dplyr::mutate(
      col_median = if (sum(!is.na(.data$size) & .data$present) >= 3)
        stats::median(.data$size[.data$present], na.rm = TRUE) else NA_real_,
      size_ratio = .data$size / .data$col_median,
      size_flag = .data$present & !is.na(.data$size_ratio) & .data$col_median > 0 &
        (.data$size_ratio < 0.6 | .data$size_ratio > 1.4),
      # Volume count: the most common count in the column is the expectation
      col_nvol = if (sum(!is.na(.data$nvol)) >= 3)
        as.integer(names(which.max(table(.data$nvol)))) else NA_integer_,
      vol_flag = .data$present & !is.na(.data$nvol) & !is.na(.data$col_nvol) &
        .data$nvol != .data$col_nvol
    ) %>%
    dplyr::ungroup()
  # When volume counts are known they decide; bytes are only a fallback
  has_vols <- !is.na(cells$col_nvol)
  cells$size_flag <- ifelse(has_vols, cells$vol_flag, cells$size_flag)

  # A subject-session with no files at all is one fact (reported once), not
  # a dozen separate missing files.
  absent_ss <- cells %>%
    dplyr::group_by(.data$session_lab, .data$subject) %>%
    dplyr::summarise(none = !any(.data$present), n_cells = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(.data$none, .data$n_cells > 1)
  cells$absent <- paste(cells$session_lab, cells$subject) %in%
    paste(absent_ss$session_lab, absent_ss$subject)
  cells$state <- dplyr::case_when(
    cells$absent ~ "absent",
    !cells$present & !cells$raw_present ~ "upstream",
    !cells$present ~ "missing",
    cells$size_flag ~ "size",
    TRUE ~ "present"
  )
  cells$companion_missing <- cells$present & cells$expects_companion & !cells$has_companion
  cells$bids_name <- paste0(
    "sub-", cells$subject,
    ifelse(is.na(cells$session), "", paste0("_ses-", cells$session)),
    ifelse(is.na(cells$stem) | cells$stem == "", "", paste0("_", cells$stem))
  )
  cells$bids_name <- ifelse(cells$scope == "derivatives",
                            paste0(sub("\n.*", "", cells$group), ": ", cells$bids_name),
                            cells$bids_name)

  cells$subject <- factor(cells$subject, levels = rev(subjects))
  cells$session_lab <- factor(cells$session_lab, levels = sessions)
  cells$group <- factor(cells$group, levels = group_levels)
  cells$colkey <- factor(paste(cells$group, cells$column, sep = "\u001f"), levels = col_levels)
  cells
}

#' @keywords internal
#' @noRd
bids_matrix_facets <- function(cells, rows = TRUE, cols = TRUE) {
  multi_session <- any(levels(cells$session_lab) != "")
  row_f <- if (rows && multi_session) "session_lab" else "."
  col_f <- if (cols) "group" else "."
  ggplot2::facet_grid(stats::as.formula(paste(row_f, "~", col_f)),
                      scales = "free", space = "free", switch = "y")
}

#' @keywords internal
#' @noRd
strip_colkey <- function(x) sub("^.*\u001f", "", x)

#' Short and long descriptions of an unusual file
#' @keywords internal
#' @noRd
flag_short <- function(cells) {
  ifelse(!is.na(cells$col_nvol) & !is.na(cells$nvol),
         paste0(cells$nvol, "/", cells$col_nvol),
         paste0(round(100 * cells$size_ratio), "%"))
}
#' @keywords internal
#' @noRd
flag_long <- function(cells) {
  ifelse(!is.na(cells$col_nvol) & !is.na(cells$nvol),
         paste0(cells$nvol, " of ", cells$col_nvol, " volumes",
                ifelse(is.na(cells$size), "", paste0(" (", format_bytes(cells$size), ")"))),
         paste0(format_bytes(cells$size), ", ", round(100 * cells$size_ratio), "% of column median"))
}

#' Lighten a colour towards white
#' @keywords internal
#' @noRd
tint <- function(col, amount = 0.72) {
  m <- grDevices::col2rgb(col) / 255
  m <- m + (1 - m) * amount
  grDevices::rgb(m[1, ], m[2, ], m[3, ])
}

#' The acquisition matrix (subjects x acquisitions)
#'
#' Normal cells are drawn quietly (a pale tint of the datatype colour; an
#' outline for derivatives) so that the ink goes to problems: missing files
#' (heavy frame with a cross), files of unusual size (heavy frame with the
#' percentage of the column median), and missing events/confounds files (a
#' small ring).
#'
#' @param cells Output of `bids_matrix_data()`.
#' @param fill_by `"datatype"` (default) or `"size"` (deviation from the column
#'   median).
#' @return A ggplot object.
#' @keywords internal
#' @noRd
bids_matrix_plot <- function(cells, fill_by = c("datatype", "size"),
                             color_scheme = "viridis", show_companions = TRUE,
                             text_size = 0, rotate_labels = NULL,
                             show_strips = TRUE) {
  fill_by <- match.arg(fill_by)
  ink <- bidser_ink()
  col_labels <- strip_colkey(levels(droplevels(cells$colkey)))
  if (is.null(rotate_labels)) {
    rotate_labels <- length(col_labels) > 16 || max(nchar(col_labels)) > 10
  }

  cells$tooltip <- paste0(
    cells$bids_name, "\n",
    dplyr::case_when(
      cells$state == "missing" ~ "missing",
      cells$state == "upstream" ~ "not expected: raw data missing",
      TRUE ~ paste0(ifelse(is.na(cells$size), "", format_bytes(cells$size)),
                    ifelse(!is.na(cells$nvol), paste0(", ", cells$nvol, " volumes"), ""),
                    ifelse(cells$state == "size", paste0("\nunusual: ", flag_long(cells)), ""))
    )
  )
  present <- cells[cells$present, ]
  missing <- cells[cells$state == "missing", ]
  upstream <- cells[cells$state %in% c("upstream", "absent"), ]
  flagged <- cells[cells$state == "size", ]
  pal <- bids_datatype_colors(unique(cells$datatype))

  p <- ggplot2::ggplot(cells, ggplot2::aes(x = .data$colkey, y = .data$subject)) +
    # Train the discrete scales on the full grid first so that row and column
    # order follow the factor levels, not the order layers happen to be drawn.
    ggplot2::geom_blank()

  if (nrow(upstream) > 0) {
    p <- p + ggplot2::geom_tile(data = upstream, width = 0.8, height = 0.8,
                                fill = NA, colour = ink[["rule"]], linewidth = 0.3,
                                linetype = "22")
  }

  if (fill_by == "datatype") {
    raw_p <- present[present$scope == "raw", ]
    der_p <- present[present$scope == "derivatives", ]
    p <- p +
      ggplot2::geom_tile(data = raw_p, ggplot2::aes(fill = .data$datatype),
                         width = 0.88, height = 0.88, colour = NA) +
      ggplot2::scale_fill_manual(values = stats::setNames(tint(pal), names(pal)), guide = "none")
    if (nrow(der_p) > 0) {
      p <- p + ggplot2::geom_tile(data = der_p, ggplot2::aes(colour = .data$datatype),
                                  width = 0.8, height = 0.8, fill = "white", linewidth = 0.3) +
        ggplot2::scale_colour_manual(values = stats::setNames(tint(pal, 0.55), names(pal)), guide = "none")
    }
  } else {
    lo <- 0.4; hi <- 1.6
    present$dev <- log2(pmax(pmin(present$size_ratio, hi), lo))
    p <- p +
      ggplot2::geom_tile(data = present, ggplot2::aes(fill = .data$dev),
                         width = 0.88, height = 0.88, colour = NA) +
      ggplot2::scale_fill_gradient2(
        low = "#B5562F", mid = ink[["empty"]], high = "#2F5D7C", midpoint = 0,
        limits = log2(c(lo, hi)), breaks = log2(c(0.4, 0.7, 1, 1.3, 1.6)),
        labels = c("\u2264 40%", "70%", "median", "130%", "\u2265 160%"),
        na.value = ink[["empty"]],
        name = "Size as % of\ncolumn median",
        guide = ggplot2::guide_colourbar(barheight = ggplot2::unit(70, "pt"),
                                         barwidth = ggplot2::unit(6, "pt"))
      ) +
      ggplot2::theme(legend.title = ggplot2::element_text(margin = ggplot2::margin(b = 10)))
    if (text_size > 0) {
      lab <- present[!is.na(present$size_ratio) & abs(present$size_ratio - 1) > 0.1, ]
      if (nrow(lab) > 0) {
        p <- p + ggplot2::geom_text(
          data = lab, ggplot2::aes(label = paste0(format_bytes(.data$size), " \u00b7 ",
                                                  round(100 * .data$size_ratio), "% of median")),
          size = text_size, colour = ifelse(abs(lab$size_ratio - 1) > 0.45, "white", ink[["text"]])
        )
      }
    }
  }

  if (nrow(missing) > 0) {
    p <- p +
      ggplot2::geom_tile(data = missing, width = 0.8, height = 0.8,
                         fill = "white", colour = ink[["text"]], linewidth = 0.7) +
      ggplot2::geom_point(data = missing, shape = 4, size = 1.6, stroke = 0.6,
                          colour = ink[["text"]])
  }

  if (nrow(flagged) > 0) {
    p <- p +
      ggplot2::geom_tile(data = flagged, width = 0.8, height = 0.8,
                         fill = NA, colour = ink[["text"]], linewidth = 0.7)
    if (fill_by == "datatype") {
      p <- p + ggplot2::geom_text(
        data = flagged, ggplot2::aes(label = flag_short(flagged)),
        size = 2.3, fontface = "bold", colour = ink[["text"]]
      )
    }
  }

  if (show_companions && fill_by == "datatype") {
    cm <- present[present$companion_missing, ]
    if (nrow(cm) > 0) {
      p <- p + ggplot2::geom_point(data = cm, shape = 21, size = 2.6, stroke = 0.9,
                                   fill = "white", colour = ink[["text"]])
    }
  }

  p +
    bids_matrix_facets(cells) +
    ggplot2::scale_x_discrete(labels = strip_colkey, expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::scale_y_discrete(labels = function(s) paste0("sub-", s),
                              expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_bidser() +
    ggplot2::theme(
      panel.spacing.x = ggplot2::unit(10, "pt"),
      panel.spacing.y = ggplot2::unit(10, "pt"),
      axis.text.x = if (rotate_labels)
        ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7)
      else ggplot2::element_text(size = 7, margin = ggplot2::margin(t = 3)),
      axis.text.y = ggplot2::element_text(size = 7.5, family = "mono"),
      strip.text.x = if (show_strips)
        ggplot2::element_text(hjust = 0, vjust = 0, face = "bold", size = 8,
                              colour = ink[["text"]], lineheight = 1.05)
      else ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, vjust = 1, face = "bold",
                                                size = 8, colour = ink[["text"]]),
      strip.placement = "outside",
      strip.clip = "off",
      legend.position = "right"
    )
}

#' Column shortfalls (text above the matrix, only where a column is short)
#' @keywords internal
#' @noRd
bids_matrix_top <- function(cells) {
  ink <- bidser_ink()
  absent <- cells %>%
    dplyr::group_by(.data$session_lab, .data$subject) %>%
    dplyr::summarise(none = !any(.data$present), .groups = "drop") %>%
    dplyr::filter(.data$none)
  top <- cells %>%
    dplyr::filter(.data$state != "upstream") %>%
    dplyr::anti_join(absent, by = c("session_lab", "subject")) %>%
    dplyr::group_by(.data$group, .data$colkey) %>%
    dplyr::summarise(n = sum(.data$present), N = dplyr::n(), .groups = "drop")
  top$label <- ifelse(top$n < top$N, paste0(top$n, "/", top$N), "")
  # keep every column so the panels line up with the matrix
  all_cols <- dplyr::distinct(cells, .data$group, .data$colkey)
  top <- dplyr::left_join(all_cols, top, by = c("group", "colkey"))
  top$label[is.na(top$label)] <- ""
  top$session_lab <- factor(levels(cells$session_lab)[1], levels = levels(cells$session_lab))

  ggplot2::ggplot(top, ggplot2::aes(x = .data$colkey, y = 0)) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = 2.4, fontface = "bold",
                       colour = ink[["text"]], vjust = 0) +
    ggplot2::scale_y_continuous(limits = c(-0.1, 1), expand = c(0, 0)) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 0.5)) +
    bids_matrix_facets(cells, rows = FALSE) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_bidser() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      panel.spacing.x = ggplot2::unit(10, "pt"),
      strip.text = ggplot2::element_text(hjust = 0, vjust = 0, face = "bold", size = 8,
                                         colour = ink[["text"]], lineheight = 1.05),
      strip.clip = "off",
      plot.margin = ggplot2::margin(0, 10, 0, 8)
    )
}

#' Row shortfalls (text to the right of the matrix, only where a row is short)
#' @keywords internal
#' @noRd
bids_matrix_right <- function(cells) {
  ink <- bidser_ink()
  right <- cells %>%
    dplyr::filter(!.data$state %in% c("upstream", "absent")) %>%
    dplyr::group_by(.data$session_lab, .data$subject) %>%
    dplyr::summarise(n = sum(.data$present), N = dplyr::n(), .groups = "drop")
  right <- dplyr::right_join(right, dplyr::distinct(cells, .data$session_lab, .data$subject),
                             by = c("session_lab", "subject"))
  right$label <- ifelse(!is.na(right$n) & right$n < right$N, paste0(right$n, "/", right$N), "")
  abs_rows <- dplyr::distinct(cells[cells$state == "absent", ], .data$session_lab, .data$subject)
  is_abs <- paste(right$session_lab, right$subject) %in% paste(abs_rows$session_lab, abs_rows$subject)
  right$label[is_abs] <- "no data"

  ggplot2::ggplot(right, ggplot2::aes(y = .data$subject, x = 0)) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), hjust = 0, size = 2.4,
                       fontface = "bold", colour = ink[["text"]]) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.5)) +
    bids_matrix_facets(cells, cols = FALSE) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_bidser() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(10, "pt"),
      plot.margin = ggplot2::margin(0, 4, 0, 0)
    )
}

#' Short factual summary of a matrix
#' @keywords internal
#' @noRd
bids_matrix_summary <- function(cells, frame) {
  n_sub <- length(unique(cells$subject))
  sess <- setdiff(levels(cells$session_lab), "")
  raw <- frame[frame$role == "primary" & frame$scope == "raw" & !is.na(frame$subject), ]
  tasks <- unique(stats::na.omit(raw$task))
  n_missing <- sum(cells$state == "missing")
  n_absent <- nrow(dplyr::distinct(cells[cells$state == "absent", ], .data$session_lab, .data$subject))
  n_flag <- sum(cells$state == "size")
  n_comp <- sum(cells$companion_missing)
  kinds <- table(raw$kind)
  kinds <- kinds[order(-kinds)]
  kind_txt <- paste0(as.integer(kinds), " ", names(kinds), collapse = ", ")
  pieces <- c(
    paste0(n_sub, " subject", if (n_sub != 1) "s"),
    if (length(sess)) paste0(length(sess), " sessions"),
    if (length(tasks)) paste0(length(tasks), " task", if (length(tasks) != 1) "s"),
    if (nzchar(kind_txt)) kind_txt
  )
  total <- sum(frame$size, na.rm = TRUE)
  if (total > 0) pieces <- c(pieces, format_bytes(total))
  status <- if (n_missing + n_flag + n_comp + n_absent == 0) {
    "Complete: every subject has every acquisition."
  } else {
    paste0(paste(c(
      if (n_absent) paste0(n_absent, " subject-session", if (n_absent != 1) "s", " with no data"),
      if (n_missing) paste0(n_missing, " expected file", if (n_missing != 1) "s", " missing"),
      if (n_flag) paste0(n_flag, " of unusual size"),
      if (n_comp) paste0(n_comp, " lacking an events or confounds file")
    ), collapse = "; "), ".")
  }
  paste0(paste(pieces, collapse = " \u00b7 "), "\n", status)
}

#' List the problems by BIDS name, collapsing a whole missing subject-session
#' @keywords internal
#' @noRd
bids_matrix_problems <- function(cells, max_items = 8) {
  sub_ord <- function(d) order(as.character(d$subject), as.integer(d$session_lab))
  cells$name_plain <- paste0(
    "sub-", cells$subject,
    ifelse(is.na(cells$session), "", paste0("_ses-", cells$session)),
    ifelse(is.na(cells$stem) | cells$stem == "", "", paste0("_", cells$stem)),
    ifelse(cells$scope == "derivatives", paste0(" (", sub("\n.*", "", cells$group), ")"), "")
  )
  rows <- list()
  ab <- cells[cells$state == "absent" & cells$scope == "raw", ]
  if (nrow(ab) > 0) {
    ab <- dplyr::count(ab, .data$session_lab, .data$subject, name = "N")
    rows[[length(rows) + 1]] <- data.frame(
      subject = as.character(ab$subject), ses = as.integer(ab$session_lab),
      txt = paste0("sub-", ab$subject,
                   ifelse(as.character(ab$session_lab) == "", "", paste0("_", ab$session_lab)),
                   ": nothing acquired (", ab$N, " expected files)"))
  }
  miss <- cells[cells$state == "missing", ]
  if (nrow(miss) > 0) {
    tot <- cells %>% dplyr::filter(.data$state != "upstream") %>%
      dplyr::count(.data$session_lab, .data$subject, name = "N")
    mc <- miss %>% dplyr::count(.data$session_lab, .data$subject, name = "m") %>%
      dplyr::left_join(tot, by = c("session_lab", "subject"))
    whole <- mc[mc$m == mc$N & mc$N > 1, ]
    if (nrow(whole) > 0) {
      rows[[length(rows) + 1]] <- data.frame(
        subject = as.character(whole$subject), ses = as.integer(whole$session_lab),
        txt = paste0("sub-", whole$subject,
                     ifelse(as.character(whole$session_lab) == "", "",
                            paste0("_", whole$session_lab)),
                     ": nothing acquired (", whole$N, " files)"))
      miss <- miss[!paste(miss$session_lab, miss$subject) %in%
                     paste(whole$session_lab, whole$subject), ]
    }
    if (nrow(miss) > 0) rows[[length(rows) + 1]] <- data.frame(
      subject = as.character(miss$subject), ses = as.integer(miss$session_lab),
      txt = paste0(miss$name_plain, ": missing"))
  }
  fl <- cells[cells$state == "size", ]
  if (nrow(fl) > 0) rows[[length(rows) + 1]] <- data.frame(
    subject = as.character(fl$subject), ses = as.integer(fl$session_lab),
    txt = paste0(fl$name_plain, ": ", flag_long(fl)))
  cm <- cells[cells$companion_missing, ]
  if (nrow(cm) > 0) rows[[length(rows) + 1]] <- data.frame(
    subject = as.character(cm$subject), ses = as.integer(cm$session_lab),
    txt = paste0(cm$name_plain, ": no ",
                 dplyr::case_when(is.na(cm$companion_kind) ~ "events",
                                  cm$companion_kind == "timeseries" ~ "desc-confounds_timeseries",
                                  TRUE ~ cm$companion_kind), ".tsv"))
  if (!length(rows)) return(character())
  all <- do.call(rbind, rows)
  out <- all$txt[order(all$subject, all$ses)]
  if (length(out) > max_items) {
    out <- c(out[seq_len(max_items - 1)], paste0("\u2026 and ", length(out) - max_items + 1, " more"))
  }
  out
}

#' Assemble the acquisition matrix with margins
#' @keywords internal
#' @noRd
bids_matrix_figure <- function(x, frame, cells, title = NULL, margins = TRUE,
                               fill_by = "datatype", color_scheme = "viridis",
                               text_size = 0, rotate_labels = NULL) {
  main <- bids_matrix_plot(cells, fill_by = fill_by, color_scheme = color_scheme,
                           text_size = text_size, rotate_labels = rotate_labels,
                           show_strips = !margins)
  if (is.null(title)) title <- bids_display_name(x)
  subtitle <- bids_matrix_summary(cells, frame)
  caption <- bids_matrix_caption(cells)

  if (!margins || !requireNamespace("patchwork", quietly = TRUE)) {
    return(main + ggplot2::labs(title = title, subtitle = subtitle, caption = caption))
  }
  top <- bids_matrix_top(cells)
  right <- bids_matrix_right(cells)
  n_rows <- nrow(dplyr::distinct(cells, .data$session_lab, .data$subject))
  patchwork::wrap_plots(
    top, patchwork::plot_spacer(), main, right,
    ncol = 2, widths = c(1, 0.06), heights = c(0.45, max(n_rows, 2))
  ) +
    patchwork::plot_annotation(
      title = title, subtitle = subtitle, caption = caption,
      theme = theme_bidser() +
        ggplot2::theme(plot.caption = ggplot2::element_text(family = "mono", size = 7,
                                                            lineheight = 1.15))
    )
}

#' @keywords internal
#' @noRd
bids_matrix_caption <- function(cells) {
  key <- c("Tinted: present", "\u00d7 in frame: missing")
  if (any(cells$state == "size")) {
    key <- c(key, if (any(!is.na(cells$col_nvol[cells$state == "size"])))
      "n/N in frame: volumes vs. usual count" else "% in frame: size vs. column median")
  }
  if (any(cells$companion_missing)) key <- c(key, "\u25cb: events/confounds file missing")
  if (any(cells$scope == "derivatives")) key <- c(key, "outline: derivative")
  if (any(cells$state %in% c("upstream", "absent"))) key <- c(key, "faint dashes: not counted (no data for that subject-session, or raw input missing)")
  problems <- bids_matrix_problems(cells)
  # Wrap the key at item boundaries so it never runs off the figure
  lines <- character()
  cur <- ""
  for (k in key) {
    cand <- if (nzchar(cur)) paste0(cur, "   ", k) else k
    if (nchar(cand) > 100 && nzchar(cur)) {
      lines <- c(lines, cur)
      cur <- k
    } else {
      cur <- cand
    }
  }
  lines <- c(lines, cur)
  paste(c(lines, if (length(problems)) c("", problems)), collapse = "\n")
}

#' Display name for a project: the dataset_description.json Name when present,
#' otherwise the directory name.
#' @keywords internal
#' @noRd
bids_display_name <- function(x) {
  nm <- tryCatch(x$desc$fields$Name, error = function(e) NULL)
  if (is.character(nm) && length(nm) == 1 && nzchar(nm)) return(nm)
  x$name %||% "BIDS project"
}

#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# ---------------------------------------------------------------------------
# Structure tree
# ---------------------------------------------------------------------------

#' Plot the file layout of a BIDS project as a template tree
#'
#' Collapses every subject (and session) into a single `sub-<label>` template
#' so the dataset's *shape* fits on one screen, then annotates each template
#' with how many subjects actually have it. Directories are drawn with their
#' datatype colour; files that some subjects lack are flagged.
#'
#' @param x A `bids_project` or `mock_bids_project`.
#' @param include_derivatives Logical. Show `derivatives/` pipelines.
#' @param collapse_runs Logical. Fold `run-1`, `run-2`, ... into a single
#'   `run-<index>` row.
#' @param include_sidecars Logical. Show `.json` sidecars.
#' @param max_rows Maximum number of rows to draw before truncating.
#' @return A ggplot object.
#' @export
#' @examples
#' fs <- tibble::tribble(
#'   ~subid, ~session, ~datatype, ~task, ~run, ~suffix, ~fmriprep,
#'   "01", NA, "anat", NA, NA, "T1w.nii.gz", FALSE,
#'   "01", NA, "func", "rest", "01", "bold.nii.gz", FALSE,
#'   "02", NA, "anat", NA, NA, "T1w.nii.gz", FALSE
#' )
#' mock <- create_mock_bids("TreeDemo", c("01", "02"), fs)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_bids_tree(mock)
#' }
plot_bids_tree <- function(x, include_derivatives = TRUE, collapse_runs = TRUE,
                           include_sidecars = TRUE, max_rows = 60) {
  if (!inherits(x, "bids_project") && !inherits(x, "mock_bids_project")) {
    stop("Input must be a bids_project or mock_bids_project object")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 is required for plotting but not installed.")
  }
  frame <- bids_plot_frame(x, include_derivatives = include_derivatives)
  if (nrow(frame) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0, y = 0, label = "No files found in BIDS project") +
             ggplot2::theme_void())
  }
  if (!include_sidecars) frame <- frame[frame$role != "sidecar", ]
  rows <- bids_tree_rows(frame, collapse_runs = collapse_runs)
  absent <- attr(rows, "absent")
  truncated <- 0L
  if (nrow(rows) > max_rows) {
    truncated <- nrow(rows) - max_rows
    rows <- rows[seq_len(max_rows), ]
  }
  attr(rows, "absent") <- absent
  bids_tree_plot(rows, title = bids_display_name(x), truncated = truncated,
                 n_sub = length(unique(stats::na.omit(frame$subject))))
}

#' Files in one template whose volume count (or, failing that, size) is far
#' from the template's usual value. Returns short labels.
#' @keywords internal
#' @noRd
tree_outliers <- function(subject, session, run, size, nvol) {
  lab <- paste0("sub-", subject, ifelse(is.na(session), "", paste0(" ses-", session)),
                ifelse(is.na(run), "", paste0(" run-", run)))
  if (sum(!is.na(nvol)) >= 3) {
    mode <- as.integer(names(which.max(table(nvol))))
    bad <- !is.na(nvol) & nvol != mode
    if (!any(bad)) return(character())
    return(paste0(lab[bad], " (", nvol[bad], "/", mode, " vols)"))
  }
  ok <- !is.na(size) & size > 0
  if (sum(ok) >= 3) {
    med <- stats::median(size[ok])
    r <- size / med
    bad <- ok & (r < 0.6 | r > 1.4)
    if (!any(bad)) return(character())
    return(paste0(lab[bad], " (", round(100 * r[bad]), "% size)"))
  }
  character()
}

#' @keywords internal
#' @noRd
bids_tree_rows <- function(frame, collapse_runs = TRUE) {
  subjects <- unique(stats::na.omit(frame$subject))
  n_sub <- length(subjects)
  pairs <- unique(frame[!is.na(frame$subject), c("subject", "session")])
  tpl <- frame$path
  tpl <- gsub("sub-[A-Za-z0-9]+", "sub-<label>", tpl)
  tpl <- gsub("ses-[A-Za-z0-9]+", "ses-<label>", tpl)
  if (collapse_runs) tpl <- gsub("run-[0-9]+", "run-<index>", tpl)
  frame$template <- tpl

  leaves <- frame %>%
    dplyr::group_by(.data$template) %>%
    dplyr::summarise(
      n_files = dplyr::n(),
      n_subjects = dplyr::n_distinct(.data$subject, na.rm = TRUE),
      n_runs = dplyr::n_distinct(.data$run, na.rm = TRUE),
      sessions = list(unique(stats::na.omit(.data$session))),
      size = if (all(is.na(.data$size))) NA_real_ else stats::median(.data$size, na.rm = TRUE),
      datatype = dplyr::first(stats::na.omit(.data$datatype)) %||% NA_character_,
      role = dplyr::first(.data$role),
      per_subject = any(!is.na(.data$subject)),
      runs = list(unique(stats::na.omit(.data$run))),
      have = list(unique(paste(.data$subject, .data$session, .data$run, sep = "|"))),
      odd = list(tree_outliers(.data$subject, .data$session, .data$run, .data$size, .data$nvol)),
      .groups = "drop"
    )
  all_sessions <- unique(stats::na.omit(frame$session))
  # Subject-sessions with no files at all are reported once (in the subtitle)
  # and left out of every row's expected count.
  absent <- if (length(all_sessions)) {
    g <- expand.grid(subject = subjects, session = all_sessions, stringsAsFactors = FALSE)
    g[!paste(g$subject, g$session) %in% paste(pairs$subject, pairs$session), , drop = FALSE]
  } else data.frame(subject = character(), session = character())
  # Which subject / session / run combinations does each template lack?
  leaves$missing_txt <- vapply(seq_len(nrow(leaves)), function(i) {
    if (!leaves$per_subject[i]) return("")
    ses <- leaves$sessions[[i]]
    units <- if (length(ses)) expand.grid(subject = subjects, session = ses,
                                          stringsAsFactors = FALSE) else
      data.frame(subject = subjects, session = NA_character_)
    runs <- leaves$runs[[i]]
    if (!length(runs)) runs <- NA_character_
    units <- units[!paste(units$subject, units$session) %in% paste(absent$subject, absent$session), , drop = FALSE]
    exp <- merge(units, data.frame(run = runs, stringsAsFactors = FALSE))
    exp_key <- paste(exp$subject, exp$session, exp$run, sep = "|")
    gone <- exp[!exp_key %in% leaves$have[[i]], , drop = FALSE]
    if (nrow(gone) == 0) return("")
    lab <- paste0("sub-", gone$subject,
                  ifelse(is.na(gone$session), "", paste0(" ses-", gone$session)),
                  ifelse(is.na(gone$run), "", paste0(" run-", gone$run)))
    if (length(lab) > 2) lab <- c(lab[1:2], paste0("+", length(lab) - 2))
    paste(lab, collapse = ", ")
  }, character(1))
  leaves$session_note <- vapply(leaves$sessions, function(ses) {
    if (length(ses) && length(all_sessions) > 1 && length(ses) < length(all_sessions))
      paste0("ses-", paste(ses, collapse = "/"), " only") else ""
  }, character(1))
  # Expected count: every subject (or subject-session present in the dataset
  # for the sessions this template occurs in) times every run seen.
  leaves$expected <- vapply(seq_len(nrow(leaves)), function(i) {
    if (!leaves$per_subject[i]) return(NA_real_)
    ses <- leaves$sessions[[i]]
    units <- if (length(ses)) n_sub * length(ses) - sum(absent$session %in% ses) else n_sub
    units * max(leaves$n_runs[i], 1)
  }, numeric(1))
  leaves$units <- leaves$expected / pmax(leaves$n_runs, 1)
  leaves$has_sessions <- lengths(leaves$sessions) > 0

  # Directory nodes implied by the templates
  parts <- strsplit(leaves$template, "/", fixed = TRUE)
  dirs <- unique(unlist(lapply(parts, function(p) {
    if (length(p) < 2) return(character())
    vapply(seq_len(length(p) - 1), function(i) paste(p[seq_len(i)], collapse = "/"), "")
  })))
  nodes <- rbind(
    data.frame(id = leaves$template, is_dir = rep(FALSE, nrow(leaves)), stringsAsFactors = FALSE),
    data.frame(id = dirs, is_dir = rep(TRUE, length(dirs)), stringsAsFactors = FALSE)
  )
  nodes$depth <- lengths(strsplit(nodes$id, "/", fixed = TRUE))
  nodes$parent <- ifelse(nodes$depth > 1, dirname(nodes$id), "")
  nodes$label <- basename(nodes$id)

  # Depth-first order: at each level files first, then directories, with
  # derivatives/ always last.
  sort_key <- function(id, is_dir) {
    segs <- strsplit(id, "/", fixed = TRUE)[[1]]
    n <- length(segs)
    paste(vapply(seq_len(n), function(i) {
      dir_here <- if (i < n) TRUE else is_dir
      rank <- if (segs[i] == "derivatives") "9" else if (dir_here) "5" else "1"
      paste0(rank, segs[i])
    }, ""), collapse = "/")
  }
  nodes$key <- mapply(sort_key, nodes$id, nodes$is_dir)
  nodes <- nodes[order(nodes$key), ]
  nodes$row <- seq_len(nrow(nodes))
  nodes <- dplyr::left_join(nodes, leaves, by = c("id" = "template"))
  dt_names <- names(bids_datatype_colors())
  nodes$datatype <- ifelse(nodes$is_dir & nodes$label %in% dt_names, nodes$label, nodes$datatype)
  attr(nodes, "absent") <- absent
  nodes
}

#' @keywords internal
#' @noRd
bids_tree_plot <- function(nodes, title, truncated = 0L, n_sub = NA_integer_) {
  ink <- bidser_ink()
  n <- nrow(nodes)

  # ---- 1. Compose every row as monospace text on one character grid -------
  # Connectors use box-drawing characters, as `tree` does, so the layout
  # stays aligned at any figure size.
  is_last <- vapply(seq_len(n), function(i) {
    sib <- which(nodes$parent == nodes$parent[i])
    i == max(sib)
  }, logical(1))
  connector <- vapply(seq_len(n), function(i) {
    if (nodes$depth[i] == 1) return("")
    anc <- character()
    p <- nodes$parent[i]
    while (nzchar(p) && nodes$depth[match(p, nodes$id)] > 1) {
      j <- match(p, nodes$id)
      anc <- c(if (is_last[j]) "    " else "\u2502   ", anc)
      p <- nodes$parent[j]
    }
    paste0(paste(anc, collapse = ""), if (is_last[i]) "\u2514\u2500\u2500 " else "\u251c\u2500\u2500 ")
  }, character(1))

  name <- ifelse(nodes$is_dir, paste0(nodes$label, "/"), nodes$label)
  prefix_len <- ifelse(nodes$is_dir, 0L,
                       nchar(stringr::str_extract(nodes$label, "^(sub-<label>_)?(ses-<label>_)?")))
  prefix_len[is.na(prefix_len)] <- 0L

  leaf <- !nodes$is_dir
  complete <- !leaf | !nodes$per_subject %in% TRUE | nodes$n_files >= nodes$expected
  runs_txt <- ifelse(!is.na(nodes$n_runs) & nodes$n_runs > 1,
                     paste0(" (", nodes$units, " \u00d7 ", nodes$n_runs, " runs)"), "")
  count <- ifelse(leaf & nodes$per_subject %in% TRUE,
                  ifelse(complete, paste0(nodes$n_files, runs_txt),
                         paste0(nodes$n_files, " of ", nodes$expected, " \u2190 ", nodes$missing_txt)),
                  "")
  odd_txt <- vapply(nodes$odd, function(o) {
    if (is.null(o) || !length(o)) return("")
    if (length(o) > 2) o <- c(o[1:2], paste0("+", length(o) - 2))
    paste0("unusual: ", paste(o, collapse = ", "))
  }, character(1))
  has_odd <- leaf & nzchar(odd_txt)
  complete <- complete & !has_odd
  # An "unusual" note goes on its own continuation line when the row already
  # carries a count, so long notes never run off the right edge.
  extra <- has_odd & nzchar(count)
  count <- ifelse(has_odd & !nzchar(count), odd_txt, count)
  note <- ifelse(leaf & !is.na(nodes$session_note) & nodes$session_note != "",
                 paste0("  ", nodes$session_note), "")
  size <- ifelse(leaf & !is.na(nodes$size) & nodes$size >= 1000, format_bytes(nodes$size), "")

  head_w <- nchar(connector) + nchar(name)
  col_size <- max(head_w) + 3          # where leaders end
  size_w <- 9
  has_ann <- nzchar(size) | nzchar(count)

  pad <- function(s, w) paste0(s, strrep(" ", pmax(w - nchar(s), 0)))
  lead <- ifelse(has_ann,
                 vapply(seq_len(n), function(i) {
                   k <- col_size - head_w[i] - 1
                   if (k < 2) return(strrep(" ", max(k, 0) + 1))
                   paste0(" ", paste(rep(c(" ", "\u00b7"), length.out = k), collapse = ""))
                 }, ""),
                 strrep(" ", col_size - head_w))
  size_f <- formatC(size, width = size_w)
  # Per-layer strings: everything but the layer's own characters is blank
  blank <- function(s) strrep(" ", nchar(s))
  L <- list(
    connector = paste0(connector, blank(name), blank(lead)),
    prefix = paste0(blank(connector), substr(name, 1, prefix_len),
                    blank(substring(name, prefix_len + 1)), lead),
    name = paste0(blank(connector), blank(substr(name, 1, prefix_len)),
                  substring(name, prefix_len + 1)),
    size = paste0(strrep(" ", col_size), size_f),
    count = paste0(strrep(" ", col_size + size_w + 2), count, note)
  )
  row_y <- -(seq_len(n) + cumsum(c(0, utils::head(extra, -1))))
  cont_conn <- ifelse(nchar(connector) >= 4,
                      paste0(substr(connector, 1, nchar(connector) - 4),
                             ifelse(is_last, "    ", "\u2502   ")), "")
  extra_df <- data.frame(
    y = row_y[extra] - 1,
    conn = cont_conn[extra],
    txt = paste0(strrep(" ", col_size + size_w + 2), odd_txt[extra])[seq_len(sum(extra))]
  )
  name_col <- dplyr::case_when(
    nodes$is_dir & !is.na(nodes$datatype) ~ unname(bids_datatype_colors(nodes$datatype)),
    nodes$is_dir ~ ink[["text"]],
    nodes$role %in% "primary" ~ ink[["text"]],
    TRUE ~ ink[["muted"]]
  )
  header <- paste0(strrep(" ", col_size), formatC("size", width = size_w), "  files")

  fs <- 2.7
  mono <- function(data, colour, face = "plain") {
    ggplot2::geom_text(data = data, ggplot2::aes(x = 0, y = .data$y, label = .data$txt),
                       hjust = 0, family = "mono", size = fs, colour = colour, fontface = face)
  }
  df <- function(txt, keep = rep(TRUE, n)) data.frame(y = row_y[keep], txt = txt[keep])

  p <- ggplot2::ggplot() +
    mono(df(L$connector), ink[["faint"]]) +
    mono(df(L$prefix), ink[["faint"]]) +
    mono(df(L$name, !nodes$is_dir), name_col[!nodes$is_dir]) +
    mono(df(L$name, nodes$is_dir), name_col[nodes$is_dir], "bold") +
    mono(df(L$size), ink[["muted"]]) +
    mono(df(L$count, complete), ink[["muted"]]) +
    mono(df(L$count, !complete), ink[["text"]], "bold") +
    mono(data.frame(y = 0.2, txt = header), ink[["faint"]]) +
    mono(data.frame(y = extra_df$y, txt = extra_df$conn), ink[["faint"]]) +
    mono(data.frame(y = extra_df$y, txt = extra_df$txt), ink[["text"]], "bold") +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = title,
      subtitle = paste(strwrap(paste0(
        n_sub, if (identical(n_sub, 1L)) " subject" else " subjects", " folded into one sub-<label> template. Bold rows fall short ",
        "of one file per subject", if (any(nodes$has_sessions, na.rm = TRUE)) " \u00d7 session",
        " \u00d7 run (the arrow names who is missing) or contain a file of unusual length."),
        width = 100), collapse = "\n"),
      caption = if (truncated > 0) paste0("\u2026 ", truncated, " more rows not shown") else NULL
    ) +
    theme_bidser() +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank())
  absent <- attr(nodes, "absent")
  if (!is.null(absent) && nrow(absent) > 0) {
    p$labels$subtitle <- paste0(p$labels$subtitle, "\nNo files at all for ",
                                paste0("sub-", absent$subject, " ses-", absent$session, collapse = ", "),
                                "; not counted below.")
  }
  # Width (in characters) of the widest line, for choosing a figure width:
  # about 0.075 inches per character at the default text size.
  attr(p, "bidser_chars") <- max(nchar(L$count), nchar(extra_df$txt), col_size + size_w)
  p
}
