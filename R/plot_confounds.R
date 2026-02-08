#' Plot PCA confounds
#'
#' Visualize principal component scores and loadings returned by
#' \code{read_confounds(..., npcs = ...)}. When multiple runs are present,
#' the default view facets per run for scores (up to \code{max_panels}) and
#' aggregates loadings across runs.
#'
#' @param x A \code{bids_confounds} object returned by \code{read_confounds()}.
#' @param view Character. One of \code{"auto"}, \code{"run"}, or
#'   \code{"aggregate"}.
#' @param pcs Integer or character vector of PCs to plot (e.g., \code{1:5}
#'   or \code{c("PC1", "PC2")}).
#' @param top_n Integer. Keep the top \code{top_n} variables per PC based on
#'   absolute loading. Set to \code{NULL} to keep all variables.
#' @param max_panels Integer. In \code{view = "auto"}, facet score plots only
#'   when the number of runs is at most \code{max_panels}.
#' @param ... Unused.
#' @return A ggplot object, or a list of ggplot objects when patchwork is not
#'   available.
#' @examples
#' \donttest{
#' parts <- c("01", "02")
#' fs <- tibble::tibble(
#'   subid = rep(c("01", "02"), each = 2),
#'   datatype = "func",
#'   suffix = rep(c("bold.nii.gz", "desc-confounds_timeseries.tsv"), 2),
#'   task = "rest", fmriprep = TRUE
#' )
#' conf_data <- list()
#' for (p in parts) {
#'   key <- paste0("derivatives/fmriprep/sub-", p,
#'                 "/func/sub-", p, "_task-rest_desc-confounds_timeseries.tsv")
#'   conf_data[[key]] <- data.frame(
#'     csf = rnorm(100), white_matter = rnorm(100),
#'     global_signal = rnorm(100), framewise_displacement = abs(rnorm(100)),
#'     trans_x = rnorm(100), trans_y = rnorm(100), trans_z = rnorm(100),
#'     rot_x = rnorm(100), rot_y = rnorm(100), rot_z = rnorm(100)
#'   )
#' }
#' mock <- create_mock_bids("ConfPlot", parts, fs, confound_data = conf_data)
#' conf <- read_confounds(mock, npcs = 3)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(conf)
#' }
#' }
#' @export
plot.bids_confounds <- function(x, view = c("auto", "run", "aggregate"),
                                pcs = NULL, top_n = 8, max_panels = 6, ...) {
  if (!inherits(x, "bids_confounds")) {
    stop("x must be a 'bids_confounds' object returned by read_confounds().")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 is required for plotting but not installed.")
  }

  pca_meta <- attr(x, "pca")
  if (is.null(pca_meta) || nrow(pca_meta) == 0) {
    stop("No PCA metadata found. Run read_confounds(..., npcs=) or perc_var= first.")
  }

  view <- match.arg(view)
  x_tbl <- dplyr::ungroup(x)
  if ("data" %in% names(x_tbl)) {
    x_tbl <- tidyr::unnest(x_tbl, cols = "data")
  }

  pc_cols <- grep("^PC\\d+$", names(x_tbl), value = TRUE)
  if (length(pc_cols) == 0) {
    stop("No PC columns found in confound data.")
  }

  pc_cols <- pc_cols[order(as.integer(sub("^PC", "", pc_cols)))]
  if (!is.null(pcs)) {
    if (is.numeric(pcs)) {
      pcs <- paste0("PC", pcs)
    }
    pc_cols <- intersect(pc_cols, pcs)
    if (length(pc_cols) == 0) {
      stop("No requested PCs found in confound data.")
    }
  }

  id_cols <- intersect(c("participant_id", "task", "session", "run"), names(x_tbl))
  if (length(id_cols) == 0) {
    id_cols <- intersect(c(".subid", ".task", ".session", ".run", ".desc"), names(x_tbl))
  }

  label_key <- c(
    participant_id = "sub", .subid = "sub",
    task = "task", .task = "task",
    session = "ses", .session = "ses",
    run = "run", .run = "run",
    desc = "desc", .desc = "desc"
  )

  make_panel_label <- function(df, cols) {
    if (length(cols) == 0) {
      return(rep("all", nrow(df)))
    }
    parts <- lapply(cols, function(col) {
      prefix <- label_key[[col]]
      val <- as.character(df[[col]])
      val[is.na(val) | val == ""] <- "unknown"
      if (prefix == "sub") {
        paste0("sub-", val)
      } else {
        paste0(prefix, "-", val)
      }
    })
    do.call(paste, c(parts, sep = " | "))
  }

  x_tbl$panel <- make_panel_label(x_tbl, id_cols)
  x_tbl <- x_tbl %>%
    dplyr::group_by(.data$panel) %>%
    dplyr::mutate(timepoint = dplyr::row_number()) %>%
    dplyr::ungroup()

  scores_long <- tidyr::pivot_longer(
    x_tbl,
    cols = dplyr::all_of(pc_cols),
    names_to = "pc",
    values_to = "score"
  ) %>%
    dplyr::filter(!is.na(.data$score))

  panel_levels <- unique(scores_long$panel)
  n_panels <- length(panel_levels)

  pca_id_cols <- intersect(id_cols, names(pca_meta))
  pca_meta$panel <- make_panel_label(pca_meta, pca_id_cols)

  variance_list <- Map(function(pca, panel) {
    if (is.null(pca) || is.null(pca$variance)) {
      return(NULL)
    }
    tibble::tibble(
      panel = panel,
      pc = names(pca$variance),
      variance = as.numeric(pca$variance)
    )
  }, pca_meta$pca, pca_meta$panel)
  variance_df <- dplyr::bind_rows(variance_list)
  if (!"pc" %in% names(variance_df)) {
    variance_map <- tibble::tibble(pc = character(), variance = numeric())
  } else {
    variance_df <- variance_df %>% dplyr::filter(.data$pc %in% pc_cols)
    variance_map <- variance_df %>%
      dplyr::group_by(.data$pc) %>%
      dplyr::summarize(variance = mean(.data$variance, na.rm = TRUE), .groups = "drop")
  }

  if (nrow(variance_map) > 0) {
    pc_label_map <- stats::setNames(
      paste0(variance_map$pc, " (", sprintf("%.1f", variance_map$variance), "%)"),
      variance_map$pc
    )
    pc_label_levels <- ifelse(pc_cols %in% names(pc_label_map), pc_label_map[pc_cols], pc_cols)
  } else {
    pc_label_map <- stats::setNames(pc_cols, pc_cols)
    pc_label_levels <- pc_cols
  }

  scores_long$pc_label <- as.character(pc_label_map[scores_long$pc])
  missing_lbl <- is.na(scores_long$pc_label)
  if (any(missing_lbl)) {
    scores_long$pc_label[missing_lbl] <- scores_long$pc[missing_lbl]
  }
  scores_long$pc_label <- factor(scores_long$pc_label, levels = pc_label_levels)

  show_scores <- TRUE
  show_loadings <- TRUE
  aggregate_loadings <- FALSE
  facet_scores <- FALSE
  facet_loadings <- FALSE

  if (view == "auto") {
    if (n_panels > 1) {
      aggregate_loadings <- TRUE
      show_scores <- n_panels <= max_panels
      facet_scores <- show_scores
    }
  } else if (view == "run") {
    facet_scores <- n_panels > 1
    facet_loadings <- n_panels > 1
  } else {
    show_scores <- FALSE
    aggregate_loadings <- TRUE
  }

  p_scores <- NULL
  if (show_scores) {
    p_scores <- ggplot2::ggplot(
      scores_long,
      ggplot2::aes(x = .data$timepoint, y = .data$score, color = .data$pc_label)
    ) +
      ggplot2::geom_line(linewidth = 0.35, alpha = 0.85) +
      ggplot2::labs(
        title = "Confound PC scores",
        x = "Timepoint",
        y = "Score",
        color = "PC"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "bottom",
        plot.title.position = "plot"
      )

    if (facet_scores) {
      ncol_scores <- if (n_panels <= 2) 1 else if (n_panels <= 6) 2 else 3
      p_scores <- p_scores + ggplot2::facet_wrap(~ panel, ncol = ncol_scores, scales = "free_x")
    }
  }

  loadings_list <- Map(function(pca, panel) {
    if (is.null(pca) || is.null(pca$rotation)) {
      return(NULL)
    }
    rot <- pca$rotation
    keep_pcs <- intersect(colnames(rot), pc_cols)
    if (length(keep_pcs) == 0) {
      return(NULL)
    }
    rot <- rot[, keep_pcs, drop = FALSE]
    tibble::tibble(
      panel = panel,
      variable = rep(rownames(rot), times = ncol(rot)),
      pc = rep(colnames(rot), each = nrow(rot)),
      loading = as.vector(rot)
    )
  }, pca_meta$pca, pca_meta$panel)
  loadings_df <- dplyr::bind_rows(loadings_list)

  if (nrow(loadings_df) == 0) {
    stop("No PCA loadings found to plot.")
  }

  if (aggregate_loadings) {
    loadings_df <- loadings_df %>%
      dplyr::group_by(.data$variable, .data$pc) %>%
      dplyr::summarize(loading = mean(abs(.data$loading), na.rm = TRUE), .groups = "drop")
  }

  if (!is.null(top_n)) {
    if (aggregate_loadings) {
      loadings_df <- loadings_df %>%
        dplyr::group_by(.data$pc) %>%
        dplyr::slice_max(abs(.data$loading), n = top_n, with_ties = FALSE) %>%
        dplyr::ungroup()
    } else {
      loadings_df <- loadings_df %>%
        dplyr::group_by(.data$panel, .data$pc) %>%
        dplyr::slice_max(abs(.data$loading), n = top_n, with_ties = FALSE) %>%
        dplyr::ungroup()
    }
  }

  var_order <- loadings_df %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::summarize(score = max(abs(.data$loading), na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(.data$score) %>%
    dplyr::pull(.data$variable)
  loadings_df$variable <- factor(loadings_df$variable, levels = var_order)

  loadings_df$pc_label <- as.character(pc_label_map[loadings_df$pc])
  missing_lbl <- is.na(loadings_df$pc_label)
  if (any(missing_lbl)) {
    loadings_df$pc_label[missing_lbl] <- loadings_df$pc[missing_lbl]
  }
  loadings_df$pc_label <- factor(loadings_df$pc_label, levels = pc_label_levels)

  loadings_title <- if (aggregate_loadings) {
    "PC loadings (mean absolute across runs)"
  } else {
    "PC loadings"
  }

  p_loadings <- ggplot2::ggplot(
    loadings_df,
    ggplot2::aes(x = .data$pc_label, y = .data$variable, fill = .data$loading)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::labs(
      title = loadings_title,
      x = "PC",
      y = NULL,
      fill = "Loading"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot"
    )

  if (aggregate_loadings) {
    p_loadings <- p_loadings +
      ggplot2::scale_fill_gradient(low = "#F1F5F9", high = "#1F77B4")
  } else {
    p_loadings <- p_loadings +
      ggplot2::scale_fill_gradient2(low = "#2C7FB8", mid = "#F7F7F7", high = "#D95F0E")
  }

  if (facet_loadings) {
    ncol_loadings <- if (n_panels <= 2) 1 else if (n_panels <= 6) 2 else 3
    p_loadings <- p_loadings + ggplot2::facet_wrap(~ panel, ncol = ncol_loadings)
  }

  if (!is.null(p_scores) && !is.null(p_loadings)) {
    if (requireNamespace("patchwork", quietly = TRUE)) {
      return(patchwork::wrap_plots(list(p_scores, p_loadings), ncol = 1, heights = c(2, 1)))
    }
    warning("Package patchwork is not installed; returning a list of plots.")
    return(list(scores = p_scores, loadings = p_loadings))
  }

  if (!is.null(p_scores)) return(p_scores)
  p_loadings
}
