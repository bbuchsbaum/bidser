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
#' @param fd_threshold Numeric. Framewise-displacement threshold in mm drawn
#'   on the motion panel (shown when a `framewise_displacement` column was
#'   kept, e.g. via `confound_strategy(raw_vars = "framewise_displacement")`).
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
                                pcs = NULL, top_n = 8, max_panels = 6,
                                fd_threshold = 0.5, ...) {
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

  varying <- id_cols[vapply(id_cols, function(col) {
    length(unique(as.character(x_tbl[[col]]))) > 1
  }, logical(1))]
  constant <- setdiff(id_cols, varying)
  # read_confounds() fills session = "1" for datasets without sessions; do not
  # print an entity that is not in the file names.
  constant <- constant[!(constant %in% c("session", ".session") &
                           vapply(constant, function(col) all(as.character(x_tbl[[col]]) %in% c("1", NA)), logical(1)))]
  ent_lab <- function(col, val) {
    prefix <- label_key[[col]]
    val <- as.character(val)
    val[is.na(val) | val == ""] <- "unknown"
    paste0(prefix, "-", val)
  }
  # Labels follow BIDS entity order (sub, ses, task, run, desc); the run goes
  # on its own line so narrow strips stay readable.
  bids_rank <- c(participant_id = 1, .subid = 1, session = 2, .session = 2,
                 task = 3, .task = 3, run = 4, .run = 4, desc = 5, .desc = 5)
  make_panel_label <- function(df, cols) {
    cols <- intersect(varying, cols)
    if (length(cols) == 0) {
      return(rep("all runs", nrow(df)))
    }
    cols <- cols[order(bids_rank[cols])]
    run_cols <- intersect(cols, c("run", ".run"))
    rest <- setdiff(cols, run_cols)
    lab <- function(cs) if (length(cs)) do.call(paste, c(lapply(cs, function(col) ent_lab(col, df[[col]])), sep = " ")) else NULL
    first <- lab(rest)
    second <- lab(run_cols)
    if (!is.null(first) && !is.null(second)) paste0(first, "\n", second) else if (!is.null(first)) first else second
  }
  context <- if (length(constant)) {
    paste(vapply(constant, function(col) ent_lab(col, x_tbl[[col]][1]), ""), collapse = " ")
  } else ""
  # Panels in a stable order: subject, task, session (chronological), run
  id_ord <- id_cols[order(bids_rank[id_cols])]
  ord_df <- unique(x_tbl[, id_ord, drop = FALSE])
  ses_col <- intersect(id_cols, c("session", ".session"))
  ord_keys <- lapply(id_ord, function(col) {
    v <- as.character(ord_df[[col]])
    if (col %in% ses_col) match(v, sub("^ses-", "", session_order(v))) else
      suppressWarnings(if (all(!is.na(as.numeric(v)))) as.numeric(v) else v)
  })
  ord_df <- ord_df[do.call(order, ord_keys), , drop = FALSE]
  panel_order <- unique(make_panel_label(ord_df, id_cols))

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

  panel_levels <- intersect(panel_order, unique(scores_long$panel))
  scores_long$panel <- factor(scores_long$panel, levels = panel_levels)
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

  ink <- bidser_ink()
  p_scores <- NULL
  if (show_scores) {
    # One row per component, one column per run: small multiples that share a
    # y-axis within each component, so spikes and drifts compare directly.
    p_scores <- ggplot2::ggplot(
      scores_long,
      ggplot2::aes(x = .data$timepoint, y = .data$score)
    ) +
      ggplot2::geom_hline(yintercept = 0, colour = ink[["rule"]], linewidth = 0.3) +
      ggplot2::geom_line(linewidth = 0.3, colour = ink[["text"]]) +
      ggplot2::scale_y_continuous(breaks = function(lims) {
        if (!all(is.finite(lims)) || diff(lims) < 1e-8) return(0)
        scales::breaks_extended(3)(lims)
      }, labels = function(b) {
        b[abs(b) < 1e-8] <- 0
        format(signif(b, 2), trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
      }) +
      ggplot2::labs(
        title = paste(c("Confound principal components over time", if (nzchar(context)) paste0("\u00b7 ", context)), collapse = " "),
        subtitle = "Rows: components, with their share of confound variance. Columns: runs.",
        x = "Volume",
        y = NULL
      ) +
      theme_bidser(grid = "") +
      ggplot2::theme(
        strip.text.y = ggplot2::element_text(angle = 0, hjust = 0, face = "bold",
                                             size = 8, colour = ink[["text"]]),
        strip.text.x = ggplot2::element_text(hjust = 0, size = 8, colour = ink[["muted"]]),
        panel.spacing = ggplot2::unit(6, "pt"),
        axis.text.y = ggplot2::element_text(size = 6.5, colour = ink[["faint"]])
      )

    if (facet_scores) {
      p_scores <- p_scores +
        ggplot2::facet_grid(pc_label ~ panel, scales = "free_y",
                            labeller = ggplot2::labeller(panel = ggplot2::label_wrap_gen(28)))
    } else {
      p_scores <- p_scores + ggplot2::facet_grid(pc_label ~ ., scales = "free_y")
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

  # Keep the top_n variables overall (by their largest |loading| on any
  # shown PC) and show every PC for them, so an absent dot never means
  # "filtered out". Rows are sorted by |loading| on the first PC, the same
  # way in every view.
  strength <- loadings_df %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::summarize(score = max(abs(.data$loading), na.rm = TRUE), .groups = "drop")
  if (!is.null(top_n)) {
    keep_vars <- strength %>%
      dplyr::slice_max(.data$score, n = top_n, with_ties = FALSE) %>%
      dplyr::pull(.data$variable)
    loadings_df <- loadings_df[loadings_df$variable %in% keep_vars, ]
  }
  first_pc <- pc_cols[1]
  var_order <- loadings_df %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::summarize(
      score = mean(abs(.data$loading[.data$pc == first_pc]), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$score, dplyr::desc(.data$variable)) %>%
    dplyr::pull(.data$variable)
  loadings_df$variable <- factor(loadings_df$variable, levels = var_order)

  loadings_df$pc_label <- as.character(pc_label_map[loadings_df$pc])
  missing_lbl <- is.na(loadings_df$pc_label)
  if (any(missing_lbl)) {
    loadings_df$pc_label[missing_lbl] <- loadings_df$pc[missing_lbl]
  }
  loadings_df$pc_label <- factor(loadings_df$pc_label, levels = pc_label_levels)

  loadings_title <- if (aggregate_loadings) {
    "What drives each component (mean |loading| across runs)"
  } else {
    "What drives each component (signed loadings)"
  }

  loadings_df$sign <- ifelse(loadings_df$loading >= 0, "positive", "negative")
  p_loadings <- ggplot2::ggplot(
    loadings_df,
    ggplot2::aes(x = .data$loading, y = .data$variable)
  ) +
    ggplot2::geom_vline(xintercept = 0, colour = ink[["rule"]], linewidth = 0.4) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$loading, yend = .data$variable),
                          colour = ink[["faint"]], linewidth = 0.35) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$sign), size = 1.6) +
    ggplot2::scale_colour_manual(values = c(positive = ink[["text"]], negative = "#C2502E"),
                                 guide = "none") +
    ggplot2::scale_x_continuous(
      limits = if (aggregate_loadings) c(0, max(loadings_df$loading, na.rm = TRUE) * 1.05)
               else c(-1, 1) * max(abs(loadings_df$loading), na.rm = TRUE) * 1.05,
      breaks = scales::breaks_pretty(n = 3)
    ) +
    ggplot2::labs(
      title = loadings_title,
      subtitle = sprintf("%s of %d confounds, by largest |loading|; rows sorted by %s.",
                         if (length(levels(loadings_df$variable)) < nrow(strength))
                           paste("Top", length(levels(loadings_df$variable))) else "All",
                         nrow(strength), first_pc),
      x = NULL, y = NULL) +
    theme_bidser(grid = "y") +
    ggplot2::theme(
      strip.text = ggplot2::element_text(hjust = 0, face = "bold", size = 8),
      panel.spacing.x = ggplot2::unit(12, "pt"),
      axis.text.y = ggplot2::element_text(size = 7.5, colour = ink[["text"]])
    )

  if (facet_loadings) {
    p_loadings <- p_loadings + ggplot2::facet_grid(panel ~ pc_label)
  } else {
    p_loadings <- p_loadings + ggplot2::facet_grid(. ~ pc_label)
  }

  # Motion: framewise displacement, when it was kept as a raw column
  fd_col <- intersect(c("framewise_displacement", "FramewiseDisplacement"), names(x_tbl))
  p_fd <- NULL
  if (length(fd_col) > 0) {
    fd_df <- tibble::tibble(panel = factor(x_tbl$panel, levels = panel_levels),
                            timepoint = x_tbl$timepoint,
                            fd = suppressWarnings(as.numeric(x_tbl[[fd_col[1]]])))
    fd_df <- fd_df[!is.na(fd_df$panel), ]
    fd_sum <- fd_df %>%
      dplyr::group_by(.data$panel) %>%
      dplyr::summarize(mean_fd = mean(.data$fd, na.rm = TRUE),
                       pct = 100 * mean(.data$fd > fd_threshold, na.rm = TRUE),
                       n_over = sum(.data$fd > fd_threshold, na.rm = TRUE),
                       .groups = "drop")
    fd_sum$label <- sprintf("mean %.2f mm\n%d vols (%.0f%%) > %.1f mm", fd_sum$mean_fd,
                            fd_sum$n_over, fd_sum$pct, fd_threshold)
    if (show_scores) {
      over <- fd_df[!is.na(fd_df$fd) & fd_df$fd > fd_threshold, ]
      p_fd <- ggplot2::ggplot(fd_df, ggplot2::aes(x = .data$timepoint, y = .data$fd)) +
        ggplot2::geom_hline(yintercept = fd_threshold, colour = "#C2502E",
                            linewidth = 0.35, linetype = "22") +
        ggplot2::geom_line(linewidth = 0.3, colour = ink[["text"]], na.rm = TRUE) +
        ggplot2::geom_point(data = over, colour = "#C2502E", size = 0.9) +
        ggplot2::facet_grid(. ~ panel, labeller = ggplot2::as_labeller(
          stats::setNames(paste0(ifelse(as.character(fd_sum$panel) == "all runs", "",
                                        paste0(as.character(fd_sum$panel), "\n")),
                                 fd_sum$label),
                          as.character(fd_sum$panel)))) +
        ggplot2::scale_y_continuous(breaks = scales::breaks_extended(3),
                                    expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
        ggplot2::labs(title = paste(c("Head motion", if (nzchar(context)) paste0("\u00b7 ", context)), collapse = " "),
                      subtitle = sprintf("Framewise displacement (mm); dashed line and dots: volumes above %.1f mm.", fd_threshold),
                      x = NULL, y = "FD (mm)") +
        theme_bidser(grid = "") +
        ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0, size = 7.5, colour = ink[["muted"]],
                                                            lineheight = 1.1),
                       axis.text.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_text(size = 6.5, colour = ink[["faint"]]),
                       panel.spacing = ggplot2::unit(6, "pt"))
      if (!is.null(p_scores)) {
        p_scores <- p_scores + ggplot2::theme(strip.text.x = ggplot2::element_blank()) +
          ggplot2::labs(title = "Confound principal components")
      }
    } else {
      # One row per subject, one dot per run: the run-level distribution
      # is the QC question, and individual bad runs are labelled.
      sub_col <- intersect(c("participant_id", ".subid"), names(x_tbl))
      run_ids <- unique(x_tbl[, c("panel", sub_col), drop = FALSE])
      fd_sum <- dplyr::left_join(fd_sum, dplyr::mutate(run_ids, panel = factor(.data$panel, levels = panel_levels)),
                                 by = "panel")
      fd_sum$subject <- if (length(sub_col)) paste0("sub-", fd_sum[[sub_col[1]]]) else "all runs"
      fd_sum$subject <- factor(fd_sum$subject, levels = rev(sort(unique(fd_sum$subject))))
      fd_sum$bad <- fd_sum$pct > 20
      bad <- fd_sum[fd_sum$bad, ]
      bad_pct <- 20
      fd_sum$bad <- fd_sum$pct > bad_pct
      bad <- fd_sum[fd_sum$bad, ]
      p_fd <- ggplot2::ggplot(fd_sum, ggplot2::aes(x = .data$pct, y = .data$subject)) +
        ggplot2::geom_vline(xintercept = bad_pct, colour = "#C2502E", linewidth = 0.35,
                            linetype = "22") +
        ggplot2::geom_point(ggplot2::aes(colour = .data$bad), size = 1.4, alpha = 0.7,
                            shape = 16,
                            position = ggplot2::position_jitter(width = 0, height = 0.22, seed = 1)) +
        ggplot2::scale_colour_manual(values = c(`FALSE` = ink[["muted"]], `TRUE` = "#C2502E"),
                                     guide = "none") +
        ggplot2::scale_x_continuous(limits = c(0, max(c(fd_sum$pct, bad_pct), na.rm = TRUE) * 1.15),
                                    expand = c(0, 0), labels = function(v) paste0(v, "%")) +
        ggplot2::labs(
          title = "Head motion per run",
          subtitle = sprintf(paste0("Share of volumes with framewise displacement above %.1f mm, ",
                                    "one dot per run (%d runs).\nRuns right of the dashed line ",
                                    "(%d%%) are named."), fd_threshold, nrow(fd_sum), bad_pct),
          x = NULL, y = NULL) +
        theme_bidser(grid = "y") +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7.5, family = "mono",
                                                           colour = ink[["text"]]))
      if (nrow(bad) > 0) {
        bad$lab <- sprintf("%s: %.0f%%, mean %.2f mm",
                           trimws(gsub("\\s+", " ", gsub("sub-[^ \n]+|\n", " ", bad$panel))),
                           bad$pct, bad$mean_fd)
        p_fd <- p_fd + ggplot2::geom_text(
          data = bad, ggplot2::aes(label = .data$lab),
          hjust = 1.04, size = 2.3, colour = "#C2502E")
      }
    }
  }

  plots <- Filter(Negate(is.null), list(fd = p_fd, scores = p_scores, loadings = p_loadings))
  if (length(plots) > 1) {
    if (requireNamespace("patchwork", quietly = TRUE)) {
      h <- c(fd = if (show_scores) 1.1 else max(1.2, length(unique(x_tbl[[intersect(c("participant_id", ".subid", "panel"), names(x_tbl))[1]]])) / 3.5),
             scores = length(pc_cols),
             loadings = max(2, length(levels(loadings_df$variable)) / 4))[names(plots)]
      # White background for the whole assembly, whatever the session theme
      return(patchwork::wrap_plots(plots, ncol = 1, heights = h) +
               patchwork::plot_annotation(theme = ggplot2::theme(
                 plot.background = ggplot2::element_rect(fill = "white", colour = NA))))
    }
    warning("Package patchwork is not installed; returning a list of plots.")
    return(plots)
  }
  plots[[1]]
}
