#' A quiet ggplot2 theme for BIDS figures
#'
#' `theme_bidser()` is the house style used by [plot_bids()],
#' [plot_bids_tree()], [bids_heatmap()] and `plot()` on confound objects. It
#' removes non-data ink (panel fills, boxes, most grid lines), left-aligns
#' titles, and uses a single warm-neutral ink colour so that the data marks
#' carry the colour.
#'
#' @param base_size Base font size in points.
#' @param base_family Base font family.
#' @param grid Which major grid lines to keep: `"none"`, `"x"`, `"y"` or `"xy"`.
#' @return A ggplot2 theme object.
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point() +
#'     theme_bidser(grid = "y")
#' }
theme_bidser <- function(base_size = 11, base_family = "", grid = "none") {
  ink <- bidser_ink()
  grid_line <- ggplot2::element_line(colour = ink[["rule"]], linewidth = 0.3)
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = ink[["text"]]),
      plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.3,
                                         hjust = 0, margin = ggplot2::margin(b = 3)),
      plot.subtitle = ggplot2::element_text(colour = ink[["muted"]], size = base_size,
                                            hjust = 0, margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(colour = ink[["muted"]], size = base_size * 0.8,
                                           hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.title = ggplot2::element_text(colour = ink[["muted"]], size = base_size * 0.85),
      axis.text = ggplot2::element_text(colour = ink[["muted"]], size = base_size * 0.8),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major.x = if (grepl("x", grid)) grid_line else ggplot2::element_blank(),
      panel.grid.major.y = if (grepl("y", grid)) grid_line else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(colour = ink[["text"]], face = "bold",
                                         size = base_size * 0.85, hjust = 0),
      legend.title = ggplot2::element_text(colour = ink[["muted"]], size = base_size * 0.8),
      legend.text = ggplot2::element_text(colour = ink[["muted"]], size = base_size * 0.8),
      legend.key.size = ggplot2::unit(base_size * 0.9, "pt"),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(8, 10, 8, 8)
    )
}

#' @keywords internal
#' @noRd
bidser_ink <- function() {
  c(text = "#1F2328", muted = "#6A6F76", faint = "#A9ADB2",
    rule = "#E6E4E0", empty = "#F1EFEB", alert = "#B42318")
}

#' Colours for BIDS datatypes
#'
#' A fixed, colour-blind-aware palette so that a datatype has the same colour
#' in every bidser figure. Unknown datatypes fall back to a neutral grey.
#'
#' @param types Optional character vector of datatypes to look up.
#' @return A named character vector of hex colours.
#' @export
#' @examples
#' bids_datatype_colors()
#' bids_datatype_colors(c("anat", "func", "unknown"))
bids_datatype_colors <- function(types = NULL) {
  pal <- c(
    anat = "#2F5D7C",  # slate blue
    func = "#C2502E",  # brick
    fmap = "#7B5EA7",  # violet
    dwi  = "#2E7D5B",  # green
    perf = "#C98B1D",  # ochre
    beh  = "#8A6D52",  # umber
    eeg  = "#21808D", meg = "#21808D", ieeg = "#21808D",  # teal family
    nirs = "#21808D", pet = "#A23B72", micr = "#5C6B73",
    other = "#7D8288"
  )
  if (is.null(types)) return(pal)
  out <- pal[types]
  out[is.na(out)] <- pal[["other"]]
  names(out) <- types
  out
}

#' @keywords internal
#' @noRd
bids_datatype_order <- function(types) {
  known <- c("anat", "func", "fmap", "dwi", "perf", "pet", "beh",
             "eeg", "meg", "ieeg", "nirs", "micr")
  types <- unique(types)
  c(intersect(known, types), sort(setdiff(types, known)))
}

#' Discrete fill scale for BIDS datatypes
#' @keywords internal
#' @noRd
scale_fill_bids_datatype <- function(types, ...) {
  ggplot2::scale_fill_manual(values = bids_datatype_colors(unique(types)), ...)
}

#' Human-friendly byte formatting (base-1000, as file browsers show).
#' @keywords internal
#' @noRd
format_bytes <- function(x) {
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  v <- x[ok]
  units <- c("B", "kB", "MB", "GB", "TB")
  pow <- pmin(pmax(floor(log10(pmax(abs(v), 1)) / 3), 0), length(units) - 1)
  val <- v / 1000^pow
  digits <- ifelse(val < 10 & pow > 0, 1, 0)
  out[ok] <- paste(ifelse(digits == 1, sprintf("%.1f", val), sprintf("%.0f", val)), units[pow + 1])
  out[ok] <- sub("\\.0 ", " ", out[ok])
  out
}
