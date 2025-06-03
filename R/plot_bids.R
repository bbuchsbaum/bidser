#' Plot a comprehensive visual overview of a BIDS project
#'
#' This function creates a multi-panel visualization of a BIDS project structure,
#' showing file distributions, completeness, and data characteristics.
#'
#' @param x A \code{bids_project} object
#' @param interactive Logical. Whether to create an interactive plot (default TRUE)
#' @param color_scheme Character. Name of the color palette to use (default "viridis")
#' @param include_derivatives Logical. Whether to include derivatives data in the visualization (default TRUE)
#' @param file_size_scale Character. Whether to scale file sizes ("log", "sqrt", or "linear", default "log")
#' @param highlight_missing Logical. Whether to highlight missing data points (default TRUE)
#' @param visualization_mode Character. The mode of visualization to use ("standard", "heatmap", or "complete")
#' @param debug Logical. Whether to print debugging information (default FALSE)
#' @return A plot object (ggplot2, plotly, or other depending on settings)
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import patchwork
#' @import plotly
#' @import viridis
#' @export
#' @examples
#' \dontrun{
#' proj <- bids_project(system.file("extdata/ds001", package="bidser"))
#' plot_bids(proj)
#'
#' # Create a non-interactive plot with a different color scheme
#' plot_bids(proj, interactive = FALSE, color_scheme = "plasma")
#'
#' # Create a focused heatmap visualization
#' plot_bids(proj, visualization_mode = "heatmap")
#'
#' # Create a comprehensive visualization with all plot types
#' plot_bids(proj, visualization_mode = "complete")
#' 
#' # Or use the specialized heatmap function
#' bids_heatmap(proj)
#'
#' # Create a mock project and visualize it
#' file_structure <- data.frame(
#'   subid = c("01", "02", "03"),
#'   session = c("pre", "post", "pre"),
#'   datatype = c("anat", "func", "func"),
#'   task = c(NA, "rest", "task1"),
#'   run = c(NA, "01", "02"),
#'   suffix = c("T1w.nii.gz", "bold.nii.gz", "bold.nii.gz"),
#'   fmriprep = FALSE
#' )
#' mock_proj <- create_mock_bids(
#'   project_name = "my_project",
#'   participants = c("sub-01", "sub-02", "sub-03"),
#'   file_structure = file_structure
#' )
#' plot_bids(mock_proj, highlight_missing = TRUE)
#' }
plot_bids <- function(x, interactive = TRUE, color_scheme = "viridis",
                      include_derivatives = TRUE, file_size_scale = "log",
                      highlight_missing = TRUE, visualization_mode = "standard",
                      debug = FALSE) {
  
  # Check input - accept both bids_project and mock_bids_project
  if (!inherits(x, "bids_project") && !inherits(x, "mock_bids_project")) {
    stop("Input must be a bids_project or mock_bids_project object")
  }
  
  # Load required packages
  for (pkg in c("ggplot2", "dplyr", "viridis", "scales")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package ", pkg, " is required but not installed")
    }
  }
  if (interactive && !requireNamespace("plotly", quietly = TRUE)) {
    warning("Package plotly is required for interactive plots but not installed. Falling back to static plot.")
    interactive <- FALSE
  }
  
  # Validate mode
  valid_modes <- c("standard", "heatmap", "complete")
  if (!visualization_mode %in% valid_modes) {
    warning("Invalid visualization_mode. Using 'standard'.")
    visualization_mode <- "standard"
  }

  # Print debug info if requested
  if (debug) {
    cat("Project name:", x$name, "\n")
    cat("Project type:", paste(class(x), collapse=" "), "\n")
    if (!is.null(x$subjects)) cat("Number of subjects:", length(x$subjects), "\n")
    if (!is.null(x$tasks)) cat("Tasks:", paste(x$tasks, collapse=", "), "\n")
    cat("Has raw data table:", !is.null(x$tbl), "\n")
    if (!is.null(x$tbl)) cat("Raw data rows:", nrow(x$tbl), "\n")
    cat("Has sessions:", !is.null(x$sessions) && length(x$sessions) > 0, "\n")
  }
  
  # Check if the BIDS project has any data
  if (is.null(x$tbl) || nrow(x$tbl) == 0) {
    if (debug) cat("No data found in the BIDS project\n")
    # Create a minimal plot indicating no data
    p <- ggplot2::ggplot() + 
         ggplot2::annotate("text", x = 0.5, y = 0.5, 
                  label = "No data found in BIDS project") +
         ggplot2::theme_void() +
         ggplot2::theme(
           plot.background = ggplot2::element_rect(fill = "white"),
           plot.margin = ggplot2::margin(20, 20, 20, 20)
         )
    return(p)
  }
  
  # Try to safely extract project data
  project_data <- tryCatch({
    # Extract metadata from the BIDS project
    data <- prepare_bids_data_for_plot(x, include_derivatives)
    
    if (debug) {
      cat("Project data prepared successfully\n")
      cat("Raw data rows:", nrow(data$raw_data), "\n")
    }
    
    data
  }, error = function(e) {
    if (debug) cat("Error preparing project data:", e$message, "\n")
    NULL
  })
  
  # If data preparation failed, create a very simple plot from the raw data
  if (is.null(project_data)) {
    if (debug) cat("Using raw data directly for plotting\n")
    
    # Use the raw data table directly
    raw_data <- x$tbl
    
    # Create a very simple plot
    if (nrow(raw_data) > 0) {
      # Ensure we have subject ID and some kind of type
      if (!"subid" %in% names(raw_data)) {
        raw_data$subid <- "subject"
      }
      
      if (!"type" %in% names(raw_data) && "folder" %in% names(raw_data)) {
        raw_data$type <- raw_data$folder
      } else if (!"type" %in% names(raw_data)) {
        raw_data$type <- "data"
      }
      
      # Create a basic count plot
      p <- ggplot2::ggplot(raw_data, ggplot2::aes(x = subid, fill = type)) +
           ggplot2::geom_bar() +
           ggplot2::scale_fill_viridis_d(option = color_scheme) +
           ggplot2::labs(
             title = paste0("BIDS Dataset: ", x$name),
             subtitle = "Basic Overview (Limited Data)",
             x = "Subject",
             y = "File Count"
           ) +
           ggplot2::theme_minimal() +
           ggplot2::theme(
             axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
             plot.background = ggplot2::element_rect(fill = "white")
           )
      
      if (interactive) {
        tryCatch({
          p <- plotly::ggplotly(p)
        }, error = function(e) {
          if (debug) cat("Error making plot interactive:", e$message, "\n")
        })
      }
      
      return(p)
    } else {
      # No data at all
      p <- ggplot2::ggplot() + 
           ggplot2::annotate("text", x = 0.5, y = 0.5, 
                    label = "No data available in BIDS project") +
           ggplot2::theme_void() +
           ggplot2::theme(
             plot.background = ggplot2::element_rect(fill = "white"),
             plot.margin = ggplot2::margin(20, 20, 20, 20)
           )
      return(p)
    }
  }
  
  # Check if the data is too minimal to create complex plots
  if (nrow(project_data$raw_data) < 3) {
    if (debug) cat("Data is minimal, creating a simple fallback plot\n")
    
    # Create a simple fallback plot
    plot_data <- project_data$raw_data
    
    # Ensure required columns exist
    if (!"type" %in% names(plot_data)) {
      if ("folder" %in% names(plot_data)) {
        plot_data$type <- plot_data$folder
      } else {
        plot_data$type <- "data"
      }
    }
    
    if (!"subid" %in% names(plot_data)) {
      plot_data$subid <- "subject"
    }
    
    # If task column exists, add it to the labels
    if ("task" %in% names(plot_data)) {
      plot_data$label <- paste0(plot_data$type, "\n(", plot_data$task, ")")
    } else {
      plot_data$label <- plot_data$type
    }
    
    # If file_size doesn't exist, create it
    if (!"file_size" %in% names(plot_data)) {
      plot_data$file_size <- 1e6  # Default 1MB
    }
    
    # Create a visually distinct fallback plot
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = subid, y = label, fill = file_size)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_viridis_c(option = color_scheme, name = "File Size", 
                           trans = "log10", labels = scales::label_bytes(units = "MB")) +
      ggplot2::geom_text(ggplot2::aes(label = scales::label_bytes(units = "MB")(file_size)), 
                          color = "white", size = 3.5) +
      ggplot2::labs(
        title = paste0("BIDS Dataset: ", x$name),
        subtitle = "Simple Overview (Limited Data)",
        x = "Subject",
        y = "Data Type"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white")
      )
    
    if (interactive) {
      tryCatch({
        p <- plotly::ggplotly(p) %>%
          plotly::layout(hovermode = "closest")
      }, error = function(e) {
        if (debug) cat("Error making plot interactive:", e$message, "\n")
      })
    }
    
    return(p)
  }
  
  # Create plots based on the selected visualization mode
  if (visualization_mode == "heatmap") {
    # Create just the heatmap visualization
    tryCatch({
      p <- bids_heatmap(project_data, color_scheme, highlight_missing)
      
      if (interactive) {
        p <- plotly::ggplotly(p) %>%
          plotly::layout(hovermode = "closest")
      }
      
      return(p)
    }, error = function(e) {
      warning("Error creating heatmap visualization: ", e$message)
      # Return an empty plot with error message
      return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0, y = 0, 
                     label = paste("Error creating heatmap:", e$message)) +
             ggplot2::theme_void())
    })
  } else if (visualization_mode == "complete") {
    # Create a complete set of visualizations including the heatmap
    plots <- list()
    
    # Safely create each plot component
    tryCatch({
      # 1. Create the advanced heatmap
      plots$heatmap <- bids_heatmap(project_data, color_scheme, highlight_missing)
      if (debug) cat("Created heatmap plot\n")
      
      # 2. Create the data completeness heatmap
      plots$completeness <- plot_bids_completeness(project_data, color_scheme)
      if (debug) cat("Created completeness plot\n")
      
      # 3. Create file size distribution
      plots$file_sizes <- plot_bids_file_sizes(project_data, color_scheme, file_size_scale)
      if (debug) cat("Created file sizes plot\n")
      
      # 4. Create task distribution plot
      plots$tasks <- plot_bids_tasks(project_data, color_scheme)
      if (debug) cat("Created tasks plot\n")
      
      # 5. Create structure overview
      plots$structure <- plot_bids_structure(project_data, color_scheme)
      if (debug) cat("Created structure plot\n")
      
      # Check if any plot is NULL or missing
      missing_plots <- names(plots)[sapply(plots, is.null)]
      if (length(missing_plots) > 0) {
        warning("Some plots could not be created: ", paste(missing_plots, collapse=", "))
        # Replace any NULL plots with empty plots
        for (name in missing_plots) {
          plots[[name]] <- ggplot2::ggplot() + 
                          ggplot2::annotate("text", x = 0, y = 0, label = paste("No data for", name)) +
                          ggplot2::theme_void()
        }
      }
      
      # Combine plots using patchwork
      combined_plot <- (plots$heatmap) / 
                        (plots$completeness + plots$file_sizes) / 
                        (plots$tasks + plots$structure) +
                        patchwork::plot_annotation(
                          title = paste0("BIDS Dataset Overview: ", x$name),
                          subtitle = paste0(
                            length(participants(x)), " subjects, ", 
                            ifelse(!is.null(x$sessions) && length(x$sessions) > 0, 
                                  paste0(length(x$sessions), " sessions, "), ""),
                            length(tasks(x)), " tasks"),
                          tag_levels = "A"
                        ) +
                        patchwork::plot_layout(heights = c(1.5, 1, 1))
      
    }, error = function(e) {
      warning("Error creating complete visualization: ", e$message)
      # Return an empty plot with error message
      return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0, y = 0, 
                              label = paste("Error creating visualization:", e$message)) +
             ggplot2::theme_void())
    })
  } else {
    # Default "standard" mode
    plots <- list()
    
    # Safely create each plot component
    tryCatch({
      # 1. Create the data completeness heatmap
      plots$completeness <- plot_bids_completeness(project_data, color_scheme)
      if (debug) cat("Created completeness plot\n")
      
      # 2. Create file size distribution
      plots$file_sizes <- plot_bids_file_sizes(project_data, color_scheme, file_size_scale)
      if (debug) cat("Created file sizes plot\n")
      
      # 3. Create task distribution plot
      plots$tasks <- plot_bids_tasks(project_data, color_scheme)
      if (debug) cat("Created tasks plot\n")
      
      # 4. Create structure overview
      plots$structure <- plot_bids_structure(project_data, color_scheme)
      if (debug) cat("Created structure plot\n")
      
      # Check if any plot is NULL or missing
      missing_plots <- names(plots)[sapply(plots, is.null)]
      if (length(missing_plots) > 0) {
        warning("Some plots could not be created: ", paste(missing_plots, collapse=", "))
        # Replace any NULL plots with empty plots
        for (name in missing_plots) {
          plots[[name]] <- ggplot2::ggplot() + 
                          ggplot2::annotate("text", x = 0, y = 0, label = paste("No data for", name)) +
                          ggplot2::theme_void()
        }
      }
      
      # Combine plots using patchwork
      combined_plot <- (plots$completeness + plots$file_sizes) / 
                        (plots$tasks + plots$structure) +
                        patchwork::plot_annotation(
                          title = paste0("BIDS Dataset Overview: ", x$name),
                          subtitle = paste0(
                            length(participants(x)), " subjects, ", 
                            ifelse(!is.null(x$sessions) && length(x$sessions) > 0, 
                                  paste0(length(x$sessions), " sessions, "), ""),
                            length(tasks(x)), " tasks"),
                          tag_levels = "A"
                        )
    }, error = function(e) {
      warning("Error creating standard visualization: ", e$message)
      # Return an empty plot with error message
      return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0, y = 0, 
                              label = paste("Error creating visualization:", e$message)) +
             ggplot2::theme_void())
    })
  }
  
  # Convert to interactive if requested
  if (interactive) {
    tryCatch({
      p <- plotly::ggplotly(combined_plot)
      # Add custom hover information and interactivity features
      p <- plotly::layout(p, 
                 hovermode = "closest",
                 updatemenus = list(
                   list(
                     buttons = list(
                       list(method = "relayout",
                            args = list("showlegend", TRUE),
                            label = "Show Legend"),
                       list(method = "relayout",
                            args = list("showlegend", FALSE),
                            label = "Hide Legend")
                     ),
                     type = "buttons",
                     direction = "right",
                     xanchor = "center",
                     yanchor = "top",
                     x = 0.5,
                     y = 1.2
                   )
                 )
               )
      if (debug) cat("Converted to interactive plot successfully\n")
      return(p)
    }, error = function(e) {
      warning("Error converting to interactive plot: ", e$message, 
              ". Returning static plot instead.")
      return(combined_plot)
    })
  } else {
    return(combined_plot)
  }
}

#' Prepare BIDS data for visualization
#'
#' This internal function processes a bids_project object and extracts the necessary
#' data for visualization, including project info and formatted data.
#'
#' @param x A bids_project object
#' @param include_derivatives Logical. Whether to include derivatives data
#' @return A list containing project info and formatted data
#' @keywords internal
prepare_bids_data_for_plot <- function(x, include_derivatives = TRUE) {
  if (!inherits(x, "bids_project") && !inherits(x, "mock_bids_project")) {
    stop("Input must be a bids_project or mock_bids_project object")
  }
  
  # Extract project information
  project_info <- list(
    name = x$name,
    root = x$root,
    subjects = x$subjects,
    sessions = x$sessions,
    tasks = x$tasks,
    runs = x$runs,
    is_virtual = isTRUE(x$is_virtual)
  )
  
  # Process the raw data table
  raw_data <- x$tbl
  
  # Check if file_size column exists, and add if not
  if (!"file_size" %in% names(raw_data)) {
    if (isTRUE(project_info$is_virtual)) {
      # For virtual projects, generate random file sizes
      raw_data$file_size <- runif(nrow(raw_data), min = 1e6, max = 100e6)
    } else {
      # For real projects, get file sizes when possible
      raw_data$file_size <- NA_real_
      
      if ("path" %in% names(raw_data)) {
        # Use vectorized file.size for performance
        existing <- !is.na(raw_data$path) & file.exists(raw_data$path)
        file_sizes <- rep(NA_real_, length(raw_data$path))
        file_sizes[existing] <- file.size(raw_data$path[existing])
        raw_data$file_size <- file_sizes
      }
    }
  }
  
  # Filter out derivatives if not requested
  if (!include_derivatives) {
    if ("derivative" %in% names(raw_data)) {
      raw_data <- dplyr::filter(raw_data, !derivative)
    }
  }
  
  # Replace any NAs in the file_size with a reasonable default
  raw_data$file_size <- ifelse(is.na(raw_data$file_size), 1e6, raw_data$file_size)
  
  return(list(
    project_info = project_info,
    raw_data = raw_data
  ))
}

#' Plot BIDS data completeness
#'
#' Creates a heatmap showing data completeness across subjects and tasks.
#'
#' @param data Preprocessed BIDS data from prepare_bids_data_for_plot
#' @param color_scheme Color scheme to use
#' @return A ggplot object
#' @keywords internal
plot_bids_completeness <- function(data, color_scheme = "viridis") {
  # Extract data
  raw_data <- data$raw_data

  if (!"type" %in% names(raw_data)) {
    stop("missing 'type' column")
  }

  # Determine if we need to include sessions
  has_sessions <- !is.null(data$project_info$sessions) && length(data$project_info$sessions) > 0

  if (has_sessions && !"session" %in% names(raw_data)) {
    stop("missing 'session' column")
  }
  
  # Prepare data for completeness heatmap
  if (has_sessions) {
    # With sessions: subject x session x task
    if (length(data$project_info$tasks) > 0) {
      if (!"task" %in% names(raw_data)) {
        stop("missing 'task' column")
      }
      completeness_data <- raw_data %>%
        filter(!is.na(subid), !is.na(session), !is.na(task)) %>%
        group_by(subid, session, task) %>%
        summarize(
          has_data = n() > 0,
          file_count = n(),
          total_size = sum(file_size, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Plot
      p <- ggplot(completeness_data, aes(x = subid, y = interaction(session, task), 
                                         fill = total_size)) +
        geom_tile(color = "white", linewidth = 0.2) +
        scale_fill_viridis_c(option = color_scheme, name = "Data Size (bytes)", 
                             trans = "log10", na.value = "grey90") +
        labs(
          title = "Data Completeness by Subject, Session, and Task",
          x = "Subject ID",
          y = "Session x Task",
          fill = "Data Size"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
    } else {
      # With sessions but no tasks: subject x session x type
      completeness_data <- raw_data %>%
        filter(!is.na(subid), !is.na(session), !is.na(type)) %>%
        group_by(subid, session, type) %>%
        summarize(
          has_data = n() > 0,
          file_count = n(),
          total_size = sum(file_size, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Plot
      p <- ggplot(completeness_data, aes(x = subid, y = interaction(session, type),
                                         fill = total_size)) +
        geom_tile(color = "white", linewidth = 0.2) +
        scale_fill_viridis_c(option = color_scheme, name = "Data Size (bytes)",
                             trans = "log10", na.value = "grey90") +
        labs(
          title = "Data Completeness by Subject, Session, and Type",
          x = "Subject ID",
          y = "Session x Type",
          fill = "Data Size"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
    }
  } else {
    # No sessions: subject x task/type
    if (length(data$project_info$tasks) > 0) {
      if (!"task" %in% names(raw_data)) {
        stop("missing 'task' column")
      }
      completeness_data <- raw_data %>%
        filter(!is.na(subid), !is.na(task)) %>%
        group_by(subid, task) %>%
        summarize(
          has_data = n() > 0,
          file_count = n(),
          total_size = sum(file_size, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Plot
      p <- ggplot(completeness_data, aes(x = subid, y = task, fill = total_size)) +
        geom_tile(color = "white", linewidth = 0.2) +
        scale_fill_viridis_c(option = color_scheme, name = "Data Size (bytes)",
                             trans = "log10", na.value = "grey90") +
        labs(
          title = "Data Completeness by Subject and Task",
          x = "Subject ID",
          y = "Task",
          fill = "Data Size"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
    } else {
      # No sessions and no tasks: subject x type
      completeness_data <- raw_data %>%
        filter(!is.na(subid), !is.na(type)) %>%
        group_by(subid, type) %>%
        summarize(
          has_data = n() > 0,
          file_count = n(),
          total_size = sum(file_size, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Plot
      p <- ggplot(completeness_data, aes(x = subid, y = type, fill = total_size)) +
        geom_tile(color = "white", linewidth = 0.2) +
        scale_fill_viridis_c(option = color_scheme, name = "Data Size (bytes)",
                             trans = "log10", na.value = "grey90") +
        labs(
          title = "Data Completeness by Subject and Type",
          x = "Subject ID",
          y = "Data Type",
          fill = "Data Size"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
    }
  }
  
  return(p)
}

#' Plot BIDS file size distribution
#'
#' Creates a plot showing the distribution of file sizes across types.
#'
#' @param data Preprocessed BIDS data from prepare_bids_data_for_plot
#' @param color_scheme Color scheme to use
#' @param scale Scale to use for file sizes ("log", "sqrt", or "linear")
#' @return A ggplot object
#' @keywords internal
plot_bids_file_sizes <- function(data, color_scheme = "viridis", scale = "log") {
  # Extract data
  raw_data <- data$raw_data

  if (!"type" %in% names(raw_data)) {
    stop("missing 'type' column")
  }
  
  # Create a boxplot of file sizes by type
  p <- ggplot(raw_data, aes(x = type, y = file_size, fill = type)) +
    geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 1.5) +
    scale_fill_viridis_d(option = color_scheme) +
    labs(
      title = "File Size Distribution by Type",
      x = "Data Type",
      y = "File Size (bytes)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  # Apply scale transformation
  if (scale == "log") {
    p <- p + scale_y_log10()
  } else if (scale == "sqrt") {
    p <- p + scale_y_sqrt()
  }
  
  return(p)
}

#' Plot BIDS task distribution
#'
#' Creates a plot showing the distribution of tasks across subjects.
#'
#' @param data Preprocessed BIDS data from prepare_bids_data_for_plot
#' @param color_scheme Color scheme to use
#' @return A ggplot object
#' @keywords internal
plot_bids_tasks <- function(data, color_scheme = "viridis") {
  # Extract data
  raw_data <- data$raw_data

  if (!"type" %in% names(raw_data)) {
    stop("missing 'type' column")
  }
  if (!"task" %in% names(raw_data)) {
    stop("missing 'task' column")
  }
  
  # Check if we have tasks
  if (length(data$project_info$tasks) == 0 || 
      !("task" %in% names(raw_data)) || 
      all(is.na(raw_data$task))) {
    # No tasks, create a plot showing data types instead
    # Check if type_summary exists, if not, create it
    if (is.null(data$type_summary)) {
      # Create type summary from raw data
      type_summary <- raw_data %>%
        dplyr::filter(!is.na(type)) %>%
        dplyr::group_by(type) %>%
        dplyr::summarize(total_files = dplyr::n(), .groups = "drop")
    } else {
      type_summary <- data$type_summary
    }
    
    # If there are no valid types, create a placeholder plot
    if (nrow(type_summary) == 0) {
      p <- ggplot2::ggplot() + 
           ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No task or type data available") +
           ggplot2::theme_void() +
           ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
      return(p)
    }
    
    p <- ggplot2::ggplot(type_summary, ggplot2::aes(x = reorder(type, total_files), y = total_files, fill = type)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_viridis_d(option = color_scheme) +
      ggplot2::labs(
        title = "File Count by Data Type",
        x = "Data Type",
        y = "Number of Files"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
  } else {
    # With tasks, create a count plot of runs by task
    # Check if run column exists
    if (!"run" %in% names(raw_data)) {
      # Create a simplified plot without run information
      task_summary <- raw_data %>%
        dplyr::filter(!is.na(task)) %>%
        dplyr::group_by(subid, task) %>%
        dplyr::summarize(count = dplyr::n(), .groups = "drop")
      
      p <- ggplot2::ggplot(task_summary, ggplot2::aes(x = task, y = count, fill = task)) +
        ggplot2::geom_boxplot(alpha = 0.8) +
        ggplot2::geom_jitter(alpha = 0.5, width = 0.2, height = 0) +
        ggplot2::scale_fill_viridis_d(option = color_scheme) +
        ggplot2::labs(
          title = "File Count by Task",
          x = "Task",
          y = "Number of Files"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )
    } else {
      task_summary <- raw_data %>%
        dplyr::filter(!is.na(task)) %>%
        dplyr::group_by(subid, task) %>%
        dplyr::summarize(runs = dplyr::n_distinct(run), .groups = "drop")
      
      p <- ggplot2::ggplot(task_summary, ggplot2::aes(x = task, y = runs, fill = task)) +
        ggplot2::geom_boxplot(alpha = 0.8) +
        ggplot2::geom_jitter(alpha = 0.5, width = 0.2, height = 0) +
        ggplot2::scale_fill_viridis_d(option = color_scheme) +
        ggplot2::labs(
          title = "Run Distribution by Task",
          x = "Task",
          y = "Number of Runs"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )
    }
  }
  
  return(p)
}

#' Plot BIDS structure overview
#'
#' Creates a hierarchical visualization of the BIDS structure.
#'
#' @param data Preprocessed BIDS data from prepare_bids_data_for_plot
#' @param color_scheme Color scheme to use
#' @return A ggplot object
#' @keywords internal
#' @noRd
plot_bids_structure <- function(data, color_scheme = "viridis") {
  # Extract data
  raw_data <- data$raw_data

  if (!"type" %in% names(raw_data)) {
    stop("missing 'type' column")
  }
  
  # Create a tree-like structure representation
  # This is a simplified version that shows the proportion of different data types
  
  # Count files by type and subject
  structure_data <- raw_data %>%
    filter(!is.na(type)) %>%
    group_by(subid, type) %>%
    summarize(count = n(), .groups = "drop") %>%
    group_by(subid) %>%
    mutate(proportion = count / sum(count)) %>%
    ungroup()
  
  # Order subjects by total file count
  subject_order <- raw_data %>%
    group_by(subid) %>%
    summarize(total = n(), .groups = "drop") %>%
    arrange(desc(total)) %>%
    pull(subid)
  
  structure_data$subid <- factor(structure_data$subid, levels = subject_order)
  
  # Create the plot
  p <- ggplot(structure_data, aes(x = subid, y = proportion, fill = type)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_viridis_d(option = color_scheme, name = "Data Type") +
    labs(
      title = "Proportion of Data Types by Subject",
      x = "Subject ID",
      y = "Proportion of Files"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

#' Create a specialized heatmap visualization of BIDS data
#'
#' This function creates a heatmap visualization of a BIDS project, where the x-axis represents
#' subjects and the y-axis represents tasks by run. Each cell in the heatmap is colored by
#' file size, providing an intuitive view of data completeness and size distribution across
#' the project. This is particularly useful for quality control and identifying missing data.
#'
#' @param x A \code{bids_project} object
#' @param interactive Logical. Whether to create an interactive plot (default TRUE)
#' @param color_scheme Character. Name of the color palette to use (default "viridis")
#' @param file_type Character. Type of files to visualize (default "func")
#' @param highlight_missing Logical. Whether to highlight missing data points (default TRUE)
#' @param text_size Numeric. Size of text labels (default 2.5)
#' @param rotate_labels Logical. Whether to rotate the axis labels (default TRUE)
#' @return A plot object (ggplot2 or plotly depending on interactive parameter)
#' @examples
#' \donttest{
#' # Create a basic interactive heatmap for a BIDS dataset
#' proj <- bids_project(system.file("extdata/ds001", package="bidser"))
#' bids_heatmap(proj)
#'
#' # Create a static heatmap with custom settings
#' bids_heatmap(proj, 
#'              interactive = FALSE,
#'              color_scheme = "plasma",
#'              text_size = 3,
#'              rotate_labels = FALSE)
#'
#' # Visualize anatomical data with missing data highlighted
#' bids_heatmap(proj, 
#'              file_type = "anat",
#'              highlight_missing = TRUE,
#'              color_scheme = "magma")
#'
#' # Create a compact visualization for a large dataset
#' ds007 <- bids_project(system.file("extdata/ds007", package="bidser"))
#' bids_heatmap(ds007, 
#'              text_size = 2,
#'              rotate_labels = TRUE,
#'              highlight_missing = FALSE)
#' }
#' @export
bids_heatmap <- function(x, interactive=TRUE, color_scheme="viridis", file_type="func",
                        highlight_missing=TRUE, text_size=2.5, rotate_labels=TRUE) {
  
  # Check input - accept both bids_project and mock_bids_project
  if (!inherits(x, "bids_project") && !inherits(x, "mock_bids_project")) {
    stop("Input must be a bids_project or mock_bids_project object")
  }
  
  # Prepare data
  project_data <- prepare_bids_data_for_plot(x, include_derivatives = TRUE)
  raw_data <- project_data$raw_data

  if (!"type" %in% names(raw_data)) {
    stop("missing 'type' column")
  }
  
  # Filter by file type
  if (!file_type %in% unique(raw_data$type)) {
    stop(paste0("File type '", file_type, "' not found in dataset. Available types: ", 
                paste(unique(raw_data$type), collapse = ", ")))
  }
  
  filtered_data <- raw_data %>%
    dplyr::filter(type == file_type)

  # Determine if we have sessions
  has_sessions <- !is.null(project_data$project_info$sessions) &&
                 length(project_data$project_info$sessions) > 0

  if (has_sessions && !"session" %in% names(raw_data)) {
    stop("missing 'session' column")
  }
  
  # Create different visualizations based on file type
  if (file_type == "func") {
    # For functional data, use task and run
    if (!"task" %in% names(raw_data)) {
      stop("missing 'task' column")
    }
    if (!"run" %in% names(raw_data)) {
      stop("missing 'run' column")
    }
    
    # Create a task-run combination for y-axis
    if (has_sessions) {
      # With sessions
      heatmap_data <- filtered_data %>%
        dplyr::filter(!is.na(subid), !is.na(task), !is.na(run)) %>%
        dplyr::group_by(subid, session, task, run) %>%
        dplyr::summarize(
          file_size = sum(file_size, na.rm = TRUE),
          file_count = dplyr::n(),
          .groups = "drop"
        ) %>%
        # Create a combined task-run label for the y-axis
        dplyr::mutate(task_run = paste0(task, "-run", run))
      
      # Create a complete grid to detect missing data
      if (highlight_missing) {
        all_subj <- unique(heatmap_data$subid)
        all_sess <- unique(heatmap_data$session)
        all_task_runs <- unique(heatmap_data$task_run)
        
        expected_grid <- expand.grid(
          subid = all_subj,
          session = all_sess,
          task_run = all_task_runs,
          stringsAsFactors = FALSE
        )
        
        # Merge to find missing combinations
        heatmap_data <- dplyr::left_join(expected_grid, heatmap_data, 
                                by = c("subid", "session", "task_run")) %>%
          dplyr::mutate(
            file_size = ifelse(is.na(file_size), 0, file_size),
            file_count = ifelse(is.na(file_count), 0, file_count),
            missing = file_count == 0
          )
      }
      
      # Create the plot - THIS IS THE MAIN VISUALIZATION
      # x-axis: subject, y-axis: task/run, facet by session, color by file size
      p <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = subid, y = task_run, fill = file_size)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.2, 
                 ggplot2::aes(alpha = ifelse(file_count == 0, 0.3, 1))) +
        ggplot2::scale_fill_viridis_c(option = color_scheme, name = "File Size",
                            trans = "log10", na.value = "grey90",
                            labels = scales::label_bytes(units = "MB")) +
        ggplot2::scale_alpha_identity() +
        ggplot2::facet_wrap(~ session, scales = "free_x", ncol = 1) +
        ggplot2::labs(
          title = paste0("BIDS Functional Data: ", x$name),
          subtitle = "Subject x Task-Run Heatmap",
          x = "Subject ID",
          y = "Task and Run"
        )
      
      # Add file size labels
      if (text_size > 0) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = ifelse(file_size > 0, 
                            paste0(round(file_size / 1e6, 1), "MB"), 
                            "")),
          size = text_size, color = "white"
        )
      }
    } else {
      # No sessions
      heatmap_data <- filtered_data %>%
        dplyr::filter(!is.na(subid), !is.na(task), !is.na(run)) %>%
        dplyr::group_by(subid, task, run) %>%
        dplyr::summarize(
          file_size = sum(file_size, na.rm = TRUE),
          file_count = dplyr::n(),
          .groups = "drop"
        ) %>%
        # Create a combined task-run label for the y-axis
        dplyr::mutate(task_run = paste0(task, "-run", run))
      
      # Create a complete grid to detect missing data
      if (highlight_missing) {
        all_subj <- unique(heatmap_data$subid)
        all_task_runs <- unique(heatmap_data$task_run)
        
        expected_grid <- expand.grid(
          subid = all_subj,
          task_run = all_task_runs,
          stringsAsFactors = FALSE
        )
        
        # Merge to find missing combinations
        heatmap_data <- dplyr::left_join(expected_grid, heatmap_data, by = c("subid", "task_run")) %>%
          dplyr::mutate(
            file_size = ifelse(is.na(file_size), 0, file_size),
            file_count = ifelse(is.na(file_count), 0, file_count),
            missing = file_count == 0
          )
      }
      
      # Create the plot - THIS IS THE MAIN VISUALIZATION
      # x-axis: subject, y-axis: task/run, color by file size
      p <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = subid, y = task_run, fill = file_size)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.2, 
                 ggplot2::aes(alpha = ifelse(file_count == 0, 0.3, 1))) +
        ggplot2::scale_fill_viridis_c(option = color_scheme, name = "File Size",
                            trans = "log10", na.value = "grey90",
                            labels = scales::label_bytes(units = "MB")) +
        ggplot2::scale_alpha_identity() +
        ggplot2::labs(
          title = paste0("BIDS Functional Data: ", x$name),
          subtitle = "Subject x Task-Run Heatmap",
          x = "Subject ID",
          y = "Task and Run"
        )
      
      # Add file size labels
      if (text_size > 0) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = ifelse(file_size > 0, 
                            paste0(round(file_size / 1e6, 1), "MB"), 
                            "")),
          size = text_size, color = "white"
        )
      }
    }
  } else if (file_type == "anat") {
    # For anatomical data, use anatomical modality (T1w, T2w, etc.)
    # x-axis: subject, y-axis: anatomical type, color by file size
    
    # Check if 'kind' column exists, if not use 'modality' or extract from filename
    if (!"kind" %in% names(filtered_data)) {
      if ("modality" %in% names(filtered_data) && !all(is.na(filtered_data$modality))) {
        filtered_data$kind <- filtered_data$modality
      } else {
        # Extract anatomical type from filename
        filtered_data$kind <- sapply(filtered_data$name, function(fname) {
          if (grepl("_T1w\\.", fname)) return("T1w")
          if (grepl("_T2w\\.", fname)) return("T2w")
          if (grepl("_inplaneT2\\.", fname)) return("inplaneT2")
          if (grepl("_FLAIR\\.", fname)) return("FLAIR")
          if (grepl("_PD\\.", fname)) return("PD")
          if (grepl("_PDT2\\.", fname)) return("PDT2")
          return("anatomical")
        })
      }
    }
    
    if (has_sessions) {
      # With sessions
      heatmap_data <- filtered_data %>%
        dplyr::filter(!is.na(subid)) %>%
        dplyr::group_by(subid, session, kind) %>%
        dplyr::summarize(
          file_size = sum(file_size, na.rm = TRUE),
          file_count = dplyr::n(),
          .groups = "drop"
        )
      
      # Create a complete grid for missing data
      if (highlight_missing) {
        all_subj <- unique(heatmap_data$subid)
        all_sess <- unique(heatmap_data$session)
        all_kinds <- unique(heatmap_data$kind)
        
        expected_grid <- expand.grid(
          subid = all_subj,
          session = all_sess,
          kind = all_kinds,
          stringsAsFactors = FALSE
        )
        
        # Merge to find missing combinations
        heatmap_data <- dplyr::left_join(expected_grid, heatmap_data, 
                                by = c("subid", "session", "kind")) %>%
          dplyr::mutate(
            file_size = ifelse(is.na(file_size), 1e3, file_size),  # Use small positive value for log safety
            file_count = ifelse(is.na(file_count), 0, file_count),
            missing = file_count == 0
          )
      }
      
      # Create the plot
      p <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = subid, y = kind, fill = file_size)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.2, 
                 ggplot2::aes(alpha = ifelse(file_count == 0, 0.3, 1))) +
        ggplot2::scale_fill_viridis_c(option = color_scheme, name = "File Size",
                            trans = "log10", na.value = "grey90",
                            labels = scales::label_bytes(units = "MB")) +
        ggplot2::scale_alpha_identity() +
        ggplot2::facet_wrap(~ session, scales = "free_x", ncol = 1) +
        ggplot2::labs(
          title = paste0("BIDS Anatomical Data: ", x$name),
          subtitle = "Subject x Image Type Heatmap",
          x = "Subject ID",
          y = "Anatomical Type"
        )
      
      # Add file size labels
      if (text_size > 0) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = ifelse(file_size > 0, 
                            paste0(round(file_size / 1e6, 1), "MB"), 
                            "")),
          size = text_size, color = "white"
        )
      }
    } else {
      # No sessions
      heatmap_data <- filtered_data %>%
        dplyr::filter(!is.na(subid)) %>%
        dplyr::group_by(subid, kind) %>%
        dplyr::summarize(
          file_size = sum(file_size, na.rm = TRUE),
          file_count = dplyr::n(),
          .groups = "drop"
        )
      
      # Create a complete grid for missing data
      if (highlight_missing) {
        all_subj <- unique(heatmap_data$subid)
        all_kinds <- unique(heatmap_data$kind)
        
        expected_grid <- expand.grid(
          subid = all_subj,
          kind = all_kinds,
          stringsAsFactors = FALSE
        )
        
        # Merge to find missing combinations
        heatmap_data <- dplyr::left_join(expected_grid, heatmap_data, 
                                by = c("subid", "kind")) %>%
          dplyr::mutate(
            file_size = ifelse(is.na(file_size), 1e3, file_size),  # Use small positive value for log safety
            file_count = ifelse(is.na(file_count), 0, file_count),
            missing = file_count == 0
          )
      }
      
      # Create the plot
      p <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = subid, y = kind, fill = file_size)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.2, 
                 ggplot2::aes(alpha = ifelse(file_count == 0, 0.3, 1))) +
        ggplot2::scale_fill_viridis_c(option = color_scheme, name = "File Size",
                            trans = "log10", na.value = "grey90",
                            labels = scales::label_bytes(units = "MB")) +
        ggplot2::scale_alpha_identity() +
        ggplot2::labs(
          title = paste0("BIDS Anatomical Data: ", x$name),
          subtitle = "Subject x Image Type Heatmap",
          x = "Subject ID",
          y = "Anatomical Type"
        )
      
      # Add file size labels
      if (text_size > 0) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = ifelse(file_size > 0, 
                            paste0(round(file_size / 1e6, 1), "MB"), 
                            "")),
          size = text_size, color = "white"
        )
      }
    }
  } else {
    # For other file types, just use the file type as the y-axis
    heatmap_data <- filtered_data %>%
      dplyr::filter(!is.na(subid)) %>%
      dplyr::group_by(subid) %>%
      dplyr::summarize(
        file_size = sum(file_size, na.rm = TRUE),
        file_count = dplyr::n(),
        .groups = "drop"
      )
    
    # Create the plot
    p <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = subid, y = 1, fill = file_size)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.2) +
      ggplot2::scale_fill_viridis_c(option = color_scheme, name = "File Size",
                          trans = "log10", na.value = "grey90",
                          labels = scales::label_bytes(units = "MB")) +
      ggplot2::labs(
        title = paste0("BIDS ", file_type, " Data: ", x$name),
        subtitle = "Subject Heatmap",
        x = "Subject ID",
        y = ""
      ) +
      ggplot2::scale_y_continuous(breaks = NULL)
    
    # Add file size labels
    if (text_size > 0) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = paste0(round(file_size / 1e6, 1), "MB")),
        size = text_size, color = "white"
      )
    }
  }
  
  # Apply common theme settings
  p <- p + ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = if(rotate_labels) 45 else 0, hjust = 1),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right"
    )
  
  # Convert to interactive if requested
  if (interactive) {
    p <- plotly::ggplotly(p) %>%
      plotly::layout(
        hovermode = "closest",
        hoverlabel = list(bgcolor = "white"),
        title = list(
          text = paste0("BIDS ", file_type, " Data: ", x$name,
                       "<br><sup>Subject x ", 
                       if(file_type == "func") "Task-Run" else "Image Type", 
                       " Heatmap</sup>")
        )
      )
  }
  
  return(p)
}

#' Plot BIDS data as a heatmap
#'
#' @param data Preprocessed BIDS data from prepare_bids_data_for_plot
#' @param color_scheme Color scheme to use
#' @param highlight_missing Whether to highlight missing data
#' @return A ggplot object
#' @keywords internal
plot_bids_heatmap <- function(data, color_scheme = "viridis", highlight_missing = TRUE) {
  raw_data <- data$raw_data

  # Ensure required columns exist
  if (!"subid" %in% names(raw_data)) {
    raw_data$subid <- "subject"
  }

  if (!"type" %in% names(raw_data)) {
    if ("folder" %in% names(raw_data)) {
      raw_data$type <- raw_data$folder
    } else {
      raw_data$type <- "data"
    }
  }

  # Combine type and task for the y-axis when tasks exist
  if ("task" %in% names(raw_data)) {
    raw_data$label <- paste0(raw_data$type,
                             ifelse(is.na(raw_data$task), "", paste0("\n(", raw_data$task, ")")))
  } else {
    raw_data$label <- raw_data$type
  }

  if (!"file_size" %in% names(raw_data)) {
    raw_data$file_size <- 1e6
  }

  heatmap_data <- raw_data %>%
    dplyr::group_by(subid, label) %>%
    dplyr::summarize(file_size = sum(file_size, na.rm = TRUE),
                     file_count = dplyr::n(), .groups = "drop")

  # Fill in missing combinations to highlight absence of data
  if (highlight_missing) {
    exp_grid <- expand.grid(
      subid = unique(raw_data$subid),
      label = unique(raw_data$label),
      stringsAsFactors = FALSE
    )

    heatmap_data <- dplyr::left_join(exp_grid, heatmap_data, by = c("subid", "label")) %>%
      dplyr::mutate(
        file_size = ifelse(is.na(file_size), 0, file_size),
        file_count = ifelse(is.na(file_count), 0, file_count),
        missing = file_count == 0
      )
  } else {
    heatmap_data$missing <- FALSE
  }

  heatmap_data$label <- factor(heatmap_data$label, levels = sort(unique(heatmap_data$label)))
  heatmap_data$subid <- factor(heatmap_data$subid, levels = sort(unique(heatmap_data$subid)))

  p <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = subid, y = label, fill = file_size)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2,
                       ggplot2::aes(alpha = ifelse(missing, 0.3, 1))) +
    ggplot2::scale_fill_viridis_c(option = color_scheme, name = "File Size",
                                  trans = "log10", na.value = "grey90",
                                  labels = scales::label_bytes(units = "MB")) +
    ggplot2::scale_alpha_identity() +
    ggplot2::labs(
      title = "BIDS Dataset Heatmap",
      x = "Subject ID",
      y = "Data Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}

#' Test the bids_heatmap visualization with example data
#'
#' This function creates a virtual BIDS project and generates a heatmap visualization
#' using the bids_heatmap function. It is useful for demonstration and testing purposes.
#'
#' @return A plotly or ggplot object
#' @keywords internal
#' @noRd
test_bids_heatmap <- function() {
  # First try to use a real dataset if it exists
  example_path <- system.file("extdata/ds001", package = "bidser")
  
  if (dir.exists(example_path) && length(list.files(example_path)) > 0) {
    message("Using real dataset from package: ", example_path)
    x <- bidser::bids_project(example_path)
  } else {
    message("Real dataset not found, creating virtual project")
    x <- create_virtual_bids_project(
      name = "Test Project",
      subjects = paste0("sub-", sprintf("%02d", 1:10)),
      sessions = c("ses-01", "ses-02"),
      tasks = c("rest", "task1", "task2"),
      runs = c("01", "02"),
      modalities = c("T1w", "T2w", "bold"),
      derivatives = TRUE
    )
  }
  
  # Return heatmap visualization
  bids_heatmap(x, interactive = TRUE, file_type = "func")
}

#' Create a virtual BIDS project for testing
#'
#' This function creates a virtual BIDS project structure for testing purposes.
#' It simulates a project with specified subjects, sessions, tasks, runs, and modalities.
#'
#' @param name Character. Name of the project
#' @param subjects Character vector. Subject IDs (e.g., "sub-01")
#' @param sessions Character vector. Session IDs (e.g., "ses-01") 
#' @param tasks Character vector. Task names (e.g., "rest")
#' @param runs Character vector. Run numbers (e.g., "01")
#' @param modalities Character vector. Imaging modalities (e.g., "T1w", "bold")
#' @param derivatives Logical. Whether to include derivative files
#' @return A bids_project object
#' @keywords internal
#' @noRd
create_virtual_bids_project <- function(name = "Virtual Project",
                                        subjects = paste0("sub-", sprintf("%02d", 1:5)),
                                        sessions = NULL,
                                        tasks = c("rest"),
                                        runs = c("01"),
                                        modalities = c("T1w", "bold"),
                                        derivatives = FALSE) {
  
  # Create a mock BIDS structure using create_mock_bids
  warning("create_virtual_bids_project is deprecated. Use create_mock_bids instead.")
  
  # Build file structure data frame
  file_structure <- data.frame()
  
  # Function to randomly skip some files to create an incomplete dataset
  should_include <- function() {
    # 80% chance of including a file
    runif(1) < 0.8
  }
  
  # Build the file structure data frame
  for (sub in subjects) {
    # Remove "sub-" prefix for the data frame
    sub_id <- gsub("^sub-", "", sub)
    
    if (!is.null(sessions) && length(sessions) > 0) {
      for (ses in sessions) {
        # Add anatomical files
        for (mod in modalities) {
          if (mod %in% c("T1w", "T2w") && should_include()) {
            file_structure <- rbind(file_structure, data.frame(
              subid = sub_id,
              session = ses,
              datatype = "anat",
              task = NA,
              run = NA,
              suffix = paste0(mod, ".nii.gz"),
              fmriprep = FALSE,
              stringsAsFactors = FALSE
            ))
          }
        }
        
        # Add functional files
        for (task in tasks) {
          for (run in runs) {
            if ("bold" %in% modalities && should_include()) {
              file_structure <- rbind(file_structure, data.frame(
                subid = sub_id,
                session = ses,
                datatype = "func",
                task = task,
                run = run,
                suffix = "bold.nii.gz",
                fmriprep = FALSE,
                stringsAsFactors = FALSE
              ))
            }
          }
        }
        
        # Add derivative files if requested
        if (derivatives) {
          for (task in tasks) {
            for (run in runs) {
              if (should_include()) {
                file_structure <- rbind(file_structure, data.frame(
                  subid = sub_id,
                  session = ses,
                  datatype = "func",
                  task = task,
                  run = run,
                  suffix = "bold.nii.gz",
                  fmriprep = TRUE,
                  space = "MNI152NLin2009cAsym",
                  desc = "preproc",
                  stringsAsFactors = FALSE
                ))
              }
            }
          }
        }
      }
    } else {
      # No sessions
      # Add anatomical files
      for (mod in modalities) {
        if (mod %in% c("T1w", "T2w") && should_include()) {
          file_structure <- rbind(file_structure, data.frame(
            subid = sub_id,
            session = NA,
            datatype = "anat",
            task = NA,
            run = NA,
            suffix = paste0(mod, ".nii.gz"),
            fmriprep = FALSE,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Add functional files
      for (task in tasks) {
        for (run in runs) {
          if ("bold" %in% modalities && should_include()) {
            file_structure <- rbind(file_structure, data.frame(
              subid = sub_id,
              session = NA,
              datatype = "func",
              task = task,
              run = run,
              suffix = "bold.nii.gz",
              fmriprep = FALSE,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      # Add derivative files if requested
      if (derivatives) {
        for (task in tasks) {
          for (run in runs) {
            if (should_include()) {
              file_structure <- rbind(file_structure, data.frame(
                subid = sub_id,
                session = NA,
                datatype = "func",
                task = task,
                run = run,
                suffix = "bold.nii.gz",
                fmriprep = TRUE,
                space = "MNI152NLin2009cAsym",
                desc = "preproc",
                stringsAsFactors = FALSE
              ))
            }
          }
        }
      }
    }
  }
  
  # Create participants data frame
  # Remove "sub-" prefix from subjects
  subject_ids <- gsub("^sub-", "", subjects)
  participants <- data.frame(
    participant_id = subjects,
    stringsAsFactors = FALSE
  )
  
  # Create the mock BIDS project using create_mock_bids
  mock_project <- bidser::create_mock_bids(
    project_name = name,
    participants = participants,
    file_structure = file_structure,
    prep_dir = if (derivatives) "derivatives/fmriprep" else NULL
  )
  
  # Copy some properties for compatibility with old code
  mock_project$is_virtual <- TRUE
  
  return(mock_project)
} 