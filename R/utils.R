# Utility Functions for Lead Surveillance Dashboard

library(dplyr)
library(scales)
library(ggplot2)
library(plotly)

# =============================================================================
# Color Palettes and Theming
# =============================================================================

#' Get color palette for lead level visualization
#' @param n Number of colors needed
#' @param type Type of palette: "sequential", "diverging", "categorical"
#' @return Vector of hex color codes
get_lead_palette <- function(n = 5, type = "sequential") {
  palettes <- list(
    sequential = c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15"),
    diverging = c("#2166ac", "#67a9cf", "#f7f7f7", "#ef8a62", "#b2182b"),
    categorical = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  )

  pal <- palettes[[type]]
  if (n > length(pal)) {
    colorRampPalette(pal)(n)
  } else {
    pal[1:n]
  }
}

#' Create consistent ggplot theme for dashboard
#' @return ggplot theme object
theme_lead_dashboard <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 11),
      axis.title = element_text(face = "bold", size = 11),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# =============================================================================
# Formatting Functions
# =============================================================================

#' Format rate for display
#' @param rate Numeric rate value
#' @param digits Number of decimal places
#' @return Formatted string
format_rate <- function(rate, digits = 1) {
  ifelse(
    is.na(rate),
    "N/A",
    paste0(format(round(rate, digits), nsmall = digits), "%")
  )
}

#' Format count for display (with thousands separator)
#' @param count Numeric count value
#' @return Formatted string
format_count <- function(count) {
  ifelse(
    is.na(count),
    "N/A",
    format(count, big.mark = ",", scientific = FALSE)
  )
}

#' Format threshold for display
#' @param threshold Numeric threshold value
#' @return Formatted string with units
format_threshold <- function(threshold) {
  paste0("≥", threshold, " μg/dL")
}

# =============================================================================
# Data Validation Functions
# =============================================================================
#' Validate data frame has required columns
#' @param df Data frame to validate
#' @param required_cols Vector of required column names
#' @return TRUE if valid, FALSE otherwise (with warning)
validate_columns <- function(df, required_cols) {
  if (is.null(df)) {
    warning("Data frame is NULL")
    return(FALSE)
  }

  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    warning("Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(FALSE)
  }

  TRUE
}

#' Check if data needs refresh based on modification time
#' @param file_path Path to data file
#' @param max_age_hours Maximum age in hours
#' @return TRUE if data needs refresh
needs_refresh <- function(file_path, max_age_hours = 24) {
  if (!file.exists(file_path)) return(TRUE)

  file_age <- difftime(Sys.time(), file.mtime(file_path), units = "hours")
  as.numeric(file_age) > max_age_hours
}

# =============================================================================
# Annotation Functions
# =============================================================================

#' Get policy change annotations for timeline plots
#' @return Data frame with policy change information
get_policy_annotations <- function() {
  data.frame(
    date = as.Date(c("2021-10-01", "2012-05-01")),
    year = c(2021, 2012),
    event = c(
      "CDC reference value changed: 5 → 3.5 μg/dL",
      "CDC adopted 5 μg/dL reference value"
    ),
    short_label = c(
      "3.5 μg/dL threshold",
      "5 μg/dL threshold"
    ),
    stringsAsFactors = FALSE
  )
}

#' Add vertical line annotations to a ggplot
#' @param p ggplot object
#' @param annotations Data frame from get_policy_annotations()
#' @param year_range Vector of c(min_year, max_year) to filter annotations
#' @return Modified ggplot object
add_policy_annotations <- function(p, annotations = NULL, year_range = NULL) {
  if (is.null(annotations)) {
    annotations <- get_policy_annotations()
  }

  if (!is.null(year_range)) {
    annotations <- annotations %>%
      filter(year >= year_range[1], year <= year_range[2])
  }

  if (nrow(annotations) == 0) return(p)

  p +
    geom_vline(
      data = annotations,
      aes(xintercept = year),
      linetype = "dashed",
      color = "gray50",
      alpha = 0.7
    ) +
    geom_text(
      data = annotations,
      aes(x = year, y = Inf, label = short_label),
      vjust = -0.5,
      hjust = 0.5,
      size = 3,
      color = "gray40",
      angle = 0
    )
}

# =============================================================================
# Summary Statistics Functions
# =============================================================================

#' Calculate summary statistics for a dataset
#' @param df Data frame with rate data
#' @param rate_col Name of the rate column
#' @param group_cols Optional grouping columns
#' @return Data frame with summary statistics
calculate_summary_stats <- function(df, rate_col = "elevated_rate", group_cols = NULL) {
  if (is.null(df) || !rate_col %in% names(df)) {
    return(NULL)
  }

  if (!is.null(group_cols)) {
    df <- df %>% group_by(across(all_of(group_cols)))
  }

  df %>%
    summarise(
      n = n(),
      mean_rate = mean(!!sym(rate_col), na.rm = TRUE),
      median_rate = median(!!sym(rate_col), na.rm = TRUE),
      min_rate = min(!!sym(rate_col), na.rm = TRUE),
      max_rate = max(!!sym(rate_col), na.rm = TRUE),
      sd_rate = sd(!!sym(rate_col), na.rm = TRUE),
      .groups = "drop"
    )
}

#' Calculate year-over-year change
#' @param df Data frame with year and rate columns
#' @param rate_col Name of the rate column
#' @param group_cols Columns to group by (e.g., geography)
#' @return Data frame with YoY change
calculate_yoy_change <- function(df, rate_col = "elevated_rate", group_cols = NULL) {
  if (is.null(df) || !all(c("year", rate_col) %in% names(df))) {
    return(NULL)
  }

  if (!is.null(group_cols)) {
    df <- df %>% group_by(across(all_of(group_cols)))
  }

  df %>%
    arrange(year) %>%
    mutate(
      prev_rate = lag(!!sym(rate_col)),
      yoy_change = !!sym(rate_col) - prev_rate,
      yoy_pct_change = (yoy_change / prev_rate) * 100
    ) %>%
    ungroup()
}

# =============================================================================
# Map Helper Functions
# =============================================================================

#' Create leaflet color palette function
#' @param values Vector of values to map
#' @param palette Color palette name or vector
#' @param domain Domain for the palette
#' @return Leaflet color function
create_map_palette <- function(values, palette = "YlOrRd", domain = NULL) {
  if (is.null(domain)) {
    domain <- range(values, na.rm = TRUE)
  }

  leaflet::colorNumeric(
    palette = palette,
    domain = domain,
    na.color = "#CCCCCC"
  )
}

#' Format popup content for map
#' @param name Geography name
#' @param rate Elevated BLL rate
#' @param count Elevated BLL count
#' @param year Year of data
#' @param threshold Threshold used
#' @return HTML string for popup
format_map_popup <- function(name, rate, count = NA, year = NA, threshold = NA) {
  content <- paste0(
    "<strong>", htmltools::htmlEscape(name), "</strong><br/>"
  )

  if (!is.na(rate)) {
    content <- paste0(content, "Elevated BLL Rate: ", format_rate(rate), "<br/>")
  }

  if (!is.na(count)) {
    content <- paste0(content, "Elevated Cases: ", format_count(count), "<br/>")
  }

  if (!is.na(year)) {
    content <- paste0(content, "Year: ", year, "<br/>")
  }

  if (!is.na(threshold)) {
    content <- paste0(content, "Threshold: ", format_threshold(threshold))
  }

  content
}

# =============================================================================
# Error Handling Helpers
# =============================================================================

#' Safely execute a function with error handling
#' @param expr Expression to evaluate
#' @param default Default value if error occurs
#' @param message Message to display on error
#' @return Result of expression or default value
safe_execute <- function(expr, default = NULL, message = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      if (!is.null(message)) {
        warning(message, ": ", e$message)
      }
      default
    }
  )
}

#' Create error message UI for Shiny
#' @param message Error message to display
#' @return Shiny UI element
create_error_ui <- function(message) {
  shiny::div(
    class = "alert alert-warning",
    shiny::icon("exclamation-triangle"),
    " ",
    message
  )
}

#' Create loading placeholder UI
#' @param message Loading message
#' @return Shiny UI element
create_loading_ui <- function(message = "Loading data...") {
  shiny::div(
    class = "text-center",
    style = "padding: 50px;",
    shiny::icon("spinner", class = "fa-spin fa-2x"),
    shiny::br(),
    shiny::br(),
    message
  )
}
