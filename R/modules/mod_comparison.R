# Comparison Module
# Shiny module for comparing NYC vs NY State vs National blood lead level data

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

# =============================================================================
# Module UI
# =============================================================================

#' Comparison Module UI
#' @param id Module namespace ID
#' @return Shiny UI elements
mod_comparison_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Header with explanation
    fluidRow(
      column(
        width = 12,
        div(
          class = "alert alert-info",
          icon("info-circle"),
          " ",
          strong("Comparison Notes: "),
          "This tab compares blood lead level trends across NYC, New York State, and national averages. ",
          "Note that the CDC reference value changed from 5 μg/dL to 3.5 μg/dL in October 2021, ",
          "which may affect trend interpretation. Rates before and after 2022 may not be directly comparable."
        )
      )
    ),

    # Filters
    fluidRow(
      column(
        width = 3,
        sliderInput(
          ns("year_range"),
          "Year Range",
          min = 2010,
          max = 2023,
          value = c(2017, 2022),
          step = 1,
          sep = ""
        )
      ),
      column(
        width = 3,
        checkboxGroupInput(
          ns("regions"),
          "Regions to Compare",
          choices = c("NYC", "NY State", "National"),
          selected = c("NYC", "NY State", "National")
        )
      ),
      column(
        width = 3,
        radioButtons(
          ns("adjustment"),
          "Rate Adjustment",
          choices = c(
            "Show actual rates" = "none",
            "Adjust to 5 μg/dL" = "adjust_5",
            "Adjust to 3.5 μg/dL" = "adjust_3.5"
          ),
          selected = "none"
        )
      ),
      column(
        width = 3,
        checkboxInput(
          ns("show_annotations"),
          "Show policy change annotations",
          value = TRUE
        )
      )
    ),

    # Main comparison chart
    fluidRow(
      column(
        width = 12,
        div(
          class = "box box-primary",
          div(class = "box-header with-border",
              h3(class = "box-title", "Trend Comparison: NYC vs NY State vs National")),
          div(class = "box-body",
              plotlyOutput(ns("trend_comparison"), height = "450px") %>%
                shinycssloaders::withSpinner()
          )
        )
      )
    ),

    # Side by side charts
    fluidRow(
      column(
        width = 6,
        div(
          class = "box box-info",
          div(class = "box-header with-border",
              h3(class = "box-title", "Year-over-Year Change")),
          div(class = "box-body",
              plotlyOutput(ns("yoy_change"), height = "350px") %>%
                shinycssloaders::withSpinner()
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "box box-info",
          div(class = "box-header with-border",
              h3(class = "box-title", "Most Recent Year Comparison")),
          div(class = "box-body",
              plotlyOutput(ns("recent_comparison"), height = "350px") %>%
                shinycssloaders::withSpinner()
          )
        )
      )
    ),

    # Detailed comparison table
    fluidRow(
      column(
        width = 12,
        div(
          class = "box box-success",
          div(class = "box-header with-border",
              h3(class = "box-title", "Detailed Comparison Data")),
          div(class = "box-body",
              DT::dataTableOutput(ns("comparison_table")) %>%
                shinycssloaders::withSpinner()
          )
        )
      )
    ),

    # Key insights
    fluidRow(
      column(
        width = 12,
        div(
          class = "box box-warning",
          div(class = "box-header with-border",
              h3(class = "box-title", icon("lightbulb"), " Key Insights")),
          div(class = "box-body",
              uiOutput(ns("insights"))
          )
        )
      )
    )
  )
}

# =============================================================================
# Module Server
# =============================================================================

#' Comparison Module Server
#' @param id Module namespace ID
#' @param comparison_data Reactive containing comparison dataset
#' @return None (side effects only)
mod_comparison_server <- function(id, comparison_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------------------------------------------------------------------------
    # Update year range based on available data
    # -------------------------------------------------------------------------
    observe({
      df <- comparison_data()
      if (is.null(df) || !"year" %in% names(df)) return()

      years <- df$year[!is.na(df$year)]
      if (length(years) == 0) return()

      updateSliderInput(
        session, "year_range",
        min = min(years),
        max = max(years),
        value = c(max(min(years), 2017), max(years))
      )
    })

    # -------------------------------------------------------------------------
    # Reactive: Filtered comparison data
    # -------------------------------------------------------------------------
    filtered_data <- reactive({
      df <- comparison_data()

      if (is.null(df) || nrow(df) == 0) return(NULL)

      # Filter by year range
      df <- df %>%
        filter(year >= input$year_range[1], year <= input$year_range[2])

      # Filter by selected regions
      if (!is.null(input$regions) && length(input$regions) > 0) {
        df <- df %>% filter(region %in% input$regions)
      }

      # Apply rate adjustment if requested
      if (input$adjustment == "adjust_5") {
        df <- df %>%
          mutate(
            display_rate = case_when(
              reference_threshold == 3.5 ~ elevated_rate * 0.75,
              TRUE ~ elevated_rate
            ),
            adjustment_applied = reference_threshold != 5
          )
      } else if (input$adjustment == "adjust_3.5") {
        df <- df %>%
          mutate(
            display_rate = case_when(
              reference_threshold == 5 ~ elevated_rate * 1.35,
              TRUE ~ elevated_rate
            ),
            adjustment_applied = reference_threshold != 3.5
          )
      } else {
        df <- df %>%
          mutate(
            display_rate = elevated_rate,
            adjustment_applied = FALSE
          )
      }

      df
    })

    # -------------------------------------------------------------------------
    # Main Trend Comparison Plot
    # -------------------------------------------------------------------------
    output$trend_comparison <- renderPlotly({
      df <- filtered_data()

      if (is.null(df) || nrow(df) == 0) {
        return(plotly_empty("No comparison data available"))
      }

      # Define colors for each region
      region_colors <- c(
        "NYC" = "#1f77b4",
        "NY State" = "#ff7f0e",
        "National" = "#2ca02c"
      )

      p <- plot_ly()

      for (reg in unique(df$region)) {
        reg_data <- df %>% filter(region == reg)
        p <- p %>%
          add_trace(
            data = reg_data,
            x = ~year,
            y = ~display_rate,
            type = "scatter",
            mode = "lines+markers",
            name = reg,
            line = list(color = region_colors[reg], width = 2),
            marker = list(size = 8),
            hovertemplate = paste(
              "<b>", reg, "</b><br>",
              "Year: %{x}<br>",
              "Rate: %{y:.1f}%<br>",
              "<extra></extra>"
            )
          )
      }

      # Add shapes and annotations
      shapes <- list()
      annotations <- list()

      if (input$show_annotations) {
        # Reference value change line (October 2021 -> affects 2022 data)
        if (input$year_range[1] <= 2022 && input$year_range[2] >= 2021) {
          shapes <- list(
            list(
              type = "line",
              x0 = 2021.75, x1 = 2021.75,
              y0 = 0, y1 = 1,
              yref = "paper",
              line = list(color = "rgba(255,0,0,0.5)", dash = "dash", width = 2)
            ),
            # Shaded region for post-threshold change
            list(
              type = "rect",
              x0 = 2021.75, x1 = input$year_range[2] + 0.5,
              y0 = 0, y1 = 1,
              yref = "paper",
              fillcolor = "rgba(255,0,0,0.05)",
              line = list(width = 0)
            )
          )

          annotations <- list(
            list(
              x = 2022,
              y = 1,
              yref = "paper",
              text = "3.5 μg/dL threshold era",
              showarrow = FALSE,
              xanchor = "left",
              yanchor = "top",
              font = list(size = 10, color = "red")
            )
          )
        }
      }

      p %>%
        layout(
          title = list(
            text = ifelse(
              input$adjustment != "none",
              paste0("Rates adjusted to ",
                     ifelse(input$adjustment == "adjust_5", "5", "3.5"),
                     " μg/dL threshold"),
              "Actual reported rates"
            ),
            font = list(size = 12)
          ),
          xaxis = list(
            title = "Year",
            tickformat = "d",
            dtick = 1
          ),
          yaxis = list(
            title = "Elevated BLL Rate (%)",
            rangemode = "tozero"
          ),
          legend = list(
            orientation = "h",
            y = -0.15,
            x = 0.5,
            xanchor = "center"
          ),
          hovermode = "x unified",
          shapes = shapes,
          annotations = annotations
        )
    })

    # -------------------------------------------------------------------------
    # Year-over-Year Change Plot
    # -------------------------------------------------------------------------
    output$yoy_change <- renderPlotly({
      df <- filtered_data()

      if (is.null(df) || nrow(df) == 0) {
        return(plotly_empty("No data available"))
      }

      # Calculate YoY change
      yoy_data <- df %>%
        group_by(region) %>%
        arrange(year) %>%
        mutate(
          prev_rate = lag(display_rate),
          yoy_change = display_rate - prev_rate,
          yoy_pct = (yoy_change / prev_rate) * 100
        ) %>%
        ungroup() %>%
        filter(!is.na(yoy_change))

      if (nrow(yoy_data) == 0) {
        return(plotly_empty("Insufficient data for YoY calculation"))
      }

      region_colors <- c(
        "NYC" = "#1f77b4",
        "NY State" = "#ff7f0e",
        "National" = "#2ca02c"
      )

      plot_ly(
        yoy_data,
        x = ~year,
        y = ~yoy_change,
        color = ~region,
        colors = region_colors,
        type = "bar",
        hovertemplate = paste(
          "<b>%{text}</b><br>",
          "Year: %{x}<br>",
          "Change: %{y:+.2f} pp<br>",
          "<extra></extra>"
        ),
        text = ~region
      ) %>%
        layout(
          title = "",
          xaxis = list(title = "Year", tickformat = "d"),
          yaxis = list(title = "Change (percentage points)"),
          barmode = "group",
          legend = list(orientation = "h", y = -0.2),
          shapes = list(
            list(
              type = "line",
              x0 = 0, x1 = 1,
              xref = "paper",
              y0 = 0, y1 = 0,
              line = list(color = "gray", dash = "dash")
            )
          )
        )
    })

    # -------------------------------------------------------------------------
    # Most Recent Year Comparison
    # -------------------------------------------------------------------------
    output$recent_comparison <- renderPlotly({
      df <- filtered_data()

      if (is.null(df) || nrow(df) == 0) {
        return(plotly_empty("No data available"))
      }

      # Get most recent year data
      recent_year <- max(df$year, na.rm = TRUE)
      recent_data <- df %>%
        filter(year == recent_year)

      if (nrow(recent_data) == 0) {
        return(plotly_empty("No recent data available"))
      }

      region_colors <- c(
        "NYC" = "#1f77b4",
        "NY State" = "#ff7f0e",
        "National" = "#2ca02c"
      )

      plot_ly(
        recent_data,
        x = ~region,
        y = ~display_rate,
        type = "bar",
        marker = list(color = ~region_colors[region]),
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Rate: %{y:.1f}%<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          title = list(
            text = paste("Year:", recent_year),
            font = list(size = 12)
          ),
          xaxis = list(title = ""),
          yaxis = list(title = "Elevated BLL Rate (%)", rangemode = "tozero"),
          showlegend = FALSE
        )
    })

    # -------------------------------------------------------------------------
    # Comparison Table
    # -------------------------------------------------------------------------
    output$comparison_table <- DT::renderDataTable({
      df <- filtered_data()

      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No comparison data available"),
          options = list(dom = "t")
        ))
      }

      # Pivot to wide format for comparison
      wide_df <- df %>%
        select(year, region, display_rate, reference_threshold) %>%
        pivot_wider(
          names_from = region,
          values_from = c(display_rate, reference_threshold),
          names_glue = "{region}_{.value}"
        ) %>%
        arrange(desc(year))

      # Clean up column names
      names(wide_df) <- gsub("_display_rate", " Rate (%)", names(wide_df))
      names(wide_df) <- gsub("_reference_threshold", " Threshold", names(wide_df))
      names(wide_df) <- gsub("year", "Year", names(wide_df))

      DT::datatable(
        wide_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("csv", "excel")
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(grep("Rate", names(wide_df), value = TRUE), digits = 1)
    })

    # -------------------------------------------------------------------------
    # Key Insights
    # -------------------------------------------------------------------------
    output$insights <- renderUI({
      df <- filtered_data()

      if (is.null(df) || nrow(df) == 0) {
        return(p("No data available to generate insights."))
      }

      insights <- list()

      # Get recent year data
      recent_year <- max(df$year, na.rm = TRUE)
      recent_data <- df %>% filter(year == recent_year)

      # Insight 1: NYC vs National comparison
      nyc_rate <- recent_data %>% filter(region == "NYC") %>% pull(display_rate)
      national_rate <- recent_data %>% filter(region == "National") %>% pull(display_rate)

      if (length(nyc_rate) > 0 && length(national_rate) > 0) {
        diff <- nyc_rate - national_rate
        if (diff > 0) {
          insights[[1]] <- tags$li(
            paste0("In ", recent_year, ", NYC's elevated BLL rate (",
                   round(nyc_rate, 1), "%) was ",
                   round(abs(diff), 1), " percentage points ",
                   "higher than the national average (",
                   round(national_rate, 1), "%).")
          )
        } else {
          insights[[1]] <- tags$li(
            paste0("In ", recent_year, ", NYC's elevated BLL rate (",
                   round(nyc_rate, 1), "%) was ",
                   round(abs(diff), 1), " percentage points ",
                   "lower than the national average (",
                   round(national_rate, 1), "%).")
          )
        }
      }

      # Insight 2: Trend direction
      first_year <- min(df$year, na.rm = TRUE)
      for (reg in unique(df$region)) {
        reg_data <- df %>%
          filter(region == reg) %>%
          arrange(year)

        if (nrow(reg_data) >= 2) {
          first_rate <- reg_data$display_rate[1]
          last_rate <- reg_data$display_rate[nrow(reg_data)]
          change <- last_rate - first_rate

          trend <- ifelse(change < 0, "decreased", "increased")
          insights[[length(insights) + 1]] <- tags$li(
            paste0(reg, " rates ", trend, " by ",
                   round(abs(change), 1), " percentage points from ",
                   first_year, " to ", recent_year, ".")
          )
        }
      }

      # Insight 3: Threshold change impact
      if (recent_year >= 2022 && input$adjustment == "none") {
        insights[[length(insights) + 1]] <- tags$li(
          class = "text-warning",
          icon("exclamation-triangle"),
          " Rates from 2022 onward use the new 3.5 μg/dL threshold and are not directly comparable to earlier years using 5 μg/dL. Consider using the rate adjustment option for trend analysis."
        )
      }

      if (length(insights) == 0) {
        return(p("Insufficient data to generate insights."))
      }

      tags$ul(insights)
    })
  })
}

# Helper function for empty plotly
plotly_empty <- function(message) {
  plot_ly() %>%
    layout(
      title = list(text = message, font = list(size = 12, color = "gray")),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}
