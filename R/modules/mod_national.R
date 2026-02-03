# National Data Visualization Module
# Shiny module for displaying CDC national blood lead level data

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)

# =============================================================================
# Module UI
# =============================================================================

#' National Module UI
#' @param id Module namespace ID
#' @return Shiny UI elements
mod_national_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Filters row
    fluidRow(
      column(
        width = 3,
        selectInput(
          ns("year"),
          "Year",
          choices = NULL,
          selected = NULL
        )
      ),
      column(
        width = 3,
        selectizeInput(
          ns("states"),
          "States (select multiple)",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "All states",
            plugins = list("remove_button")
          )
        )
      ),
      column(
        width = 3,
        selectInput(
          ns("sort_by"),
          "Sort Map By",
          choices = c(
            "Elevated Rate" = "elevated_rate",
            "Children Tested" = "children_tested",
            "Elevated Count" = "elevated_count"
          ),
          selected = "elevated_rate"
        )
      ),
      column(
        width = 3,
        checkboxInput(
          ns("show_all_years"),
          "Show all years in time series",
          value = TRUE
        )
      )
    ),

    # Summary cards
    fluidRow(
      column(
        width = 3,
        div(
          class = "info-box",
          div(class = "info-box-icon bg-blue", icon("flag-usa")),
          div(
            class = "info-box-content",
            span(class = "info-box-text", "States Reporting"),
            span(class = "info-box-number", textOutput(ns("states_reporting"), inline = TRUE))
          )
        )
      ),
      column(
        width = 3,
        div(
          class = "info-box",
          div(class = "info-box-icon bg-green", icon("child")),
          div(
            class = "info-box-content",
            span(class = "info-box-text", "Children Tested"),
            span(class = "info-box-number", textOutput(ns("total_tested"), inline = TRUE))
          )
        )
      ),
      column(
        width = 3,
        div(
          class = "info-box",
          div(class = "info-box-icon bg-yellow", icon("exclamation-triangle")),
          div(
            class = "info-box-content",
            span(class = "info-box-text", "Elevated BLL Cases"),
            span(class = "info-box-number", textOutput(ns("elevated_count"), inline = TRUE))
          )
        )
      ),
      column(
        width = 3,
        div(
          class = "info-box",
          div(class = "info-box-icon bg-red", icon("percentage")),
          div(
            class = "info-box-content",
            span(class = "info-box-text", "National Rate"),
            span(class = "info-box-number", textOutput(ns("national_rate"), inline = TRUE))
          )
        )
      )
    ),

    # Main visualizations
    fluidRow(
      column(
        width = 7,
        div(
          class = "box box-primary",
          div(class = "box-header with-border",
              h3(class = "box-title", "US Map - State Elevated BLL Rates")),
          div(class = "box-body",
              leafletOutput(ns("us_map"), height = "500px") %>%
                shinycssloaders::withSpinner()
          )
        )
      ),
      column(
        width = 5,
        div(
          class = "box box-primary",
          div(class = "box-header with-border",
              h3(class = "box-title", "State Rankings")),
          div(class = "box-body",
              plotlyOutput(ns("state_rankings"), height = "500px") %>%
                shinycssloaders::withSpinner()
          )
        )
      )
    ),

    # Time series and national summary
    fluidRow(
      column(
        width = 6,
        div(
          class = "box box-info",
          div(class = "box-header with-border",
              h3(class = "box-title", "State Time Series")),
          div(class = "box-body",
              plotlyOutput(ns("state_time_series"), height = "400px") %>%
                shinycssloaders::withSpinner()
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "box box-info",
          div(class = "box-header with-border",
              h3(class = "box-title", "National Trend")),
          div(class = "box-body",
              plotlyOutput(ns("national_trend"), height = "400px") %>%
                shinycssloaders::withSpinner()
          )
        )
      )
    ),

    # Data table
    fluidRow(
      column(
        width = 12,
        div(
          class = "box box-success",
          div(class = "box-header with-border",
              h3(class = "box-title", "State Data Table")),
          div(class = "box-body",
              DT::dataTableOutput(ns("data_table")) %>%
                shinycssloaders::withSpinner()
          )
        )
      )
    )
  )
}

# =============================================================================
# Module Server
# =============================================================================

#' National Module Server
#' @param id Module namespace ID
#' @param state_data Reactive containing CDC state data
#' @param national_data Reactive containing CDC national data
#' @param geo Reactive containing geographic boundaries
#' @return None (side effects only)
mod_national_server <- function(id, state_data, national_data, geo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------------------------------------------------------------------------
    # Update filter choices when data changes
    # -------------------------------------------------------------------------
    observe({
      req(state_data())

      # Update years
      years <- state_data() %>%
        filter(!is.na(year)) %>%
        pull(year) %>%
        unique() %>%
        sort(decreasing = TRUE)

      if (length(years) > 0) {
        updateSelectInput(
          session, "year",
          choices = c("All", years),
          selected = years[1]
        )
      }

      # Update states
      states <- state_data() %>%
        filter(!is.na(state)) %>%
        pull(state) %>%
        unique() %>%
        sort()

      updateSelectizeInput(
        session, "states",
        choices = states,
        selected = NULL
      )
    })

    # -------------------------------------------------------------------------
    # Reactive: Filtered state data
    # -------------------------------------------------------------------------
    filtered_state_data <- reactive({
      req(state_data())

      df <- state_data()

      # Filter by year
      if (!is.null(input$year) && input$year != "All") {
        df <- df %>% filter(year == as.numeric(input$year))
      }

      # Filter by states
      if (!is.null(input$states) && length(input$states) > 0) {
        df <- df %>% filter(state %in% input$states)
      }

      df
    })

    # -------------------------------------------------------------------------
    # Summary Statistics Outputs
    # -------------------------------------------------------------------------
    output$states_reporting <- renderText({
      df <- filtered_state_data()
      if (is.null(df)) return("N/A")
      length(unique(df$state))
    })

    output$total_tested <- renderText({
      df <- filtered_state_data()
      if (is.null(df) || !"children_tested" %in% names(df)) return("N/A")
      format_count(sum(df$children_tested, na.rm = TRUE))
    })

    output$elevated_count <- renderText({
      df <- filtered_state_data()
      if (is.null(df) || !"elevated_count" %in% names(df)) return("N/A")
      format_count(sum(df$elevated_count, na.rm = TRUE))
    })

    output$national_rate <- renderText({
      nd <- national_data()
      if (is.null(nd) || !"elevated_rate" %in% names(nd)) {
        # Calculate from state data
        df <- filtered_state_data()
        if (is.null(df)) return("N/A")
        return(format_rate(weighted.mean(
          df$elevated_rate,
          df$children_tested,
          na.rm = TRUE
        )))
      }

      # Filter to selected year
      if (!is.null(input$year) && input$year != "All") {
        nd <- nd %>% filter(year == as.numeric(input$year))
      }

      if (nrow(nd) == 0) return("N/A")
      format_rate(nd$elevated_rate[1])
    })

    # -------------------------------------------------------------------------
    # US Map
    # -------------------------------------------------------------------------
    output$us_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 4)
    })

    # Update map with data
    observe({
      df <- filtered_state_data()
      geo_data <- geo()

      leafletProxy(ns("us_map")) %>%
        clearShapes() %>%
        clearControls()

      if (is.null(geo_data) || is.null(geo_data$us_states)) return()
      if (is.null(df) || nrow(df) == 0) return()

      boundaries <- geo_data$us_states

      # Get the metric to display
      metric_col <- input$sort_by

      # Aggregate data by state for the selected year
      state_summary <- df %>%
        group_by(state) %>%
        summarise(
          elevated_rate = mean(elevated_rate, na.rm = TRUE),
          children_tested = sum(children_tested, na.rm = TRUE),
          elevated_count = sum(elevated_count, na.rm = TRUE),
          .groups = "drop"
        )

      # Try to join with boundaries
      # Common column names in GeoJSON: name, NAME, State
      name_col <- intersect(names(boundaries), c("name", "NAME", "State", "state"))[1]

      if (!is.na(name_col)) {
        boundaries <- boundaries %>%
          left_join(state_summary, by = setNames("state", name_col))

        # Create color palette
        metric_values <- boundaries[[metric_col]]
        if (all(is.na(metric_values))) {
          pal <- colorNumeric("YlOrRd", domain = c(0, 10), na.color = "#CCCCCC")
        } else {
          pal <- colorNumeric(
            "YlOrRd",
            domain = range(metric_values, na.rm = TRUE),
            na.color = "#CCCCCC"
          )
        }

        # Add polygons
        leafletProxy(ns("us_map")) %>%
          addPolygons(
            data = boundaries,
            fillColor = ~pal(get(metric_col)),
            fillOpacity = 0.7,
            weight = 1,
            color = "#333",
            opacity = 0.8,
            highlightOptions = highlightOptions(
              weight = 2,
              color = "#000",
              fillOpacity = 0.9
            ),
            popup = ~paste0(
              "<strong>", get(name_col), "</strong><br/>",
              "Elevated Rate: ", round(elevated_rate, 1), "%<br/>",
              "Children Tested: ", format(children_tested, big.mark = ","), "<br/>",
              "Elevated Cases: ", format(elevated_count, big.mark = ",")
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = metric_values,
            title = switch(
              metric_col,
              "elevated_rate" = "Rate (%)",
              "children_tested" = "Tested",
              "elevated_count" = "Cases"
            ),
            na.label = "No data"
          )
      }
    })

    # -------------------------------------------------------------------------
    # State Rankings Bar Chart
    # -------------------------------------------------------------------------
    output$state_rankings <- renderPlotly({
      df <- filtered_state_data()

      if (is.null(df) || nrow(df) == 0) {
        return(plotly_empty_message("No state data available"))
      }

      # Get top/bottom states by rate
      state_summary <- df %>%
        group_by(state) %>%
        summarise(
          elevated_rate = mean(elevated_rate, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(elevated_rate)) %>%
        head(15)

      plot_ly(
        state_summary,
        y = ~reorder(state, elevated_rate),
        x = ~elevated_rate,
        type = "bar",
        orientation = "h",
        marker = list(
          color = ~elevated_rate,
          colorscale = "YlOrRd"
        ),
        hovertemplate = "<b>%{y}</b><br>Rate: %{x:.1f}%<extra></extra>"
      ) %>%
        layout(
          title = "",
          xaxis = list(title = "Elevated BLL Rate (%)"),
          yaxis = list(title = ""),
          showlegend = FALSE,
          margin = list(l = 100)
        )
    })

    # -------------------------------------------------------------------------
    # State Time Series
    # -------------------------------------------------------------------------
    output$state_time_series <- renderPlotly({
      df <- state_data()

      if (is.null(df) || nrow(df) == 0) {
        return(plotly_empty_message("No time series data available"))
      }

      # Filter states if selected
      selected_states <- input$states
      if (!is.null(selected_states) && length(selected_states) > 0) {
        df <- df %>% filter(state %in% selected_states)
      } else {
        # Show top 5 states by most recent rate
        top_states <- df %>%
          filter(year == max(year, na.rm = TRUE)) %>%
          arrange(desc(elevated_rate)) %>%
          head(5) %>%
          pull(state)
        df <- df %>% filter(state %in% top_states)
      }

      plot_ly(
        df,
        x = ~year,
        y = ~elevated_rate,
        color = ~state,
        type = "scatter",
        mode = "lines+markers",
        hovertemplate = "<b>%{text}</b><br>Year: %{x}<br>Rate: %{y:.1f}%<extra></extra>",
        text = ~state
      ) %>%
        layout(
          title = "",
          xaxis = list(title = "Year", tickformat = "d"),
          yaxis = list(title = "Elevated BLL Rate (%)", rangemode = "tozero"),
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified"
        )
    })

    # -------------------------------------------------------------------------
    # National Trend
    # -------------------------------------------------------------------------
    output$national_trend <- renderPlotly({
      nd <- national_data()

      if (is.null(nd) || nrow(nd) == 0) {
        return(plotly_empty_message("No national trend data available"))
      }

      plot_ly(
        nd,
        x = ~year,
        y = ~elevated_rate,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#1f77b4", width = 3),
        marker = list(size = 10),
        hovertemplate = "Year: %{x}<br>National Rate: %{y:.1f}%<extra></extra>"
      ) %>%
        layout(
          title = "",
          xaxis = list(title = "Year", tickformat = "d"),
          yaxis = list(title = "National Elevated BLL Rate (%)", rangemode = "tozero"),
          shapes = list(
            list(
              type = "line",
              x0 = 2021.75, x1 = 2021.75,
              y0 = 0, y1 = 1,
              yref = "paper",
              line = list(color = "red", dash = "dash", width = 1)
            )
          ),
          annotations = list(
            list(
              x = 2021.75,
              y = 0.95,
              yref = "paper",
              text = "Threshold: 5→3.5 μg/dL",
              showarrow = FALSE,
              textangle = 0,
              xanchor = "left",
              font = list(size = 9, color = "red")
            )
          )
        )
    })

    # -------------------------------------------------------------------------
    # Data Table
    # -------------------------------------------------------------------------
    output$data_table <- DT::renderDataTable({
      df <- filtered_state_data()

      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No data available"),
          options = list(dom = "t")
        ))
      }

      display_df <- df %>%
        select(any_of(c("state", "year", "children_tested",
                        "elevated_count", "elevated_rate", "threshold_used"))) %>%
        arrange(desc(year), desc(elevated_rate))

      names(display_df) <- c("State", "Year", "Children Tested",
                             "Elevated Cases", "Rate (%)", "Threshold (μg/dL)")[1:ncol(display_df)]

      DT::datatable(
        display_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("csv", "excel")
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound("Rate (%)", digits = 1) %>%
        DT::formatRound("Children Tested", digits = 0) %>%
        DT::formatRound("Elevated Cases", digits = 0)
    })
  })
}

# Helper function for empty plotly messages
plotly_empty_message <- function(message) {
  plot_ly() %>%
    layout(
      title = message,
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}
