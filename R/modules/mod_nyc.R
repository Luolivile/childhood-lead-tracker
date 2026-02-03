# NYC Data Visualization Module
# Shiny module for displaying NYC blood lead level data

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)

# =============================================================================
# Module UI
# =============================================================================

#' NYC Module UI
#' @param id Module namespace ID
#' @return Shiny UI elements
mod_nyc_ui <- function(id) {
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
        selectInput(
          ns("geography_level"),
          "Geography Level",
          choices = c("Citywide", "Borough", "NTA", "UHF", "CD"),
          selected = "Borough"
        )
      ),
      column(
        width = 3,
        selectInput(
          ns("threshold"),
          "BLL Threshold",
          choices = c("≥5 μg/dL" = 5, "≥3.5 μg/dL" = 3.5),
          selected = 5
        )
      ),
      column(
        width = 3,
        actionButton(
          ns("refresh_data"),
          "Refresh Data",
          icon = icon("sync"),
          class = "btn-primary",
          style = "margin-top: 25px;"
        )
      )
    ),

    # Summary cards
    fluidRow(
      column(
        width = 4,
        div(
          class = "info-box",
          div(class = "info-box-icon bg-blue", icon("child")),
          div(
            class = "info-box-content",
            span(class = "info-box-text", "Children Tested"),
            span(class = "info-box-number", textOutput(ns("total_tested"), inline = TRUE))
          )
        )
      ),
      column(
        width = 4,
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
        width = 4,
        div(
          class = "info-box",
          div(class = "info-box-icon bg-red", icon("percentage")),
          div(
            class = "info-box-content",
            span(class = "info-box-text", "Elevated BLL Rate"),
            span(class = "info-box-number", textOutput(ns("elevated_rate"), inline = TRUE))
          )
        )
      )
    ),

    # Main visualizations
    fluidRow(
      column(
        width = 6,
        div(
          class = "box box-primary",
          div(class = "box-header with-border",
              h3(class = "box-title", "NYC Map - Elevated BLL Rates")),
          div(class = "box-body",
              leafletOutput(ns("nyc_map"), height = "450px") %>%
                shinycssloaders::withSpinner()
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "box box-primary",
          div(class = "box-header with-border",
              h3(class = "box-title", "Time Series - Elevated BLL Rates")),
          div(class = "box-body",
              plotlyOutput(ns("time_series"), height = "450px") %>%
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
          class = "box box-info",
          div(class = "box-header with-border",
              h3(class = "box-title", "Detailed Data")),
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

#' NYC Module Server
#' @param id Module namespace ID
#' @param data Reactive containing NYC data list
#' @param geo Reactive containing geographic boundaries
#' @return None (side effects only)
mod_nyc_server <- function(id, data, geo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------------------------------------------------------------------------
    # Reactive: Filtered data based on user selections
    # -------------------------------------------------------------------------
    filtered_data <- reactive({
      req(data())

      df <- data()

      if (is.null(df)) {
        return(NULL)
      }

      # Filter by year if selected
      if (!is.null(input$year) && input$year != "All") {
        df <- df %>% filter(year == as.numeric(input$year))
      }

      # Filter by geography level
      if (!is.null(input$geography_level) && "geography_type" %in% names(df)) {
        df <- df %>%
          filter(
            tolower(geography_type) == tolower(input$geography_level) |
              (input$geography_level == "Citywide" &
                 str_detect(tolower(geography_name), "city|nyc"))
          )
      }

      df
    })

    # -------------------------------------------------------------------------
    # Update year choices when data changes
    # -------------------------------------------------------------------------
    observe({
      req(data())

      years <- data() %>%
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
    })

    # -------------------------------------------------------------------------
    # Summary Statistics Outputs
    # -------------------------------------------------------------------------
    output$total_tested <- renderText({
      df <- filtered_data()
      if (is.null(df) || !"children_tested" %in% names(df)) {
        return("N/A")
      }
      format_count(sum(df$children_tested, na.rm = TRUE))
    })

    output$elevated_count <- renderText({
      df <- filtered_data()
      if (is.null(df) || !"elevated_count" %in% names(df)) {
        return("N/A")
      }
      format_count(sum(df$elevated_count, na.rm = TRUE))
    })

    output$elevated_rate <- renderText({
      df <- filtered_data()
      if (is.null(df) || !"elevated_rate" %in% names(df)) {
        return("N/A")
      }
      format_rate(mean(df$elevated_rate, na.rm = TRUE))
    })

    # -------------------------------------------------------------------------
    # NYC Map
    # -------------------------------------------------------------------------
    output$nyc_map <- renderLeaflet({
      # Initialize base map centered on NYC
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -73.95, lat = 40.7128, zoom = 10)
    })

    # Update map with data
    observe({
      df <- filtered_data()
      geo_data <- geo()

      # Get appropriate boundaries based on geography level
      boundaries <- NULL
      if (!is.null(geo_data)) {
        if (input$geography_level == "Borough" && !is.null(geo_data$nyc_borough)) {
          boundaries <- geo_data$nyc_borough
        } else if (input$geography_level == "NTA" && !is.null(geo_data$nyc_nta)) {
          boundaries <- geo_data$nyc_nta
        }
      }

      leafletProxy(ns("nyc_map")) %>%
        clearShapes() %>%
        clearControls()

      if (!is.null(boundaries) && !is.null(df) && nrow(df) > 0) {
        # Try to join data with boundaries
        # This requires matching geography names
        if ("elevated_rate" %in% names(df)) {
          # Create color palette
          pal <- colorNumeric(
            palette = "YlOrRd",
            domain = c(0, max(df$elevated_rate, na.rm = TRUE)),
            na.color = "#CCCCCC"
          )

          # Simple choropleth if boundaries don't have data
          leafletProxy(ns("nyc_map")) %>%
            addPolygons(
              data = boundaries,
              fillColor = "#3388ff",
              fillOpacity = 0.3,
              weight = 1,
              color = "#333",
              opacity = 0.8,
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#666",
                fillOpacity = 0.5
              )
            )
        }
      }
    })

    # -------------------------------------------------------------------------
    # Time Series Plot
    # -------------------------------------------------------------------------
    output$time_series <- renderPlotly({
      df <- data()

      if (is.null(df) || !"year" %in% names(df) || !"elevated_rate" %in% names(df)) {
        return(
          plot_ly() %>%
            layout(
              title = "No data available",
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        )
      }

      # Aggregate by year and geography type
      plot_data <- df %>%
        filter(!is.na(year), !is.na(elevated_rate)) %>%
        group_by(year, geography_type) %>%
        summarise(
          mean_rate = mean(elevated_rate, na.rm = TRUE),
          .groups = "drop"
        )

      if (nrow(plot_data) == 0) {
        return(
          plot_ly() %>%
            layout(
              title = "No data available for selected filters",
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        )
      }

      # Get policy annotations
      annotations <- get_policy_annotations()

      p <- plot_ly(plot_data, x = ~year, y = ~mean_rate,
                   color = ~geography_type, type = "scatter", mode = "lines+markers",
                   hovertemplate = paste(
                     "<b>%{x}</b><br>",
                     "Rate: %{y:.1f}%<br>",
                     "<extra></extra>"
                   )) %>%
        layout(
          title = "",
          xaxis = list(title = "Year", tickformat = "d"),
          yaxis = list(title = "Elevated BLL Rate (%)", rangemode = "tozero"),
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified",
          shapes = list(
            # Reference value change line
            list(
              type = "line",
              x0 = 2021.75, x1 = 2021.75,
              y0 = 0, y1 = 1,
              yref = "paper",
              line = list(color = "gray", dash = "dash", width = 1)
            )
          ),
          annotations = list(
            list(
              x = 2021.75,
              y = 1,
              yref = "paper",
              text = "Threshold change",
              showarrow = FALSE,
              textangle = -90,
              xanchor = "left",
              font = list(size = 10, color = "gray")
            )
          )
        )

      p
    })

    # -------------------------------------------------------------------------
    # Data Table
    # -------------------------------------------------------------------------
    output$data_table <- DT::renderDataTable({
      df <- filtered_data()

      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No data available"),
          options = list(dom = "t")
        ))
      }

      # Select and rename columns for display
      display_cols <- c("geography_name", "geography_type", "year",
                        "elevated_rate", "elevated_count", "threshold_note")
      display_cols <- intersect(display_cols, names(df))

      display_df <- df %>%
        select(all_of(display_cols)) %>%
        arrange(desc(year), geography_name)

      # Rename for display
      names(display_df) <- c("Geography", "Type", "Year",
                             "Rate (%)", "Count", "Threshold")[1:length(display_cols)]

      DT::datatable(
        display_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("csv", "excel")
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound("Rate (%)", digits = 1)
    })

    # -------------------------------------------------------------------------
    # Refresh data button
    # -------------------------------------------------------------------------
    observeEvent(input$refresh_data, {
      showNotification("Data refresh requested...", type = "message")
      # This would trigger a re-fetch in the parent app
    })
  })
}
