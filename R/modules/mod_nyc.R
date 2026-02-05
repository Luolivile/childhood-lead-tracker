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
          choices = NULL,
          selected = NULL
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

      # Filter by threshold
      if (!is.null(input$threshold) && "reference_threshold" %in% names(df)) {
        df <- df %>% filter(reference_threshold == as.numeric(input$threshold))
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
    # Update threshold choices based on available data
    # -------------------------------------------------------------------------
    observe({
      req(data())

      if ("reference_threshold" %in% names(data())) {
        thresholds <- sort(unique(data()$reference_threshold))
        choices <- setNames(thresholds, paste0("\u2265", thresholds, " \u03bcg/dL"))
        updateSelectInput(session, "threshold", choices = choices, selected = 5)
      }
    })

    # -------------------------------------------------------------------------
    # Update year choices when data or threshold changes
    # -------------------------------------------------------------------------
    observe({
      req(data())

      df <- data()
      # Filter by threshold first to show only relevant years
      if (!is.null(input$threshold) && "reference_threshold" %in% names(df)) {
        df <- df %>% filter(reference_threshold == as.numeric(input$threshold))
      }

      years <- df %>%
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
    # Update geography level choices based on available data
    # -------------------------------------------------------------------------
    observe({
      req(data())

      geo_types <- data() %>%
        filter(!is.na(geography_type)) %>%
        pull(geography_type) %>%
        unique()

      if (length(geo_types) > 0) {
        updateSelectInput(
          session, "geography_level",
          choices = geo_types,
          selected = geo_types[1]
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

      leafletProxy(ns("nyc_map")) %>%
        clearShapes() %>%
        clearControls()

      # If we have data, create markers for each geography
      if (!is.null(df) && nrow(df) > 0 && "elevated_rate" %in% names(df)) {
        # Create color palette based on data
        # Use a fixed domain to ensure consistent colors and legend
        max_rate <- max(df$elevated_rate, na.rm = TRUE)
        # Ensure we have a reasonable range for the palette (at least 0-5%)
        domain_max <- max(5, ceiling(max_rate))

        pal <- colorNumeric(
          palette = "YlOrRd",
          domain = c(0, domain_max),
          na.color = "#CCCCCC"
        )

        # Centroid coordinates for marker placement
        geo_coords <- data.frame(
          geography_name = c(
            "Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island",
            "New York City",
            "Kingsbridge-Riverdale","NE Bronx","Fordham-Bronx Park",
            "Pelham-Throgs Neck","Crotona-Tremont","High Bridge-Morrisania",
            "Hunts Point-Mott Haven",
            "Greenpoint","Downtown-Heights-Park Slope","Bed-Stuy-Crown Heights",
            "East New York","Sunset Park","Borough Park","E Flatbush-Flatbush",
            "Canarsie-Flatlands","Bensonhurst-Bay Ridge","Coney Island-Sheepshead Bay",
            "Williamsburg-Bushwick",
            "Washington Heights","Central Harlem-Morningside Hts","East Harlem",
            "Upper West Side","Upper East Side","Chelsea-Clinton",
            "Gramercy Park-Murray Hill","Greenwich Village-SoHo",
            "Union Square-Lower East Side","Lower Manhattan",
            "Long Island City-Astoria","West Queens","Flushing-Clearview",
            "Bayside-Meadowlands","Ridgewood-Forest Hills","Fresh Meadows",
            "SW Queens","Jamaica","SE Queens","Rockaway",
            "Port Richmond","Stapleton-St. George","Willowbrook",
            "South Beach-Tottenville"
          ),
          lat = c(
            40.8448, 40.6782, 40.7831, 40.7282, 40.5795,
            40.7128,
            40.8960, 40.8690, 40.8615, 40.8230, 40.8400, 40.8310, 40.8100,
            40.7270, 40.6810, 40.6710, 40.6610, 40.6480, 40.6340, 40.6500,
            40.6350, 40.6100, 40.5780, 40.7080,
            40.8440, 40.8110, 40.7950, 40.7870, 40.7740, 40.7500,
            40.7440, 40.7280, 40.7190, 40.7080,
            40.7600, 40.7440, 40.7640, 40.7600, 40.7120, 40.7350,
            40.6750, 40.6910, 40.6600, 40.5830,
            40.6350, 40.6280, 40.5980, 40.5560
          ),
          lng = c(
            -73.8648, -73.9442, -73.9712, -73.7949, -74.1502,
            -74.0060,
            -73.9040, -73.8470, -73.8890, -73.8210, -73.8970, -73.9130, -73.8870,
            -73.9510, -73.9870, -73.9410, -73.8780, -74.0130, -73.9920, -73.9570,
            -73.9010, -74.0150, -73.9610, -73.9360,
            -73.9380, -73.9520, -73.9430, -73.9720, -73.9570, -73.9980,
            -73.9810, -74.0020, -73.9870, -74.0150,
            -73.9180, -73.8760, -73.8140, -73.7680, -73.8500, -73.7850,
            -73.8100, -73.7870, -73.7530, -73.8250,
            -74.1380, -74.0780, -74.1640, -74.1280
          ),
          stringsAsFactors = FALSE
        )

        # Join coordinates to data
        map_data <- df %>%
          left_join(geo_coords, by = "geography_name")

        # Drop rows without coordinates
        map_data <- map_data %>% filter(!is.na(lat) & !is.na(lng))

        if (nrow(map_data) > 0) {
          leafletProxy(ns("nyc_map")) %>%
            addCircleMarkers(
              data = map_data,
              lng = ~lng,
              lat = ~lat,
              radius = ~sqrt(elevated_rate) * 8,
              fillColor = ~pal(elevated_rate),
              fillOpacity = 0.7,
              weight = 2,
              color = "#333",
              opacity = 0.9,
              popup = ~paste0(
                "<strong>", geography_name, "</strong><br/>",
                "Elevated BLL Rate: ", round(elevated_rate, 1), "%<br/>",
                "Elevated Cases: ", format(elevated_count, big.mark = ","), "<br/>",
                "Children Tested: ", format(children_tested, big.mark = ","), "<br/>",
                "Year: ", year, "<br/>",
                "Threshold: ", threshold_note
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal = pal,
              values = c(0, domain_max),
              title = "Elevated BLL<br/>Rate (%)",
              opacity = 0.7,
              labFormat = labelFormat(suffix = "%")
            )
        }
      }

      # Also add NTA boundaries if available (as background context)
      if (!is.null(geo_data) && !is.null(geo_data$nyc_nta)) {
        leafletProxy(ns("nyc_map")) %>%
          addPolygons(
            data = geo_data$nyc_nta,
            fillColor = "transparent",
            fillOpacity = 0,
            weight = 0.5,
            color = "#999",
            opacity = 0.5
          )
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

      # Filter by selected threshold for consistent time series
      if (!is.null(input$threshold) && "reference_threshold" %in% names(df)) {
        df <- df %>% filter(reference_threshold == as.numeric(input$threshold))
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
