# Lead Surveillance Dashboard
# Main Shiny Application Entry Point
#
# This dashboard displays childhood blood lead level (BLL) surveillance data
# comparing NYC-level and national (CDC) data.

# =============================================================================
# Load Required Packages
# =============================================================================

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(DT)
library(here)

# =============================================================================
# Source Helper Files and Modules
# =============================================================================

source(here("R", "data_fetch.R"))
source(here("R", "data_clean.R"))
source(here("R", "utils.R"))
source(here("R", "modules", "mod_nyc.R"))
source(here("R", "modules", "mod_national.R"))
source(here("R", "modules", "mod_comparison.R"))

# =============================================================================
# UI Definition
# =============================================================================

ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(
    title = "Lead Surveillance Dashboard",
    titleWidth = 280
  ),

  # Sidebar
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem("NYC Overview", tabName = "nyc", icon = icon("city")),
      menuItem("National Overview", tabName = "national", icon = icon("flag-usa")),
      menuItem("Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("About / Methods", tabName = "about", icon = icon("info-circle"))
    ),

    # Data refresh section
    hr(),
    div(
      style = "padding: 10px;",
      h5("Data Status"),
      verbatimTextOutput("data_status", placeholder = TRUE),
      actionButton(
        "refresh_all",
        "Refresh All Data",
        icon = icon("sync"),
        class = "btn-primary btn-block"
      )
    )
  ),

  # Body
  dashboardBody(
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

    tabItems(
      # NYC Tab
      tabItem(
        tabName = "nyc",
        h2("NYC Blood Lead Level Surveillance"),
        p(
          class = "lead",
          "Explore childhood blood lead level data across New York City boroughs and neighborhoods."
        ),
        mod_nyc_ui("nyc_module")
      ),

      # National Tab
      tabItem(
        tabName = "national",
        h2("National Blood Lead Level Surveillance"),
        p(
          class = "lead",
          "View CDC surveillance data for blood lead levels across all US states."
        ),
        mod_national_ui("national_module")
      ),

      # Comparison Tab
      tabItem(
        tabName = "comparison",
        h2("NYC vs National Comparison"),
        p(
          class = "lead",
          "Compare blood lead level trends between NYC, New York State, and national averages."
        ),
        mod_comparison_ui("comparison_module")
      ),

      # About Tab
      tabItem(
        tabName = "about",
        fluidRow(
          column(
            width = 8,
            div(
              class = "box box-primary",
              div(class = "box-header with-border",
                  h3(class = "box-title", "About This Dashboard")),
              div(
                class = "box-body",
                h4("Purpose"),
                p(
                  "This dashboard provides interactive visualization of childhood blood lead level ",
                  "(BLL) surveillance data. It allows users to explore trends in lead exposure ",
                  "across NYC neighborhoods and compare them to state and national patterns."
                ),

                h4("Data Sources"),
                tags$table(
                  class = "table table-bordered",
                  tags$thead(
                    tags$tr(
                      tags$th("Source"),
                      tags$th("Coverage"),
                      tags$th("Geography")
                    )
                  ),
                  tags$tbody(
                    tags$tr(
                      tags$td(tags$a(
                        href = "https://a816-dohbesp.nyc.gov/IndicatorPublic/data-explorer/lead/",
                        target = "_blank",
                        "NYC Environment & Health Data Portal"
                      )),
                      tags$td("NYC"),
                      tags$td("Borough, UHF, CD, NTA")
                    ),
                    tags$tr(
                      tags$td(tags$a(
                        href = "https://github.com/nychealth/EHDP-data",
                        target = "_blank",
                        "nychealth/EHDP-data GitHub"
                      )),
                      tags$td("NYC"),
                      tags$td("Borough, UHF, CD, NTA")
                    ),
                    tags$tr(
                      tags$td(tags$a(
                        href = "https://www.cdc.gov/lead-prevention/php/data/state-surveillance-data.html",
                        target = "_blank",
                        "CDC State Surveillance"
                      )),
                      tags$td("United States"),
                      tags$td("State, County")
                    ),
                    tags$tr(
                      tags$td(tags$a(
                        href = "https://www.cdc.gov/lead-prevention/php/data/national-surveillance-data.html",
                        target = "_blank",
                        "CDC National Surveillance"
                      )),
                      tags$td("United States"),
                      tags$td("National")
                    )
                  )
                ),

                h4("Important Methodology Notes"),
                tags$ul(
                  tags$li(
                    tags$strong("Reference Value Change: "),
                    "The CDC blood lead reference value changed from 5 μg/dL to 3.5 μg/dL ",
                    "in October 2021. This means that data from 2022 onward uses a lower ",
                    "threshold for defining 'elevated' blood lead levels. Direct comparisons ",
                    "between pre-2022 and post-2021 data should be made with caution."
                  ),
                  tags$li(
                    tags$strong("Geographic Limitations: "),
                    "NYC does not publish ZIP-code level data directly. The finest geographic ",
                    "resolution available is Neighborhood Tabulation Area (NTA). NYS Open Data ",
                    "has ZIP-level data but excludes NYC."
                  ),
                  tags$li(
                    tags$strong("Testing Variability: "),
                    "The number of children tested varies by year and geography. Rates may be ",
                    "less reliable for areas with small numbers of tested children."
                  ),
                  tags$li(
                    tags$strong("Reporting Delays: "),
                    "Surveillance data typically has a 1-2 year lag. The most recent available ",
                    "data may not reflect current conditions."
                  )
                ),

                h4("Rate Adjustment"),
                p(
                  "The Comparison tab offers an option to adjust rates for threshold changes. ",
                  "This uses an approximate conversion factor based on typical BLL distributions. ",
                  "Actual adjustment would require access to individual-level data, which is not ",
                  "publicly available. Use adjusted rates for trend analysis only."
                ),

                h4("Contact & Feedback"),
                p(
                  "For questions about this dashboard or to report issues, please ",
                  tags$a(
                    href = "https://github.com/YOUR_USERNAME/childhood-lead-tracker/issues",
                    target = "_blank",
                    "open an issue on GitHub"
                  ), "."
                )
              )
            )
          ),
          column(
            width = 4,
            div(
              class = "box box-info",
              div(class = "box-header with-border",
                  h3(class = "box-title", "Key Terms")),
              div(
                class = "box-body",
                tags$dl(
                  tags$dt("BLL"),
                  tags$dd("Blood Lead Level - the concentration of lead in blood, measured in micrograms per deciliter (μg/dL)"),

                  tags$dt("Elevated BLL"),
                  tags$dd("Blood lead level at or above the reference value (currently 3.5 μg/dL, formerly 5 μg/dL)"),

                  tags$dt("Reference Value"),
                  tags$dd("The CDC-established threshold used to identify children with elevated blood lead levels requiring follow-up"),

                  tags$dt("NTA"),
                  tags$dd("Neighborhood Tabulation Area - a geographic unit used by NYC for neighborhood-level statistics"),

                  tags$dt("UHF"),
                  tags$dd("United Hospital Fund neighborhood - a geographic unit used for health statistics in NYC"),

                  tags$dt("CD"),
                  tags$dd("Community District - an administrative geographic unit in NYC")
                )
              )
            ),
            div(
              class = "box box-warning",
              div(class = "box-header with-border",
                  h3(class = "box-title", "Disclaimer")),
              div(
                class = "box-body",
                p(
                  "This dashboard is provided for informational and educational purposes only. ",
                  "It should not be used for clinical decision-making. Always consult ",
                  "official health department sources and healthcare providers for ",
                  "guidance on lead exposure and testing."
                )
              )
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# Server Logic
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Reactive Values for Data Storage
  # ---------------------------------------------------------------------------
  app_data <- reactiveValues(
    raw = NULL,
    cleaned = NULL,
    last_updated = NULL,
    status = "Not loaded"
  )

  # ---------------------------------------------------------------------------
  # Initial Data Load
  # ---------------------------------------------------------------------------
  observe({
    # Try to load processed data first
    processed <- tryCatch({
      load_processed_data()
    }, error = function(e) {
      NULL
    })

    if (!is.null(processed) && !all(sapply(processed, is.null))) {
      app_data$cleaned <- processed
      app_data$status <- "Loaded from cache"
      app_data$last_updated <- Sys.time()
    } else {
      # Fetch and process fresh data
      withProgress(message = "Loading data...", value = 0, {
        incProgress(0.2, detail = "Fetching NYC data...")
        raw <- fetch_all_data(force = FALSE)

        incProgress(0.5, detail = "Processing data...")
        cleaned <- clean_all_data(raw)

        incProgress(0.8, detail = "Saving processed data...")
        save_processed_data(cleaned)

        app_data$raw <- raw
        app_data$cleaned <- cleaned
        app_data$status <- "Freshly loaded"
        app_data$last_updated <- Sys.time()
      })
    }
  })

  # ---------------------------------------------------------------------------
  # Data Refresh Handler
  # ---------------------------------------------------------------------------
  observeEvent(input$refresh_all, {
    withProgress(message = "Refreshing all data...", value = 0, {
      incProgress(0.2, detail = "Fetching NYC data...")
      raw <- fetch_all_data(force = TRUE)

      incProgress(0.5, detail = "Processing data...")
      cleaned <- clean_all_data(raw)

      incProgress(0.8, detail = "Saving processed data...")
      save_processed_data(cleaned)

      app_data$raw <- raw
      app_data$cleaned <- cleaned
      app_data$status <- "Refreshed"
      app_data$last_updated <- Sys.time()
    })

    showNotification("Data refreshed successfully!", type = "message")
  })

  # ---------------------------------------------------------------------------
  # Data Status Display
  # ---------------------------------------------------------------------------
  output$data_status <- renderText({
    if (is.null(app_data$last_updated)) {
      "Status: Loading..."
    } else {
      paste0(
        "Status: ", app_data$status, "\n",
        "Updated: ", format(app_data$last_updated, "%Y-%m-%d %H:%M")
      )
    }
  })

  # ---------------------------------------------------------------------------
  # Module Servers
  # ---------------------------------------------------------------------------

  # NYC Module
  mod_nyc_server(
    "nyc_module",
    data = reactive({
      app_data$cleaned$nyc
    }),
    geo = reactive({
      app_data$cleaned$geo
    })
  )

  # National Module
  mod_national_server(
    "national_module",
    state_data = reactive({
      app_data$cleaned$cdc_state
    }),
    national_data = reactive({
      app_data$cleaned$cdc_national
    }),
    geo = reactive({
      app_data$cleaned$geo
    })
  )

  # Comparison Module
  mod_comparison_server(
    "comparison_module",
    comparison_data = reactive({
      app_data$cleaned$comparison
    })
  )
}

# =============================================================================
# Run Application
# =============================================================================

shinyApp(ui = ui, server = server)
