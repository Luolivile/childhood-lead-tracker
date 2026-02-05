# Data Fetch Functions for Lead Surveillance Dashboard
# Functions to download and cache data from NYC and CDC sources

library(httr)
library(readr)
library(readxl)
library(dplyr)

# Configuration
DATA_DIR <- here::here("data")
RAW_DIR <- file.path(DATA_DIR, "raw")
PROCESSED_DIR <- file.path(DATA_DIR, "processed")
CACHE_HOURS <- 24  # Re-download data after this many hours

# Ensure directories exist
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)

#' Check if cached file is still valid
#' @param file_path Path to the cached file
#' @param max_age_hours Maximum age in hours before cache expires
#' @return TRUE if cache is valid, FALSE otherwise
is_cache_valid <- function(file_path, max_age_hours = CACHE_HOURS) {
  if (!file.exists(file_path)) return(FALSE)

  file_age <- difftime(Sys.time(), file.mtime(file_path), units = "hours")
  as.numeric(file_age) < max_age_hours
}

#' Download file with caching
#' @param url URL to download from
#' @param dest_file Destination file path
#' @param force Force re-download even if cache is valid
#' @return Path to the downloaded file
download_with_cache <- function(url, dest_file, force = FALSE) {
  if (!force && is_cache_valid(dest_file)) {
    message("Using cached file: ", basename(dest_file))
    return(dest_file)
  }

  message("Downloading: ", url)
  tryCatch({
    response <- GET(url, write_disk(dest_file, overwrite = TRUE), progress())
    if (http_error(response)) {
      stop("HTTP error: ", status_code(response))
    }
    message("Downloaded successfully: ", basename(dest_file))
    dest_file
  }, error = function(e) {
    warning("Download failed: ", e$message)
    if (file.exists(dest_file)) {
      message("Using existing cached file despite download failure")
      return(dest_file)
    }
    NULL
  })
}

# =============================================================================
# NYC Data Functions
# =============================================================================

#' Fetch NYC lead data from EHDP GitHub repository (production branch)
#' Indicator 2184: Elevated blood lead levels (under age 6)
#' @param force Force re-download
#' @return List with elevated_bll data frame
fetch_nyc_lead_data <- function(force = FALSE) {
  # Source: nychealth/EHDP-data GitHub, production branch
  # Indicator 2184 measures:
  #   795 = children tested (count)
  #   774 = elevated count at >=5 mcg/dL
  #   777 = elevated rate per 1,000 at >=5 mcg/dL
  #   775/778 = count/rate at >=10, 776/779 = count/rate at >=15
  #   1408 = elevated count at >=3.5 mcg/dL (2022+ only)
  #   1409 = elevated rate per 1,000 at >=3.5 mcg/dL (2022+ only)

  data_url <- "https://raw.githubusercontent.com/nychealth/EHDP-data/production/indicators/data/2184.json"
  tp_url <- "https://raw.githubusercontent.com/nychealth/EHDP-data/production/indicators/metadata/TimePeriods.json"

  data_file <- file.path(RAW_DIR, "nyc_ehdp_2184.json")
  tp_file <- file.path(RAW_DIR, "nyc_ehdp_timeperiods.json")

  data_result <- download_with_cache(data_url, data_file, force)
  tp_result <- download_with_cache(tp_url, tp_file, force)

  if (is.null(data_result) || is.null(tp_result)) {
    message("EHDP download failed, using sample NYC data")
    return(list(elevated_bll = create_sample_nyc_data()))
  }

  tryCatch({
    raw <- jsonlite::fromJSON(data_file)
    tp_all <- jsonlite::fromJSON(tp_file)

    df <- as.data.frame(raw)
    tp_years <- tp_all[tp_all$TimeType == "year", c("TimePeriodID", "TimePeriod")]

    # Join to get year
    df <- merge(df, tp_years, by = "TimePeriodID", all.x = TRUE)
    df$year <- as.integer(df$TimePeriod)
    df <- df[!is.na(df$year), ]

    # GeoID to borough name mapping
    borough_map <- data.frame(
      GeoID = 1:5,
      geography_name = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
      stringsAsFactors = FALSE
    )

    # UHF42 neighborhood name mapping
    uhf42_map <- data.frame(
      GeoID = c(101,102,103,104,105,106,107,
                201,202,203,204,205,206,207,208,209,210,211,
                301,302,303,304,305,306,307,308,309,310,
                401,402,403,404,405,406,407,408,409,410,
                501,502,503,504),
      geography_name = c(
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
        "South Beach-Tottenville"),
      stringsAsFactors = FALSE
    )

    # Helper to build dataset for a given threshold
    build_threshold_data <- function(count_mid, rate_mid, threshold_val, threshold_label) {
      count_df <- df[df$MeasureID == count_mid, c("GeoID", "GeoType", "year", "Value")]
      rate_df <- df[df$MeasureID == rate_mid, c("GeoID", "GeoType", "year", "Value")]
      tested_df <- df[df$MeasureID == 795, c("GeoID", "GeoType", "year", "Value")]

      if (nrow(count_df) == 0 || nrow(rate_df) == 0) return(NULL)

      names(count_df)[4] <- "elevated_count"
      names(rate_df)[4] <- "rate_per_1000"
      names(tested_df)[4] <- "children_tested"

      m <- merge(tested_df, count_df, by = c("GeoID", "GeoType", "year"), all.x = TRUE)
      m <- merge(m, rate_df, by = c("GeoID", "GeoType", "year"), all.x = TRUE)
      # Keep only rows that have the threshold data
      m <- m[!is.na(m$elevated_count), ]
      m$elevated_rate <- m$rate_per_1000 / 10  # convert per-1000 to percent
      m$reference_threshold <- threshold_val
      m$threshold_note <- threshold_label
      m
    }

    # Build >=5 threshold data (all years)
    data_5 <- build_threshold_data(774, 777, 5, "\u22655 \u03bcg/dL")
    # Build >=3.5 threshold data (2022+ only)
    data_3_5 <- build_threshold_data(1408, 1409, 3.5, "\u22653.5 \u03bcg/dL")

    all_threshold <- rbind(data_5, data_3_5)
    if (is.null(all_threshold) || nrow(all_threshold) == 0) {
      message("No valid EHDP data parsed, using sample NYC data")
      return(list(elevated_bll = create_sample_nyc_data()))
    }

    # Map GeoIDs to names by GeoType
    result_parts <- list()

    # Borough data
    boro <- all_threshold[all_threshold$GeoType == "Borough", ]
    if (nrow(boro) > 0) {
      boro <- merge(boro, borough_map, by = "GeoID")
      boro$geography_type <- "Borough"
      result_parts$boro <- boro
    }

    # Citywide data
    cw <- all_threshold[all_threshold$GeoType == "Citywide", ]
    if (nrow(cw) > 0) {
      cw$geography_name <- "New York City"
      cw$geography_type <- "Citywide"
      result_parts$cw <- cw
    }

    # UHF42 data
    uhf <- all_threshold[all_threshold$GeoType == "UHF42", ]
    if (nrow(uhf) > 0) {
      uhf <- merge(uhf, uhf42_map, by = "GeoID", all.x = TRUE)
      # Fallback name for any unmapped GeoIDs
      uhf$geography_name[is.na(uhf$geography_name)] <-
        paste0("UHF42-", uhf$GeoID[is.na(uhf$geography_name)])
      uhf$geography_type <- "UHF42"
      result_parts$uhf <- uhf
    }

    out_cols <- c("geography_name", "geography_type", "year",
                  "children_tested", "elevated_count", "elevated_rate",
                  "reference_threshold", "threshold_note")

    result <- do.call(rbind, lapply(result_parts, function(x) x[, out_cols]))
    result$data_source <- "NYC EHDP"
    rownames(result) <- NULL

    message("Loaded ", nrow(result), " NYC EHDP records (",
            length(unique(result$year)), " years, ",
            length(unique(result$geography_name)), " geographies)")

    list(elevated_bll = result)
  }, error = function(e) {
    warning("Failed to parse EHDP data: ", e$message)
    message("Using sample NYC data for demonstration")
    list(elevated_bll = create_sample_nyc_data())
  })
}

#' Fetch NYC geographic boundaries (for mapping)
#' @param geo_type Type of geography: "borough", "uhf", "cd", "nta"
#' @param force Force re-download
#' @return sf object with boundaries
fetch_nyc_geo_boundaries <- function(geo_type = "nta", force = FALSE) {
  # NYC Open Data has geographic boundary files
  geo_urls <- list(
    borough = "https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON",
    nta = "https://data.cityofnewyork.us/api/geospatial/9nt8-h7nd?method=export&format=GeoJSON",
    cd = "https://data.cityofnewyork.us/api/geospatial/jp9i-3b7y?method=export&format=GeoJSON"
  )

  if (!geo_type %in% names(geo_urls)) {
    warning("Unknown geography type: ", geo_type, ". Using NTA.")
    geo_type <- "nta"
  }

  geo_file <- file.path(RAW_DIR, paste0("nyc_", geo_type, "_boundaries.geojson"))

  result <- download_with_cache(geo_urls[[geo_type]], geo_file, force)
  if (!is.null(result) && file.exists(result)) {
    tryCatch({
      sf::st_read(geo_file, quiet = TRUE)
    }, error = function(e) {
      warning("Failed to read NYC ", geo_type, " boundaries: ", e$message)
      NULL
    })
  } else {
    NULL
  }
}

# =============================================================================
# NYS Open Data Functions
# =============================================================================

#' Fetch NYS childhood blood lead testing data from NYS Open Data
#' Dataset: Childhood Blood Lead Testing and Elevated Incidence by Birth Year and Zip Code
#' https://health.data.ny.gov/Health/Childhood-Blood-Lead-Testing-and-Elevated-Incidenc/dyed-4zxh
#' @param force Force re-download
#' @param min_year Minimum year to fetch (default 2000)
#' @return Data frame with NYS lead data by county, ZIP, and birth year
fetch_nys_lead_data <- function(force = FALSE, min_year = 2000) {
  # Socrata Open Data API endpoint — birth year dataset (goes to 2021)
  # Fetch in two batches since dataset has ~130k records
  base_url <- "https://health.data.ny.gov/resource/dyed-4zxh.json"

  all_data <- NULL

  offsets <- as.integer(c(0, 50000, 100000))
  for (offset in offsets) {
    batch_file <- file.path(RAW_DIR, paste0("nys_lead_data_", as.integer(offset), ".json"))

    api_url <- paste0(
      base_url,
      "?$limit=50000",
      "&$offset=", format(offset, scientific = FALSE),
      "&$where=year>='", min_year, "'",
      "&$select=county,year,tests,total_elevated_blood_levels",
      "&$order=year,county"
    )

    result <- download_with_cache(api_url, batch_file, force)
    if (!is.null(result) && file.exists(result)) {
      batch <- tryCatch({
        df <- jsonlite::fromJSON(batch_file)
        if (nrow(df) > 0 && "county" %in% names(df)) df else NULL
      }, error = function(e) NULL)

      if (!is.null(batch) && nrow(batch) > 0) {
        all_data <- if (is.null(all_data)) batch else rbind(all_data, batch)
      } else {
        break  # No more data
      }
    }
  }

  if (!is.null(all_data) && nrow(all_data) > 0) {
    message("Fetched ", nrow(all_data), " NYS records, aggregating by county and year")
    # Aggregate across birth years and zip codes to get county-year totals
    nys_data <- all_data %>%
      mutate(
        tests = as.numeric(tests),
        total_eblls = as.numeric(total_elevated_blood_levels)
      ) %>%
      group_by(county, year) %>%
      summarise(
        tests = sum(tests, na.rm = TRUE),
        total_eblls = sum(total_eblls, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        year = as.integer(year),
        percent = ifelse(tests > 0, (total_eblls / tests) * 100, 0),
        rate_per_1_000 = ifelse(tests > 0, (total_eblls / tests) * 1000, 0)
      )
  } else {
    nys_data <- NULL
  }

  if (is.null(nys_data)) {
    message("NYS Open Data fetch failed, using sample NYS data")
    nys_data <- create_sample_nys_data()
  }

  nys_data
}

# =============================================================================
# CDC National Data Functions
# =============================================================================

#' Parse the CDC CBLS national Excel file into state and national data
#' The file has a merged header; actual column layout (17 cols):
#'   Year, State, Total Population, Number Tested,
#'   Num >=3.5, Pct >=3.5, Num >=5, Pct >=5,
#'   Num >=10, Pct >=10, Num >=15, Pct >=15,
#'   Num >=20, Pct >=20, Num >=25, Pct >=25,
#'   Num >=45, Pct >=45   (last pair may be absent)
#' @param file_path Path to downloaded xlsx
#' @return List with $state and $national data frames, or NULL
parse_cdc_excel <- function(file_path) {
  tryCatch({
    raw <- read_excel(file_path, sheet = 1, col_names = FALSE, skip = 2)
    if (ncol(raw) < 10) return(NULL)

    # Assign column names based on known layout
    col_names <- c("year", "state", "total_population", "children_tested",
                   "num_ge_3_5", "pct_ge_3_5", "num_ge_5", "pct_ge_5",
                   "num_ge_10", "pct_ge_10", "num_ge_15", "pct_ge_15",
                   "num_ge_20", "pct_ge_20", "num_ge_25", "pct_ge_25",
                   "num_ge_45", "pct_ge_45")
    names(raw) <- col_names[1:ncol(raw)]

    # Keep only data rows (year is a 4-digit number)
    raw <- raw %>% filter(!is.na(year) & grepl("^\\d{4}$", year))

    # Clean percentage columns: remove "%" and convert
    pct_cols <- grep("^pct_", names(raw), value = TRUE)
    for (col in pct_cols) {
      raw[[col]] <- as.numeric(gsub("[%,]", "", raw[[col]]))
    }

    # Clean numeric columns: remove commas
    num_cols <- c("total_population", "children_tested",
                  grep("^num_", names(raw), value = TRUE))
    for (col in num_cols) {
      raw[[col]] <- as.numeric(gsub("[,]", "", raw[[col]]))
    }
    raw$year <- as.integer(raw$year)

    # Split into state data and U.S. totals
    us_rows <- grepl("U\\.S\\.", raw$state, ignore.case = TRUE)

    state_data <- raw[!us_rows, ] %>%
      # Clean state names (remove trailing markers like *)
      mutate(state = gsub("[*\u2021\u01C1]", "", state) %>% trimws())

    national_data <- raw[us_rows, ] %>%
      mutate(state = "United States")

    message("Loaded CDC data: ", nrow(state_data), " state rows, ",
            nrow(national_data), " national rows")

    list(state = state_data, national = national_data)
  }, error = function(e) {
    warning("Failed to parse CDC Excel: ", e$message)
    NULL
  })
}

#' Fetch CDC state surveillance data
#' @param force Force re-download
#' @return Data frame with state-level lead surveillance data
fetch_cdc_state_data <- function(force = FALSE) {
  state_file <- file.path(RAW_DIR, "cdc_cbls_national_data.xlsx")

  cdc_url <- "https://www.cdc.gov/lead-prevention/media/files/2025/08/2017-2022-cbls-national-data-508-1.xlsx"

  result <- download_with_cache(cdc_url, state_file, force)
  if (!is.null(result) && file.exists(result)) {
    parsed <- parse_cdc_excel(state_file)
    if (!is.null(parsed) && !is.null(parsed$state) && nrow(parsed$state) > 0) {
      return(parsed$state)
    }
  }

  message("Using embedded sample CDC state data for demonstration")
  create_sample_cdc_state_data()
}

#' Fetch CDC national summary data
#' @param force Force re-download
#' @return Data frame with national summary data
fetch_cdc_national_data <- function(force = FALSE) {
  # Same Excel file as state data — check if already downloaded
  state_file <- file.path(RAW_DIR, "cdc_cbls_national_data.xlsx")

  cdc_url <- "https://www.cdc.gov/lead-prevention/media/files/2025/08/2017-2022-cbls-national-data-508-1.xlsx"

  result <- download_with_cache(cdc_url, state_file, force)
  if (!is.null(result) && file.exists(result)) {
    parsed <- parse_cdc_excel(state_file)
    if (!is.null(parsed) && !is.null(parsed$national) && nrow(parsed$national) > 0) {
      return(parsed$national)
    }
  }

  message("Using embedded sample CDC national data for demonstration")
  create_sample_cdc_national_data()
}

#' Fetch US state boundaries for mapping
#' @param force Force re-download
#' @return sf object with state boundaries
fetch_us_state_boundaries <- function(force = FALSE) {
  # Use Census Bureau TIGER/Line files
  states_file <- file.path(RAW_DIR, "us_states_boundaries.geojson")

  # Simplified US states GeoJSON from public source
  states_url <- "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"

  result <- download_with_cache(states_url, states_file, force)
  if (!is.null(result) && file.exists(result)) {
    tryCatch({
      sf::st_read(states_file, quiet = TRUE)
    }, error = function(e) {
      warning("Failed to read US state boundaries: ", e$message)
      NULL
    })
  } else {
    NULL
  }
}

# =============================================================================
# Sample Data Generation (for demo/testing when real data unavailable)
# =============================================================================

#' Create sample CDC state data for demonstration
create_sample_cdc_state_data <- function() {
  states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
              "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
              "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
              "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
              "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
              "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
              "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
              "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
              "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
              "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

  years <- 2017:2022

  # Generate realistic-looking sample data
  set.seed(42)
  expand.grid(state = states, year = years) %>%
    mutate(
      children_tested = round(runif(n(), 10000, 500000)),
      # Rates generally declining over time
      elevated_rate_5 = pmax(0.5, 3.5 - (year - 2017) * 0.3 + rnorm(n(), 0, 0.8)),
      elevated_rate_3_5 = elevated_rate_5 * 1.3,  # More children at lower threshold
      threshold_used = ifelse(year >= 2022, 3.5, 5),
      # Use appropriate rate based on year/threshold
      elevated_rate = ifelse(year >= 2022, elevated_rate_3_5, elevated_rate_5),
      elevated_count = round(children_tested * elevated_rate / 100)
    ) %>%
    select(state, year, children_tested, elevated_count, elevated_rate, threshold_used)
}

#' Create sample CDC national data for demonstration
create_sample_cdc_national_data <- function() {
  years <- 2017:2022

  set.seed(42)
  data.frame(year = years) %>%
    mutate(
      children_tested = round(runif(n(), 3000000, 4000000)),
      # National rate declining over time
      elevated_rate_5 = pmax(0.8, 2.8 - (year - 2017) * 0.25 + rnorm(n(), 0, 0.3)),
      elevated_rate_3_5 = elevated_rate_5 * 1.35,
      threshold_used = ifelse(year >= 2022, 3.5, 5),
      elevated_rate = ifelse(year >= 2022, elevated_rate_3_5, elevated_rate_5),
      elevated_count = round(children_tested * elevated_rate / 100)
    ) %>%
    select(year, children_tested, elevated_count, elevated_rate, threshold_used)
}

#' Create sample NYC data for demonstration
create_sample_nyc_data <- function() {
  boroughs <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
  years <- 2017:2022

  set.seed(123)
  expand.grid(geography_name = boroughs, year = years, stringsAsFactors = FALSE) %>%
    mutate(
      geography_type = "Borough",
      children_tested = round(runif(n(), 5000, 50000)),
      # NYC rates - Bronx typically higher
      base_rate = case_when(
        geography_name == "Bronx" ~ 4.0,
        geography_name == "Brooklyn" ~ 3.2,
        geography_name == "Manhattan" ~ 2.0,
        geography_name == "Queens" ~ 2.5,
        geography_name == "Staten Island" ~ 2.8,
        TRUE ~ 3.0
      ),
      # Rates declining over time
      elevated_rate = pmax(0.3, base_rate - (year - 2017) * 0.35 + rnorm(n(), 0, 0.4)),
      elevated_count = round(children_tested * elevated_rate / 100),
      reference_threshold = ifelse(year >= 2022, 3.5, 5),
      threshold_note = paste0(">=", reference_threshold, " mcg/dL"),
      data_source = "Sample Data"
    ) %>%
    select(-base_rate) %>%
    # Add citywide totals
    bind_rows(
      data.frame(year = years) %>%
        mutate(
          geography_name = "New York City",
          geography_type = "Citywide",
          children_tested = round(runif(n(), 100000, 200000)),
          elevated_rate = pmax(0.5, 3.0 - (year - 2017) * 0.3 + rnorm(n(), 0, 0.3)),
          elevated_count = round(children_tested * elevated_rate / 100),
          reference_threshold = ifelse(year >= 2022, 3.5, 5),
          threshold_note = paste0(">=", reference_threshold, " mcg/dL"),
          data_source = "Sample Data"
        )
    )
}

#' Create sample NYS data for demonstration
create_sample_nys_data <- function() {
  counties <- c("Albany", "Bronx", "Erie", "Kings", "Monroe",
                "Nassau", "New York", "Oneida", "Onondaga", "Orange",
                "Queens", "Richmond", "Rockland", "Suffolk", "Westchester")
  years <- 2005:2020

  set.seed(99)
  expand.grid(county = counties, year = years, stringsAsFactors = FALSE) %>%
    mutate(
      tests = round(runif(n(), 500, 20000)),
      rate_base = case_when(
        county %in% c("Bronx", "Kings", "New York") ~ 6.0,
        county %in% c("Erie", "Oneida", "Onondaga") ~ 5.0,
        TRUE ~ 3.5
      ),
      percent = pmax(0.5, rate_base - (year - 2005) * 0.25 + rnorm(n(), 0, 0.8)),
      total_eblls = round(tests * percent / 100),
      rate_per_1_000 = round(percent * 10, 1),
      less_than_5_mcg_dl = tests - total_eblls,
      fips = as.character(match(county, counties))
    ) %>%
    select(county, fips, year, tests, less_than_5_mcg_dl, total_eblls, percent, rate_per_1_000)
}

# =============================================================================
# Main Data Fetch Function
# =============================================================================

#' Fetch all data sources
#' @param force Force re-download of all data
#' @return List containing all fetched data
fetch_all_data <- function(force = FALSE) {
  message("Fetching all data sources...")

  list(
    nyc = fetch_nyc_lead_data(force),
    nys = fetch_nys_lead_data(force),
    cdc_state = fetch_cdc_state_data(force),
    cdc_national = fetch_cdc_national_data(force),
    geo = list(
      nyc_nta = fetch_nyc_geo_boundaries("nta", force),
      nyc_borough = fetch_nyc_geo_boundaries("borough", force),
      us_states = fetch_us_state_boundaries(force)
    )
  )
}
