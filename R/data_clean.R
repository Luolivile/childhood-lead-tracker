# Data Cleaning Functions for Lead Surveillance Dashboard
# Standardizes and processes data from multiple sources

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Reference value change information
# CDC changed the blood lead reference value from 5 μg/dL to 3.5 μg/dL in October 2021
REFERENCE_VALUE_CHANGE_DATE <- as.Date("2021-10-01")
OLD_REFERENCE_VALUE <- 5.0
NEW_REFERENCE_VALUE <- 3.5

#' Clean and standardize NYC lead data
#' @param nyc_data List containing NYC data frames from fetch_nyc_lead_data()
#' @return Cleaned and standardized data frame
clean_nyc_data <- function(nyc_data) {
  if (is.null(nyc_data)) return(NULL)

  # If data already has standard columns (from EHDP JSON fetch), pass through
  if (!is.null(nyc_data$elevated_bll) &&
      all(c("geography_name", "geography_type", "year", "elevated_rate") %in%
          names(nyc_data$elevated_bll))) {
    df <- nyc_data$elevated_bll %>%
      mutate(
        year = as.numeric(year),
        elevated_rate = as.numeric(elevated_rate),
        children_tested = as.numeric(children_tested),
        elevated_count = as.numeric(elevated_count)
      )
    return(df)
  }

  # Legacy path for old CSV-format data
  cleaned_list <- list()

  if (!is.null(nyc_data$elevated_bll)) {
    cleaned_list$elevated_bll <- nyc_data$elevated_bll %>%
      clean_column_names() %>%
      standardize_geography_names() %>%
      add_reference_threshold() %>%
      mutate(
        data_source = "NYC EHDP",
        indicator = "elevated_bll"
      )
  }

  if (!is.null(nyc_data$tested)) {
    cleaned_list$tested <- nyc_data$tested %>%
      clean_column_names() %>%
      standardize_geography_names() %>%
      mutate(
        data_source = "NYC EHDP",
        indicator = "children_tested"
      )
  }

  combine_nyc_datasets(cleaned_list)
}

#' Clean column names to standard format
#' @param df Data frame to clean
#' @return Data frame with standardized column names
clean_column_names <- function(df) {
  df %>%
    rename_with(~ tolower(.) %>%
                  str_replace_all("[^a-z0-9]", "_") %>%
                  str_replace_all("_+", "_") %>%
                  str_remove("^_") %>%
                  str_remove("_$"))
}

#' Standardize geography names across datasets
#' @param df Data frame with geography column
#' @return Data frame with standardized geography names
standardize_geography_names <- function(df) {
  # If already has standard column names, return as-is
  if ("geography_name" %in% names(df) && "geography_type" %in% names(df)) {
    return(df)
  }

  # Common geography column names to look for
  geo_cols <- c("geoname", "geography", "geo_name", "geo", "location",
                "neighborhood", "borough", "nta", "uhf", "cd")

  existing_geo_col <- intersect(names(df), geo_cols)[1]

  if (!is.na(existing_geo_col) && !"geography_name" %in% names(df)) {
    df <- df %>%
      rename(geography_name = !!sym(existing_geo_col))
  }

  # Look for geography type column
  geo_type_cols <- c("geotype", "geo_type", "geoentitytype")
  existing_type_col <- intersect(names(df), geo_type_cols)[1]

  if (!is.na(existing_type_col)) {
    df <- df %>%
      rename(geography_type = !!sym(existing_type_col)) %>%
      mutate(
        geography_type = case_when(
          str_detect(tolower(geography_type), "borough") ~ "Borough",
          str_detect(tolower(geography_type), "nta|neighborhood tabulation") ~ "NTA",
          str_detect(tolower(geography_type), "uhf|united hospital") ~ "UHF",
          str_detect(tolower(geography_type), "cd|community district") ~ "CD",
          str_detect(tolower(geography_type), "city") ~ "Citywide",
          TRUE ~ geography_type
        )
      )
  }

  df
}

#' Add reference threshold information based on year
#' @param df Data frame with year column
#' @return Data frame with threshold information
add_reference_threshold <- function(df) {
  # If year column already exists, just ensure it's numeric
  if ("year" %in% names(df)) {
    df <- df %>% mutate(year = as.numeric(year))
  } else {
    # Look for year column with different names
    year_cols <- c("time", "year_number", "yearnum")
    existing_year_col <- intersect(names(df), year_cols)[1]

    if (!is.na(existing_year_col)) {
      df <- df %>%
        rename(year = !!sym(existing_year_col)) %>%
        mutate(year = as.numeric(year))
    } else {
      # Try to extract year from time/period column
      time_cols <- c("time", "period", "timeperiod", "time_period")
      time_col <- intersect(names(df), time_cols)[1]

      if (!is.na(time_col)) {
        df <- df %>%
          mutate(year = as.numeric(str_extract(!!sym(time_col), "\\d{4}")))
      } else {
        df$year <- NA_integer_
      }
    }
  }

  # Add/update reference threshold columns
  df %>%
    mutate(
      reference_threshold = ifelse(year >= 2022, NEW_REFERENCE_VALUE, OLD_REFERENCE_VALUE),
      threshold_note = ifelse(
        year >= 2022,
        paste0("≥", NEW_REFERENCE_VALUE, " μg/dL (post-2021 standard)"),
        paste0("≥", OLD_REFERENCE_VALUE, " μg/dL (pre-2022 standard)")
      )
    )
}

#' Combine NYC datasets into unified format
#' @param cleaned_list List of cleaned NYC data frames
#' @return Unified data frame
combine_nyc_datasets <- function(cleaned_list) {
  # If we have the elevated BLL data, use it as primary
  if (!is.null(cleaned_list$elevated_bll)) {
    primary_df <- cleaned_list$elevated_bll

    # Standardize rate column names
    rate_cols <- c("rate", "data_value", "value", "percent", "percentage")
    existing_rate_col <- intersect(names(primary_df), rate_cols)[1]

    if (!is.na(existing_rate_col)) {
      primary_df <- primary_df %>%
        rename(elevated_rate = !!sym(existing_rate_col))
    }

    # Standardize count column names
    count_cols <- c("count", "number", "n", "numerator")
    existing_count_col <- intersect(names(primary_df), count_cols)[1]

    if (!is.na(existing_count_col)) {
      primary_df <- primary_df %>%
        rename(elevated_count = !!sym(existing_count_col))
    }

    return(primary_df)
  }

  NULL
}

#' Clean and standardize NYS Open Data lead data
#' @param nys_data Data frame from fetch_nys_lead_data()
#' @return Cleaned and standardized data frame
clean_nys_data <- function(nys_data) {
  if (is.null(nys_data)) return(NULL)

  nys_data %>%
    mutate(
      data_source = "NYS Open Data",
      county = as.character(county),
      year = as.integer(year),
      children_tested = as.numeric(tests),
      elevated_count = as.numeric(total_eblls),
      elevated_rate = as.numeric(percent),
      rate_per_1000 = as.numeric(rate_per_1_000),
      # NYS data uses 5 mcg/dL threshold (data goes to 2021)
      threshold_used = 5.0,
      threshold_note = "≥5 μg/dL"
    ) %>%
    filter(!is.na(county) & !is.na(year)) %>%
    select(county, year, children_tested, elevated_count, elevated_rate,
           rate_per_1000, threshold_used, threshold_note, data_source)
}

#' Aggregate NYS data to statewide totals by year
#' @param nys_clean Cleaned NYS data from clean_nys_data()
#' @return Data frame with one row per year for NY State
aggregate_nys_statewide <- function(nys_clean) {
  if (is.null(nys_clean)) return(NULL)

  nys_clean %>%
    group_by(year, threshold_used, threshold_note, data_source) %>%
    summarise(
      children_tested = sum(children_tested, na.rm = TRUE),
      elevated_count = sum(elevated_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      elevated_rate = (elevated_count / children_tested) * 100,
      state = "New York"
    )
}

#' Clean and standardize CDC state data
#' @param state_data Data frame from fetch_cdc_state_data()
#' @return Cleaned and standardized data frame
clean_cdc_state_data <- function(state_data) {
  if (is.null(state_data)) return(NULL)

  # Handle real CDC Excel format (has num_ge_5, pct_ge_5, etc.)
  if ("num_ge_5" %in% names(state_data)) {
    state_data %>%
      mutate(
        data_source = "CDC CBLS",
        state = str_to_title(str_trim(state)),
        # Fix "District Of Columbia" -> "District of Columbia" to match GeoJSON
        state = gsub(" Of ", " of ", state),
        state_abbr = state.abb[match(state, state.name)],
        state_abbr = ifelse(is.na(state_abbr), state, state_abbr),
        year = as.integer(year),
        children_tested = as.numeric(children_tested),
        elevated_count = as.numeric(num_ge_5),
        elevated_rate = as.numeric(pct_ge_5),
        threshold_used = 5,
        threshold_note = "\u22655 \u03bcg/dL"
      ) %>%
      filter(!is.na(state) & !is.na(year) &
               !grepl("U\\.S\\.|Total|New York City|Puerto Rico", state))
  } else {
    # Fallback for sample data format
    state_data %>%
      clean_column_names() %>%
      mutate(
        data_source = "CDC State Surveillance",
        state = str_to_title(str_trim(state)),
        state_abbr = state.abb[match(state, state.name)],
        state_abbr = ifelse(is.na(state_abbr), state, state_abbr),
        year = as.integer(year),
        children_tested = as.numeric(children_tested),
        elevated_count = as.numeric(elevated_count),
        elevated_rate = as.numeric(elevated_rate),
        threshold_used = as.numeric(threshold_used),
        threshold_note = paste0("\u2265", threshold_used, " \u03bcg/dL")
      ) %>%
      filter(!is.na(state) & !is.na(year))
  }
}

#' Clean and standardize CDC national data
#' @param national_data Data frame from fetch_cdc_national_data()
#' @return Cleaned and standardized data frame
clean_cdc_national_data <- function(national_data) {
  if (is.null(national_data)) return(NULL)

  # Handle real CDC Excel format
  if ("num_ge_5" %in% names(national_data)) {
    result <- national_data %>%
      mutate(
        data_source = "CDC CBLS",
        geography_name = "United States",
        geography_type = "National",
        year = as.integer(year),
        children_tested = as.numeric(children_tested),
        elevated_count = as.numeric(num_ge_5),
        elevated_rate = as.numeric(pct_ge_5),
        threshold_used = 5,
        threshold_note = "\u22655 \u03bcg/dL"
      ) %>%
      filter(!is.na(year))

    # Excel stores national pct as decimal fractions (e.g. 0.015 = 1.5%)
    # while state rows have text "1.5%". Detect and fix.
    if (max(result$elevated_rate, na.rm = TRUE) < 1) {
      result$elevated_rate <- result$elevated_rate * 100
    }

    result
  } else {
    # Fallback for sample data format
    national_data %>%
      clean_column_names() %>%
      mutate(
        data_source = "CDC National Surveillance",
        geography_name = "United States",
        geography_type = "National",
        year = as.integer(year),
        children_tested = as.numeric(children_tested),
        elevated_count = as.numeric(elevated_count),
        elevated_rate = as.numeric(elevated_rate),
        threshold_used = as.numeric(threshold_used),
        threshold_note = paste0("\u2265", threshold_used, " \u03bcg/dL")
      ) %>%
      filter(!is.na(year))
  }
}

#' Create comparison dataset combining NYC, NYS, and national data
#' @param nyc_data Cleaned NYC data
#' @param nys_statewide Aggregated NYS statewide data (from aggregate_nys_statewide)
#' @param cdc_state_data Cleaned CDC state data
#' @param cdc_national_data Cleaned CDC national data
#' @return Combined data frame for comparison
create_comparison_data <- function(nyc_data, nys_statewide = NULL, cdc_state_data = NULL, cdc_national_data = NULL) {
  comparison_list <- list()

  # NYC citywide data
  if (!is.null(nyc_data) && nrow(nyc_data) > 0) {
    # Check if geography columns exist, add defaults if not
    if (!"geography_type" %in% names(nyc_data)) {
      nyc_data$geography_type <- "Unknown"
    }
    if (!"geography_name" %in% names(nyc_data)) {
      nyc_data$geography_name <- "Unknown"
    }

    nyc_citywide <- nyc_data %>%
      filter(
        str_detect(tolower(geography_type), "city") |
          str_detect(tolower(geography_name), "new york city|nyc|citywide")
      )

    # Use >=5 threshold for comparison (consistent across full time series)
    if ("reference_threshold" %in% names(nyc_citywide)) {
      nyc_citywide <- nyc_citywide %>% filter(reference_threshold == 5)
    }

    nyc_citywide <- nyc_citywide %>%
      mutate(
        region = "NYC",
        geography_type = "City"
      ) %>%
      select(any_of(c("year", "region", "geography_type", "elevated_rate",
                      "elevated_count", "reference_threshold", "threshold_note",
                      "data_source")))

    if (nrow(nyc_citywide) > 0) {
      comparison_list$nyc <- nyc_citywide
    }
  }

  # NY State data — prefer real NYS Open Data over CDC
  if (!is.null(nys_statewide) && nrow(nys_statewide) > 0) {
    ny_state <- nys_statewide %>%
      mutate(
        region = "NY State",
        geography_type = "State",
        reference_threshold = threshold_used
      ) %>%
      select(any_of(c("year", "region", "geography_type", "elevated_rate",
                      "elevated_count", "reference_threshold", "threshold_note",
                      "data_source")))

    if (nrow(ny_state) > 0) {
      comparison_list$ny_state <- ny_state
    }
  } else if (!is.null(cdc_state_data)) {
    # Fallback to CDC state data for NY
    ny_state <- cdc_state_data %>%
      filter(state == "New York") %>%
      mutate(
        region = "NY State",
        geography_type = "State",
        reference_threshold = threshold_used
      ) %>%
      select(any_of(c("year", "region", "geography_type", "elevated_rate",
                      "elevated_count", "reference_threshold", "threshold_note",
                      "data_source")))

    if (nrow(ny_state) > 0) {
      comparison_list$ny_state <- ny_state
    }
  }

  # National data
  if (!is.null(cdc_national_data)) {
    national <- cdc_national_data %>%
      mutate(
        region = "National",
        reference_threshold = threshold_used
      ) %>%
      select(any_of(c("year", "region", "geography_type", "elevated_rate",
                      "elevated_count", "reference_threshold", "threshold_note",
                      "data_source")))

    if (nrow(national) > 0) {
      comparison_list$national <- national
    }
  }

  # Combine all
  if (length(comparison_list) > 0) {
    bind_rows(comparison_list)
  } else {
    NULL
  }
}

#' Calculate adjusted rates for threshold comparison
#' @param df Data frame with rates at different thresholds
#' @param target_threshold Target threshold for adjustment
#' @return Data frame with adjusted rates
#' @note This is an approximation; actual adjustment requires raw data
adjust_rates_for_threshold <- function(df, target_threshold = 5.0) {
  # Note: This is an approximation based on typical BLL distributions

# Actual adjustment would require access to individual-level data

  adjustment_factor <- ifelse(
    target_threshold == 5.0,
    1.0,  # No adjustment for 5 μg/dL
    0.75  # Approximate factor for 5 -> 3.5 (reverse adjustment)
  )

  df %>%
    mutate(
      adjusted_rate = case_when(
        reference_threshold == target_threshold ~ elevated_rate,
        reference_threshold == 3.5 & target_threshold == 5.0 ~ elevated_rate * 0.75,
        reference_threshold == 5.0 & target_threshold == 3.5 ~ elevated_rate * 1.35,
        TRUE ~ elevated_rate
      ),
      adjustment_note = ifelse(
        reference_threshold != target_threshold,
        paste0("Adjusted from ", reference_threshold, " to ", target_threshold, " μg/dL threshold"),
        "No adjustment needed"
      )
    )
}

#' Process all data through cleaning pipeline
#' @param raw_data List of raw data from fetch_all_data()
#' @return List of cleaned data frames
clean_all_data <- function(raw_data) {
  message("Cleaning all data sources...")

  nyc_clean <- clean_nyc_data(raw_data$nyc)
  nys_clean <- clean_nys_data(raw_data$nys)
  nys_statewide <- aggregate_nys_statewide(nys_clean)
  cdc_state_clean <- clean_cdc_state_data(raw_data$cdc_state)
  cdc_national_clean <- clean_cdc_national_data(raw_data$cdc_national)

  comparison <- create_comparison_data(nyc_clean, nys_statewide, cdc_state_clean, cdc_national_clean)

  list(
    nyc = nyc_clean,
    nys = nys_clean,
    nys_statewide = nys_statewide,
    cdc_state = cdc_state_clean,
    cdc_national = cdc_national_clean,
    comparison = comparison,
    geo = raw_data$geo
  )
}

#' Save processed data to disk
#' @param cleaned_data List of cleaned data frames
#' @param output_dir Directory to save processed data
save_processed_data <- function(cleaned_data, output_dir = NULL) {
  if (is.null(output_dir)) {
    output_dir <- here::here("data", "processed")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Save each dataset
  if (!is.null(cleaned_data$nyc)) {
    saveRDS(cleaned_data$nyc, file.path(output_dir, "nyc_lead_data.rds"))
  }

  if (!is.null(cleaned_data$nys)) {
    saveRDS(cleaned_data$nys, file.path(output_dir, "nys_lead_data.rds"))
  }

  if (!is.null(cleaned_data$nys_statewide)) {
    saveRDS(cleaned_data$nys_statewide, file.path(output_dir, "nys_statewide_data.rds"))
  }

  if (!is.null(cleaned_data$cdc_state)) {
    saveRDS(cleaned_data$cdc_state, file.path(output_dir, "cdc_state_data.rds"))
  }

  if (!is.null(cleaned_data$cdc_national)) {
    saveRDS(cleaned_data$cdc_national, file.path(output_dir, "cdc_national_data.rds"))
  }

  if (!is.null(cleaned_data$comparison)) {
    saveRDS(cleaned_data$comparison, file.path(output_dir, "comparison_data.rds"))
  }

  message("Processed data saved to: ", output_dir)
}

#' Load processed data from disk
#' @param input_dir Directory containing processed data
#' @return List of data frames
load_processed_data <- function(input_dir = NULL) {
  if (is.null(input_dir)) {
    input_dir <- here::here("data", "processed")
  }

  list(
    nyc = tryCatch(readRDS(file.path(input_dir, "nyc_lead_data.rds")), error = function(e) NULL),
    nys = tryCatch(readRDS(file.path(input_dir, "nys_lead_data.rds")), error = function(e) NULL),
    nys_statewide = tryCatch(readRDS(file.path(input_dir, "nys_statewide_data.rds")), error = function(e) NULL),
    cdc_state = tryCatch(readRDS(file.path(input_dir, "cdc_state_data.rds")), error = function(e) NULL),
    cdc_national = tryCatch(readRDS(file.path(input_dir, "cdc_national_data.rds")), error = function(e) NULL),
    comparison = tryCatch(readRDS(file.path(input_dir, "comparison_data.rds")), error = function(e) NULL)
  )
}
