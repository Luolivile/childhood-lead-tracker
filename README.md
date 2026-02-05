# Childhood Lead Tracker Dashboard

An interactive R Shiny dashboard for childhood blood lead level (BLL) surveillance, comparing NYC, NY State, and national (CDC) data.

**[View Live Dashboard](https://luolivile.shinyapps.io/childhood-lead-tracker/)**

## Overview

This dashboard provides visualization and analysis of childhood lead exposure data from multiple sources:

- **NYC Data**: Borough and UHF42 neighborhood level data from NYC Environment & Health Data Portal (2005-2024)
- **NY State Data**: County-level data from NYS Open Data (2000-2021)
- **National Data**: State-level data from CDC Childhood Blood Lead Surveillance (2017-2022)

## Features

- Interactive maps showing geographic distribution of elevated blood lead levels
- Time series analysis of lead exposure trends
- Comparison between NYC, NY State, and national averages
- Filters for year, geography level, and BLL thresholds (5 and 3.5 μg/dL)
- Data tables with export options

## Important Notes

- The CDC reference value for elevated BLL changed from 5 μg/dL to 3.5 μg/dL in October 2021
- NYC data includes both thresholds for 2022+ allowing comparison
- NYC does not publish ZIP-code level data; UHF42/Borough is the finest available geography
- NYS Open Data excludes NYC (NYC has its own reporting system)

## Installation

### Prerequisites

- R (>= 4.0.0)
- RStudio (recommended)

### Setup

1. Clone the repository:
```bash
git clone https://github.com/Luolivile/childhood-lead-tracker.git
cd childhood-lead-tracker
```

2. Install required R packages:
```r
install.packages(c("shiny", "shinydashboard", "dplyr", "tidyr", "ggplot2",
                   "plotly", "leaflet", "sf", "httr", "readr", "readxl",
                   "jsonlite", "lubridate", "scales", "stringr", "DT",
                   "shinycssloaders", "here"))
```

3. Run the app:
```r
shiny::runApp()
```

The app will automatically download and cache data from all sources on first run.

## Data Sources

| Source | Coverage | Years | Geography | URL |
|--------|----------|-------|-----------|-----|
| NYC EHDP | NYC | 2005-2024 | Borough, UHF42, Citywide | [GitHub](https://github.com/nychealth/EHDP-data) |
| NYS Open Data | NY State (excl. NYC) | 2000-2021 | County | [Dataset](https://health.data.ny.gov/Health/Childhood-Blood-Lead-Testing-and-Elevated-Incidenc/dyed-4zxh) |
| CDC CBLS | United States | 2017-2022 | State, National | [CDC](https://www.cdc.gov/lead-prevention/php/data/national-surveillance-data.html) |

## Project Structure

```
childhood-lead-tracker/
├── app.R                     # Main Shiny app entry point
├── R/
│   ├── data_fetch.R          # Functions to download/update data
│   ├── data_clean.R          # Data cleaning and standardization
│   ├── utils.R               # Helper functions
│   └── modules/
│       ├── mod_nyc.R         # NYC data visualization module
│       ├── mod_national.R    # CDC national data module
│       └── mod_comparison.R  # NYC vs national comparison module
├── data/
│   ├── raw/                  # Downloaded raw data (gitignored)
│   └── processed/            # Cleaned data for app (gitignored)
└── www/
    └── styles.css            # Custom styling
```

## License

MIT License - see LICENSE file for details.

## Contributing

Contributions are welcome! Please open an issue or submit a pull request.
