# Childhood Lead Tracker Dashboard

An interactive R Shiny dashboard for childhood blood lead level (BLL) surveillance, comparing NYC-level and national (CDC) data.

## Overview

This dashboard provides visualization and analysis of childhood lead exposure data from multiple sources:

- **NYC Data**: Borough, UHF neighborhoods, Community Districts, and Neighborhood Tabulation Area (NTA) level data from NYC Environment & Health Data Portal
- **National Data**: State and county-level data from CDC surveillance reports

## Features

- Interactive maps showing geographic distribution of elevated blood lead levels
- Time series analysis of lead exposure trends
- Comparison between NYC, NY State, and national averages
- Filters for year, geography level, and BLL thresholds

## Important Notes

- The CDC reference value for elevated BLL changed from 5 μg/dL to 3.5 μg/dL in October 2021
- NYC does not publish ZIP-code level data; NTA/Borough is the finest available geography
- NYS Open Data ZIP-level data excludes NYC

## Installation

### Prerequisites

- R (>= 4.0.0)
- RStudio (recommended)

### Setup

1. Clone the repository:
```bash
git clone https://github.com/YOUR_USERNAME/childhood-lead-tracker.git
cd childhood-lead-tracker
```

2. Restore R dependencies:
```r
install.packages("renv")
renv::restore()
```

3. Run the app:
```r
shiny::runApp()
```

## Data Sources

### NYC Level
- [NYC Environment & Health Data Portal](https://a816-dohbesp.nyc.gov/IndicatorPublic/data-explorer/lead/)
- [nychealth/EHDP-data GitHub](https://github.com/nychealth/EHDP-data)

### National Level (CDC)
- [CDC State Surveillance Data](https://www.cdc.gov/lead-prevention/php/data/state-surveillance-data.html)
- [CDC National Surveillance Data](https://www.cdc.gov/lead-prevention/php/data/national-surveillance-data.html)

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
│   └── processed/            # Cleaned data for app
├── www/
│   └── styles.css            # Custom styling
└── tests/
    └── testthat/             # Unit tests
```

## License

MIT License - see LICENSE file for details.

## Contributing

Contributions are welcome! Please open an issue or submit a pull request.
