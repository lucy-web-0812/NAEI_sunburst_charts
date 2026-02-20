# UK Air Pollutants Dashboard

An interactive Shiny web application for exploring UK air pollutant emissions data from the National Atmospheric Emissions Inventory (NAEI). This dashboard provides comprehensive visualization of emission sources and historical trends across different pollutants.

![Dashboard Preview](screenshot.png)

## Features

### 🎯 Interactive Sunburst Chart

-   **Hierarchical Visualisation**: Explore emissions data across three levels (sectors, sub-sectors, and specific sources)
-   **Dynamic Filtering**: Select different pollutants and years
-   **Click-to-Explore**: Click on chart sections to explore detailed trends
-   **Units Display**: Appropriate units shown throughout (kt, kt NO2 equivalent, etc.)

### 📈 Historical Trends Analysis

-   **Time Series Plots**: View emissions from 1990 to projected 2050
-   **Historical vs Projected**: Distinguish between actual and projected data
-   **Source-Specific Trends**: Click sunburst sections to see individual source trends
-   **Contextual Information**: Detailed background commentary for each pollutant

## Installation

### Required R Packages

Install the required packages by running this code in R:

``` r
# Core packages
install.packages(c(
  "shiny",           # Web application framework
  "plotly",          # Interactive plotting
  "dplyr",           # Data manipulation
  "ggplot2",         # Static plotting
  "DT",               # Interactive data tables
  "shinycssloaders", # Loading animations
  "bslib",           # Bootstrap themes
  "thematic"         # Plot theming
))
```

### Data Requirements

The application expects a CSV file named `combined_historic_and_projected.csv` with the following columns:

-   `pollutant`: Pollutant name (e.g., "PM10", "NOx\\n(as NO2)", "SO2")
-   `year`: Year in YYYY-MM-DD format (e.g., "2022-01-01")
-   `source_description`: Detailed source description
-   `NFR_mid`: Mid-level NFR category
-   `NFR_wide.y`: Top-level NFR category\
-   `emission`: Emission value (numeric)
-   `status`: Data status ("actual" or "projected")

## Usage

### Running the Application

1.  **Set Working Directory**: Ensure your working directory contains both `app.R` and the data file:

    ``` r
    setwd("path/to/your/app/directory")
    ```

2.  **Launch the App**:

    ``` r
    # Option 1: If app.R is in current directory
    shiny::runApp()

    # Option 2: Specify the directory
    shiny::runApp("path/to/app/directory")

    # Option 3: Run in browser
    shiny::runApp(launch.browser = TRUE)
    ```

### Using the Dashboard

1.  **Select Parameters**:
    -   Choose a pollutant from the dropdown menu
    -   Select a year to focus on
    -   View total emissions for that combination
2.  **Explore the Sunburst Chart**:
    -   Hover over sections to see detailed information
    -   Click on sections to drill down into specific sources
    -   Use the "Reset Chart View" button to return to overview
3.  **Analyse Trends**:
    -   The trends graph updates based on your sunburst selections
    -   Historical data shows actual emissions 
    -   Future projections are shown in grey

## Data Sources

The application uses data from the **National Atmospheric Emissions Inventory (NAEI)**: - **Website**: [https://naei.energysecurity.gov.uk](https://naei.energysecurity.gov.uk/data/data-selector?view=air-pollutants) - **Coverage**: UK emissions from 1990 with projections to 2050 - **Pollutants**: NOx, PM2.5, PM10, SO2, NH3, NMVOC, and all those on the NAEI website - **Classification**: Uses NFR (Nomenclature For Reporting) categories

## Supported Pollutants

-   **NOx (as NO2)**: Nitrogen oxides - mainly from transport and energy
-   **PM2.5**: Fine particulate matter - from combustion and industrial processes\
-   **PM10**: Coarse particulate matter - from transport, construction, and industry
-   **SO2**: Sulphur dioxide - from energy production and industrial processes
-   **NH3**: Ammonia - primarily from agricultural activities
-   **NMVOC**: Non-methane volatile organic compounds - from transport and solvents

## File Structure

```         
your-app-directory/
├── app.R                              # Main application file
├── combined_historic_and_projected.csv # Data file
├── README.md                          # This file

```

## Troubleshooting

### Common Issues

1.  **"Error sourcing app.R"**

    ``` r
    # Check if file exists and working directory is correct
    getwd()
    file.exists("app.R")
    file.exists("combined_historic_and_projected.csv")
    ```

2.  **"Package not found" errors**

    ``` r
    # Install missing packages
    install.packages(c("shiny", "plotly", "dplyr"))
    ```

3.  **"Object not found" errors**

    ``` r
    # Test data loading separately
    test_data <- read.csv("combined_historic_and_projected.csv")
    head(test_data)
    ```

4.  **Chart not displaying**

    -   Ensure plotly is installed and loaded
    -   Check browser console for JavaScript errors
    -   Try refreshing the browser


## Customisation

### Modifying Colours

Customize the colour palette by updating the `base_colours` vector:

``` r
base_colours <- c("#your_color1", "#your_color2", ...)
```

### Enhancing Commentary

Add pollutant-specific information in the `get_commentary()` function:

``` r
"Your_Pollutant" = "Your custom commentary about this pollutant..."
```


