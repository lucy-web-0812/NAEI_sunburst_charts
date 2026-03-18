# UK Air Pollutants Dashboard

An interactive Shiny web application for exploring UK air pollutant emissions data from the National Atmospheric Emissions Inventory (NAEI). This dashboard provides comprehensive visualization of emission sources and historical trends across different pollutants.

[Dashboard Here](https://shiny.york.ac.uk/NAEI_visualiser/)

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


## Customisation

### Modifying Colours

Customise the colour palette by updating the `base_colours` vector:

``` r
base_colours <- c("#your_color1", "#your_color2", ...)
```

### Enhancing Commentary

Add pollutant-specific information in the `get_commentary()` function:

``` r
"Your_Pollutant" = "Your custom commentary about this pollutant..."
```


