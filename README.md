# UK Air Pollutants Dashboard

An interactive Shiny web application for exploring UK air pollutant emissions data from the National Atmospheric Emissions Inventory (NAEI). This dashboard provides comprehensive visualization of emission sources and historical trends across different pollutants.

![Dashboard Preview](screenshot.png)

## Features

### 🎯 Interactive Sunburst Chart

-   **Hierarchical Visualization**: Explore emissions data across three levels (sectors, sub-sectors, and specific sources)
-   **Dynamic Filtering**: Select different pollutants and years
-   **Click-to-Drill**: Click on chart sections to explore detailed trends
-   **Units Display**: Proper units shown throughout (kt, kt NO2 equivalent, etc.)

### 📈 Historical Trends Analysis

-   **Time Series Plots**: View emissions from 1990 to projected 2050
-   **Historical vs Projected**: Distinguish between actual and projected data
-   **Source-Specific Trends**: Click sunburst sections to see individual source trends
-   **Contextual Information**: Detailed background commentary for each pollutant

### 📊 Data Table

-   **Detailed View**: Expandable table with complete emissions data
-   **Interactive Features**: Sort, filter, and search functionality
-   **Export Options**: Download data as CSV or Excel
-   **Visual Indicators**: Color-coded emission levels

### 💡 User-Friendly Interface

-   **Responsive Design**: Works on desktop, tablet, and mobile
-   **Clear Navigation**: Intuitive controls and help text
-   **Professional Styling**: Modern Bootstrap-based design
-   **Loading Indicators**: Smooth user experience with progress spinners

## Installation

### Prerequisites

Ensure you have R (version 4.0 or higher) installed on your system.

### Required R Packages

Install the required packages by running this code in R:

``` r
# Core packages
install.packages(c(
  "shiny",           # Web application framework
  "plotly",          # Interactive plotting
  "dplyr",           # Data manipulation
  "ggplot2",         # Static plotting
  "DT"               # Interactive data tables
))

# Optional packages for enhanced features
install.packages(c(
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
    -   Historical data shows actual emissions (green)
    -   Future projections are shown in brown/orange
4.  **View Detailed Data**:
    -   Click "Data Table" to expand the detailed view
    -   Use filters to find specific sources
    -   Export data using the CSV/Excel buttons

## Data Sources

The application uses data from the **National Atmospheric Emissions Inventory (NAEI)**: - **Website**: [https://naei.energysecurity.gov.uk](https://naei.energysecurity.gov.uk/data/data-selector?view=air-pollutants) - **Coverage**: UK emissions from 1990 with projections to 2050 - **Pollutants**: NOx, PM2.5, PM10, SO2, NH3, NMVOC, and others - **Classification**: Uses NFR (Nomenclature For Reporting) categories

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
└── screenshot.png                     # Optional screenshot
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

### Performance Tips

-   **Large Datasets**: The app handles data efficiently, but very large files may slow initial loading
-   **Browser Compatibility**: Works best in modern browsers (Chrome, Firefox, Safari, Edge)
-   **Memory**: Close other applications if experiencing slowdowns with large datasets

## Customisation

### Adding New Pollutants

Update the `pollutant_units` list in the code:

``` r
pollutant_units <- list(
  "Your_Pollutant" = "your_units",
  # ... existing pollutants
)
```

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

## Contributing

To contribute to this project:

1.  Fork the repository
2.  Create a feature branch (`git checkout -b feature/new-feature`)
3.  Commit your changes (`git commit -am 'Add new feature'`)
4.  Push to the branch (`git push origin feature/new-feature`)
5.  Create a Pull Request

## Technical Details

-   **Framework**: Built with R Shiny
-   **Visualization**: Plotly for interactive charts, ggplot2 for static plots
-   **Styling**: Bootstrap 5 with custom CSS
-   **Data Processing**: dplyr for efficient data manipulation
-   **Responsive**: Mobile-friendly design using Bootstrap grid system

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

-   **Data Source**: National Atmospheric Emissions Inventory (NAEI)
-   **Original Developer**: Lucy Webster
-   **R Community**: For excellent packages that made this possible

## Version History

-   **v2.0**: Enhanced UI, added units, improved interactivity
-   **v1.0**: Initial sunburst visualization

## Support

For questions, issues, or suggestions:

1.  Check the troubleshooting section above
2.  Search existing GitHub issues
3.  Create a new issue with:
    -   R version (`R.version.string`)
    -   Package versions (`sessionInfo()`)
    -   Error messages (full text)
    -   Steps to reproduce

------------------------------------------------------------------------

**Made with ❤️ using R Shiny**
