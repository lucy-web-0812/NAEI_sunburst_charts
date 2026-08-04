library(shinydashboard)
library(shiny)
library(plotly)
library(tidyverse)
library(shinycssloaders)
library(bslib)

# Doing some changes that may appear in the git 



pollutants_and_units <- read_csv("list_of_pollutants.csv") |> 
  mutate(Pollutant = gsub("[\r]", "", Pollutant), 
         Pollutant = ifelse(Pollutant == "Total 1-4", "Total PAHs", Pollutant)) 


# Pre-processing 

air_pollutant_data <- read.csv("combined_historic_and_projected.csv") |>  # Note that need read.csv to get the right pollutant format for the join! Don't know why! 
  mutate(NFR_wide.y = case_when(
    Code_3 == "1A1" ~ "Energy Supply",
    Code_3 == "1A2" ~ "Industrial Combustion",
    Code_3 == "1A3" ~ "Transport",
    Code_3 == "1A4" ~ "Residential & Commercial Combustion",
    Code_3 == "1A5" ~ "Other Combustion",
    Code_2 == "1A"  ~ "Other Combustion",   # catches NA Code_3 under 1A
    Code_2 == "1B"  ~ "Fugitive Emissions",
    Code_2 %in% c("2A","2B","2C","2D","2G","2H","2I","2J","2K","2L") ~ "Industrial Processes",
    Code_2 %in% c("3B","3D","3F","3I") ~ "Agriculture",
    Code_2 %in% c("5A","5B","5C","5D","5E") ~ "Waste",
    TRUE ~ "Other"  # z_, 6A, 6B, 11A, memo/natural sources
  ))  |> 
  mutate(NFR_mid = ifelse(Code_2 %in% c("1A", "1B"), source_description, NFR_mid)) |>
  mutate(NFR_wide.y = ifelse(
    NFR_wide.y == "Other" & NFR_mid == "Transport", 
    "Transport", 
    NFR_wide.y
  ), 
    NFR_mid = ifelse(
      NFR_wide.y == "Other" & NFR_mid == "Transport", 
    "International Transport", 
    NFR_mid
  ) )|> 
  left_join(pollutants_and_units, join_by(pollutant == Pollutant)) |> 
  select(-c(`X`, `...1`)) |> 
  rename(wide_col = NFR_wide.y, mid_col = NFR_mid)



ghg_data <- read_csv("ghg_data.csv") |> 
  select(-`...1`) |> 
  filter(CRT_Code != "non-IPCC") |> 
  filter(is.na(Code_1) == F ) |> 
  filter(!is.na(emission)) |> 
  mutate(CRT_wide = case_when(
    # Split fuel combustion (1A) into meaningful sectors by sub-code
    substr(CRT_Code, 1, 3) == "1A1" ~ "Energy Supply",
    substr(CRT_Code, 1, 3) == "1A2" ~ "Industrial Combustion",
    substr(CRT_Code, 1, 3) == "1A3" ~ "Transport",
    substr(CRT_Code, 1, 3) == "1A4" ~ "Residential & Commercial Combustion",
    substr(CRT_Code, 1, 2) == "1A"  ~ "Other Combustion",
    substr(CRT_Code, 1, 2) == "1B"  ~ "Fugitive Emissions",
    substr(CRT_Code, 1, 1) == "2"   ~ "Industrial Processes",
    substr(CRT_Code, 1, 1) == "3"   ~ "Agriculture",
    substr(CRT_Code, 1, 1) == "4"   ~ "Land Use (LULUCF)",
    substr(CRT_Code, 1, 1) == "5"   ~ "Waste",
    TRUE ~ "Other"  # Keep all other wide categories as-is 
  )) |>
  mutate(CRT_mid = case_when(
    # For combustion sectors, use source_description as the mid level
    substr(CRT_Code, 1, 2) == "1A" ~ source_description,
    substr(CRT_Code, 1, 2) == "1B" ~ source_description,
    TRUE ~ CRT_mid
  )) |>
  mutate(CRT_wide = ifelse(
    CRT_wide == "Other" & CRT_mid == "International Transport", 
    "Transport", 
    CRT_wide
  ), 
  NFR_mid = ifelse(
    CRT_wide == "Other" & CRT_mid == "International Transport", 
    "International Transport", 
    CRT_mid
  ) ) |> 
  rename(wide_col = CRT_wide, mid_col = CRT_mid, pollutant = greenhouse_gas) |> 
  mutate(year = as.character(paste0(year, "-01-01")))



# Make all the colours consistent across the sectors, even when pollutant selected changes 

# For the purposes of visualisation, I think we need to break up the Fuel Combustion Activities section... 


num_colours <- length(unique(c(air_pollutant_data$wide_col, ghg_data$wide_col)))
base_colours <- c("#B39DDB","#1F618D","#E573A0","#006400" , "#F4D03F", "#FF7F50", "#C0392B", "#76D7C4",  "#5DADE2", "yellow", "green", "blue", "orange") # RColorBrewer::brewer.pal(9, "Set1")


colour_key <- data.frame(grandparent_colour = base_colours[1:num_colours], wide_col = unique(c(air_pollutant_data$wide_col, ghg_data$wide_col)))



# Function to generate lighter versions for children
lighten_colour <- function(colour, factor) {
  rgb_col <- col2rgb(colour) / 255
  rgb(rgb_col[1] + (1 - rgb_col[1]) * factor, 
      rgb_col[2] + (1 - rgb_col[2]) * factor, 
      rgb_col[3] + (1 - rgb_col[3]) * factor)
}


ghg_for_colours <- ghg_data |> 
  select(Source, wide_col, mid_col, source_description) |> 
  rename(source = Source)


# Joining the colour data to the raw data set, adding the parent and child colours 

colour_data <- air_pollutant_data |> 
  select(source, wide_col, mid_col, source_description) |> 
  rbind(ghg_for_colours) |> 
  distinct() |> 
  left_join(colour_key, by = join_by(wide_col)) |> 
  mutate(
    parent_colour = sapply(grandparent_colour, lighten_colour, factor = 0.1),  
    child_colour = sapply(parent_colour, lighten_colour, factor = 0.2)  
  )



# and mapping colours for the line chart data

line_colour_map <- colour_data |>
  select(source, wide_col, mid_col, source_description, child_colour) |>
  distinct() |>
  pivot_longer(cols = c(source, source_description,mid_col, wide_col), names_to = "heirachy", values_to = "source") |> 
  select(source, child_colour) |> 
  rename(historic_colour = child_colour) |> 
  distinct()  |> 
  add_row(source = "Total",  historic_colour = "white") # And the totals colours....





# Getting a list of each hierachial level and their colours 

grandparent_colours <- colour_data |> 
  select(wide_col, grandparent_colour) |> 
  distinct() |> 
  rename(label = wide_col, colour = grandparent_colour) |> 
  mutate(level = "grandparent")

parent_colours <- colour_data |> 
  select(mid_col, parent_colour) |> 
  distinct() |> 
  rename(label = mid_col, colour = parent_colour) |> 
  mutate(level = "parent")

child_colours <- colour_data |> 
  select(source_description, child_colour) |> 
  distinct() |> 
  rename(label = source_description, colour = child_colour) |> 
  mutate(level = "child")

# Binding these together to get the colour in one df


colour_mapping <- rbind(grandparent_colours, 
                        parent_colours, 
                        child_colours)


# Function needed to get the data in a format to use the sunburst chart


sunburst_dataprocessing <- function(raw_data_to_process, pollutant_species, selected_year){
  
  df <- raw_data_to_process |> 
    filter(pollutant == pollutant_species) |> 
    filter(year == selected_year) |> 
    select(c(source_description, mid_col, wide_col, emission)) |> 
    filter(emission > 0)
  
  
  # Preparing the data for the chart.... 
  
  # firstly get the total... 
  
  
  df_total <- df |> 
    summarise(emission = sum(emission, na.rm = T)) |> 
    mutate(label = "Total", parent = "") |> 
    mutate(colour = "rgba(0,0,0,0)")
  
  # And all the totals for the NFR_wide
  
  
  df_wide <- df |> 
    group_by(wide_col) |> 
    summarise(emission = sum(emission, na.rm = T)) |> 
    mutate(parent = "Total") |> 
    rename(label = wide_col) |> 
    left_join(filter(colour_mapping, level == "grandparent"), by = join_by(label))
  
  
  # And all the totals for the NFR_mid
  
  df_mid <- df |> 
    group_by(mid_col) |> 
    summarise(emission = sum(emission, na.rm = T)) |> 
    left_join(df, join_by(mid_col == mid_col)) |> 
    rename(parent = wide_col) |> 
    #mutate(parent = "Total") |> 
    select(mid_col, parent, emission.x) |> 
    rename(label = mid_col, emission = emission.x) |> 
    filter((label == parent) == F) |> 
    distinct() |> 
    left_join(filter(colour_mapping, level == "parent"), by = join_by(label))
  
  # And all the totals for the NFR_source
  
  df_source <- df |> 
    filter(is.na(emission) == F) |> 
    select(c(source_description, mid_col, emission)) |> 
    rename(label = source_description, parent = mid_col) |> 
    filter((label == parent) == F) |> 
    group_by(label, parent) |> 
    summarise(emission = sum(emission)) |> 
    distinct() |> 
    left_join(filter(colour_mapping, level == "child"), by = join_by(label))
  
  # bind the rows and make available outside the link
  
  
  hierachial_data <- bind_rows(
    df_total, 
    df_wide, 
    df_mid, 
    df_source
  ) |>
    distinct(label, parent, .keep_all = TRUE)
  
  return(hierachial_data)
  
}



# Prepare the years... 

years <- sort(unique(as.numeric(substr(air_pollutant_data$year, 1, 4))))

historic_years  <- years[years <= 2024]
projected_years <- years[years > 2024]


# ---- Define UI ---- 
ui <- tagList(
  
  # Meta tags for if sharing link
  tags$head(
    tags$meta(property = "og:title", content = "Sources of Air Pollutants in the UK"),
    tags$meta(property = "og:description", content = "Interactive visualisation of historical and projected UK air pollutant emissions by source."),
    tags$meta(property = "og:image", content = "https://github.com/lucy-web-0812/NAEI_sunburst_charts/blob/main/shinyappimage.png"),
    tags$meta(property = "og:type", content = "website"),
    
    # Import Google Font
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Figtree:wght@300;400;500;600;700&display=swap"
    ),
    
    
    # CSS for fonts and responsive tweaks
    tags$style(HTML("
      body, pre, .shiny-text-output, .shiny-verbatim-text-output,
      .js-plotly-plot .plotly text, .js-plotly-plot .hovertext,
      svg text, .d3-tip, .axis text {
         font-family: 'Figtree', sans-serif !important;
        font-size: 1.1em !important;
      }
      @media (max-width: 768px) {
        body { font-size: 1em !important; }
        h5 { font-size: 1.2em !important; }
        
      }
    "))
  ),
  
  # Fluid page with bslib theme
  page_fluid(
    theme = bs_theme(
      version = 5,
      bg = "white",
      fg = "#2b2b2b",
      primary = "#9A7197",
      base_font = "Consolas"
    ),
    
    # Dark mode toggle at top-right
    div(class = "d-flex justify-content-end mb-2", 
        style = "padding:10px;", 
        bslib::input_dark_mode(id = "mode")
    ),
    
    # App title
    titlePanel("Sources of air pollutants"),
    
    navset_tab(
      
      
      nav_panel("Air Pollutants", 
                # Inputs section
                fluidRow(
                  div(class = "col-12",
                      div(class = "section",
                          style = "padding:10px; margin-bottom:10px;",
                          h5("View emission proportions by pollutant and year:", style = "margin-bottom: 10px; font-weight: bold; font-size: 1.4em"),
                          
                          fluidRow(
                            div(class = "col-12 col-md-4",
                                selectInput("pollutant",
                                            h5("Pollutant:", style = "margin-bottom: 5px; font-weight: bold; font-size: 1.2em"),
                                            selected = "PM10",
                                            choices = unique(air_pollutant_data$pollutant),
                                            width = "100%")
                            ),
                            div(class = "col-12 col-md-4",
                                selectInput("year",
                                            h5("Year:", style = "margin-bottom: 5px; font-weight: bold; font-size: 1.2em"),
                                            selected = "2024",
                                            choices = list(
                                              "Historic Data:" = historic_years,
                                              "Projected Data:" = projected_years
                                            ),
                                            width = "100%")
                            )
                          )
                      )
                  )
                ),
                
                # Plots section
                fluidRow(
                  div(class = "col-12 col-lg-7",
                      div(class = "section",
                          withSpinner(plotlyOutput("sunburstplot", width = "100%", height = "60vh"), color="#0dc5c1", type = 6)
                      )
                  ),
                  div(class = "col-12 col-lg-5",
                      div(class = "section",
                          style = "padding:10px;",
                          h5("Changes in the sources of air pollutants in the UK:", style = "margin-bottom: 20px; font-weight: bold;"),
                          uiOutput("commentary"),
                          withSpinner(plotlyOutput("totals_graph", width = "100%", height = "50vh"), color = "#0dc5c1", type = 6),
                          div(style = "text-align: right; margin-top: 10px;",
                              downloadButton("download", label = "Download Plot Data")
                          )
                      )
                  )
                )
                
               
        
      ), 
      
      
      nav_panel(
        "Greenhouse Gases - In Development",
        
        fluidRow(
          
          div(
            class = "col-12 col-md-4",
            selectInput(
              "ghg",
              "Greenhouse Gas:",
              choices = unique(ghg_data$pollutant),
              selected = "Carbon Dioxide as Carbon"
            )
          ),
          
          div(
            class = "col-12 col-md-4",
            selectInput(
              "ghg_year",
              "Year:",
              selected = "2023",
              choices = list(
                "Historic Data:" = historic_years,
                "Projected Data:" = projected_years
              )
            )
          )
        ),
        
        fluidRow(
          
          div(
            class = "col-12 col-lg-7",
            div(
              class = "section",
              withSpinner(
                plotlyOutput("ghg_sunburst", height = "60vh"),
                color = "#0dc5c1",
                type = 6
              )
            )
          ),
          
          div(
            class = "col-12 col-lg-5",
            div(
              class = "section",
              style = "padding:10px;",
              
              h5(
                "Changes in greenhouse gas emissions:",
                style = "margin-bottom: 20px; font-weight: bold;"
              ),
              
              withSpinner(
                plotlyOutput("totals_graph_ghg", height = "50vh"),
                color = "#0dc5c1",
                type = 6
              )
            )
          )
        )
      )
    ), 
    
    
    
    # Footer
    tags$div(
      style = "margin-top: 1px; padding-top: 1px; border-top: 1px solid #ccc; text-align: right; font-size: 16px;",
      "All data from: ",
      tags$a(href = "https://naei.energysecurity.gov.uk/data/data-selector?view=air-pollutants", 
             "National Atmospheric Emissions Inventory", 
             target = "_blank", 
             style = "text-decoration: underline;"),
      " | Made by: Lucy Webster | ", 
      tags$a(
        href = "https://github.com/lucy-web-0812/NAEI_sunburst_charts",
        target = "_blank",
        "View code on GitHub"
      )
    )
      
      
    )
    

)


# ----- And the server function ------
server <- function(input, output) {
  thematic::thematic_shiny()
  
  
  # Run the function on the selected data 
  
  hierachial_data <- reactive({
    sunburst_dataprocessing(air_pollutant_data, input$pollutant, paste0(input$year, "-01-01")) |> 
      mutate(
        percentage = ifelse(
          label == "Total",
          100,
          (emission / sum(emission[parent == "Total"], na.rm = TRUE)) * 100
        )
      )
  })
  
  
  hierachial_data_ghg <- reactive({
    sunburst_dataprocessing(ghg_data, input$ghg, paste0(input$ghg_year, "-01-01")) |> 
      mutate(
        percentage = ifelse(
          label == "Total",
          100,
          (emission / sum(emission[parent == "Total"], na.rm = TRUE)) * 100
        )
      )
  })
  
  
  
  

# AIR POLLUTANTS ----------------------------------------------------------

  
  
  
  # And the plot output 
  
  output$sunburstplot <- renderPlotly({
    
    colours <- hierachial_data()$colour
    
    
    validate(
      need(sum(hierachial_data()$emission) != 0, paste0("Sorry, there are no projections available for ", input$pollutant))
    )

    
    plot_ly(
      labels = hierachial_data()$label, 
      parents = hierachial_data()$parent,
      values = hierachial_data()$emission,
      type = 'sunburst',
      source = "sunburst",
      branchvalues = 'total',
      marker = list(colors =  colours,
                    line = list(color = "white", width = 1)),
      insidetextorientation = 'radial', 
      text = paste0(
        hierachial_data()$label, "<br>",
        round(hierachial_data()$emission, 2), " ",
        unique(air_pollutant_data$Units[air_pollutant_data$pollutant == input$pollutant]), "<br>",
        round(hierachial_data()$percentage, 2), "%" 
      ), 
      hoverinfo = 'text', 
      textinfo = 'text',
    ) |> layout(
      font = list(color = if (input$mode == "dark") "white" else "black"),
      paper_bgcolor = "rgba(0,0,0,0)",  # Fully transparent background
      plot_bgcolor = "rgba(0,0,0,0)"  # Background of the plotting region is transparent too )  
    )
    
    
  })
  
  
  # Also want to have graph and text that pops up depending upon what has been selected...... 
  
  
  output$totals_graph <- renderPlotly({
    
    # Need a pollutant to be selected to run the code 
    
    req(input$pollutant) 
    
    # If the user clicks on the plot, records the info
    
    click_event <- event_data("plotly_click", source = "sunburst")  # Capture click event
    
    point_index <- click_event$pointNumber + 1
    
    selected_label <- hierachial_data()$label[point_index]  # Extract the correct label
    
    parent_of_selection <- hierachial_data()$parent[point_index] # And the parent
    
    
    if (is.null(click_event)) {return(
      plot_ly(
        type = 'scatter',
        mode = 'text',
        text = "Please click on a section of the chart.",
        x = c(0.5),
        y = c(0.5),
        textposition = "middle center"
      ) |> layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
        plot_bgcolor = "rgba(0,0,0,0)",   # Transparent plot area
        paper_bgcolor = "rgba(0,0,0,0)",   # Transparent outer area
        font = list(color = if (input$mode == "dark") "white" else "black")
      )
    )
      
    } else if (selected_label == "Total") {  
      
      
      filtered_data <- air_pollutant_data |> 
        filter(pollutant == input$pollutant) |> 
        group_by(wide_col, year, status, Units) |> 
        summarise(emission = sum(emission, na.rm = T)) |> 
        rename(source = wide_col) |> 
        group_by(year, Units, status) |> 
        summarise(emission = sum(emission, na.rm = T)) |> 
        mutate(source = "Total")
      
    } else if (hierachial_data()$level[point_index] ==  "child") {
      
      # So if it is the lowest level, its fine to keep the source description as the filter criteria as this is what it is based upon
      
      filtered_data <- air_pollutant_data |> 
        filter(source_description == selected_label) |> 
        filter(pollutant == input$pollutant) |>  
        left_join(colour_key, by = join_by(wide_col)) |> 
        mutate(source = source_description)
      
    } else if (hierachial_data()$level[point_index] ==  "parent") {
      
      # However, it needs to be different for the next level up.
      #### AT THE MOMENT we have the wrong order of what is being selected, in that we want to see breakdown befpre clicking. Parent and child should be the same (coode for the chuld atm)
      
      # Instead of plotting only the parent total,
        # get all of its children (source_description)
        filtered_data <- air_pollutant_data |>
        filter(pollutant == input$pollutant) |>
        filter(mid_col == selected_label) |> 
        group_by(source_description, year, status, Units) |> 
        summarise(emission = sum(emission, na.rm = TRUE), .groups = "drop") |>
        rename(source = source_description)
      
    } else if (hierachial_data()$level[point_index] == "grandparent") {
      
      filtered_data <- air_pollutant_data |> 
        filter(pollutant == input$pollutant) |> 
        group_by(wide_col, year, status, Units) |> 
        summarise(emission = sum(emission, na.rm = T)) |> 
        rename(source = wide_col) |> 
        filter(source == selected_label)
    } 
      
    
    
    
    data_for_export  <<- filtered_data |> 
      mutate(pollutant = input$pollutant) |> 
      rename(units = Units) |> 
      select(pollutant, source, year, emission, units, status)
    
    
    
    
    filtered_data_with_colours <- filtered_data |>  
      left_join(line_colour_map, by = "source") |>
      mutate(
        plot_colour = ifelse(status == "Historic", historic_colour, "darkgrey")
      ) |> 
      mutate(plot_colour = ifelse(source == "Total" & status == "Historic" & input$mode == "light", "#394F49", plot_colour))  |> 
      mutate(plot_colour = ifelse(source == "Total" & status == "Historic" & input$mode == "dark", "#FFFFFF", plot_colour))
    
    
    ggplotly(
      ggplot(filtered_data_with_colours) +
        geom_point(
          aes(
            x = as.Date(year),
            y = emission,
            colour = plot_colour, 
            group = interaction(source, status),   
            shape = status,
            linetype = status,
            text = paste0(
              "Year: ", substr(as.character(year),1,4),
              "<br>Emission: ", round(emission, 2), " ", Units,
              "<br>Status: ", status,
              "<br>Source: ", source
            )
          )
        ) +
        geom_line(
          aes(
            x = as.Date(year),
            y = emission,
            colour = plot_colour,
            group = interaction(source, status),
            linetype = status,
            text = paste0(
              "Year: ", substr(as.character(year),1,4),
              "<br>Emission: ", round(emission, 2), " ", Units,
              "<br>Status: ", status,
              "<br>Source: ", source
            )
          )
        ) +
        scale_colour_identity() +
        scale_x_date(name = "Year", limits = c(as.Date("1990-01-01"), as.Date("2050-12-31"))) +
        scale_y_continuous(name = paste0("Emissions (", unique(filtered_data$Units), ")"), limits = c(0,NA)) +
        ggtitle(ggtitle(paste(unique(filtered_data$source), collapse = ", "))) +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(colour = "lightgrey"),
              plot.title = element_text(face = "bold"), 
              legend.position = "none", 
              panel.background = element_rect("white"), 
              plot.background = element_rect("white")), 
      tooltip = "text"
    ) |>
      layout(legend = list(
        x = 0.75,
        y = 0.85
      ), 
      paper_bgcolor = "rgba(0,0,0,0)",  # Fully transparent background
      plot_bgcolor = "rgba(0,0,0,0)" , # Background of the plotting region is transparent too ) ) 
      margin = list(t = 80) )
    })
    
  output$download <- downloadHandler(
    filename = paste0(input$pollutant,"_data.csv"),
    content = function(file) {
      readr::write_csv(data_for_export, file)
    }
  )
  
  
  output$commentary <-  renderUI({
    
    req(input$pollutant) 
    
    # Main source in 1990
    top_1990 <- air_pollutant_data |>
      filter(pollutant == input$pollutant, year == "1990-01-01") |>
      group_by(source_description, Units) |>
      summarise(total_emission = sum(emission, na.rm = TRUE)) |>
      arrange(desc(total_emission)) |> 
      head(n = 1)
    
    # Main source in 2050
    top_2050 <- air_pollutant_data |>
      filter(pollutant == input$pollutant, year == "2050-01-01") |>
      group_by(source_description, Units) |>
      summarise(total_emission = sum(emission, na.rm = TRUE)) |>
      arrange(desc(total_emission)) |> 
      head(n = 1)
    
    
    if (top_2050$total_emission[1] != 0) {
      paste0(
        "For ", input$pollutant, ": in 1990 the largest source was ",
        top_1990$source_description, " (", round(top_1990$total_emission, 1), " ", top_1990$Units, "). ",
        "By 2050 the main source is projected to be ",
        top_2050$source_description, " (", round(top_2050$total_emission, 1), " ", top_2050$Units,")."
      )
    } else {
      HTML(paste0(
        "For ", input$pollutant, ": in 1990 the largest source was ",
        top_1990$source_description, " (", round(top_1990$total_emission, 1), " ", top_1990$Units,"). ",
       "<b>The projections to 2050 are not available for this pollutant.</b>"
        
      ))
    }
    
    
  })
  

# GREENHOUSE GASES --------------------------------------------------------

  output$ghg_sunburst <- renderPlotly({
    
    colours <- hierachial_data_ghg()$colour
    
    
    validate(
      need(sum(hierachial_data_ghg()$emission) != 0, paste0("Sorry, there are no projections available for ", input$ghg))
    )
    
    
    plot_ly(
      labels = hierachial_data_ghg()$label, 
      parents = hierachial_data_ghg()$parent,
      values = hierachial_data_ghg()$emission,
      type = 'sunburst',
      source = "ghg_sunburst",
      branchvalues = 'total',
      marker = list(colors =  colours,
                    line = list(color = "white", width = 1)),
      insidetextorientation = 'radial', 
      text = paste0(
        hierachial_data_ghg()$label, "<br>",
        round(hierachial_data_ghg()$emission, 2), " ",
        unique(ghg_data$Units[ghg_data$pollutant == input$ghg]), "<br>",
        round(hierachial_data_ghg()$percentage, 2), "%" 
      ), 
      hoverinfo = 'text', 
      textinfo = 'text',
    ) |> layout(
      font = list(color = if (input$mode == "dark") "white" else "black"),
      paper_bgcolor = "rgba(0,0,0,0)",  # Fully transparent background
      plot_bgcolor = "rgba(0,0,0,0)"  # Background of the plotting region is transparent too )  
    )
    
    
  })
  
  
  
  
  
  output$totals_graph_ghg <- renderPlotly({
    
    # Need a pollutant to be selected to run the code 
    
    req(input$ghg) 
    
    # If the user clicks on the plot, records the info
    
    click_event <- event_data("plotly_click", source = "ghg_sunburst")  # Capture click event
    
    point_index <- click_event$pointNumber + 1
    
    selected_label <- hierachial_data_ghg()$label[point_index]  # Extract the correct label
    
    parent_of_selection <- hierachial_data_ghg()$parent[point_index] # And the parent
    
    
    if (is.null(click_event)) {return(
      plot_ly(
        type = 'scatter',
        mode = 'text',
        text = "Please click on a section of the chart.",
        x = c(0.5),
        y = c(0.5),
        textposition = "middle center"
      ) |> layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
        plot_bgcolor = "rgba(0,0,0,0)",   # Transparent plot area
        paper_bgcolor = "rgba(0,0,0,0)",   # Transparent outer area
        font = list(color = if (input$mode == "dark") "white" else "black")
      )
    )
      
    } else if (selected_label == "Total") {  
      
      
      filtered_data <- ghg_data |> 
        filter(pollutant == input$ghg) |> 
        group_by(wide_col, year, Units) |> 
        summarise(emission = sum(emission, na.rm = T)) |> 
        rename(source = wide_col) |> 
        group_by(year, Units) |> 
        summarise(emission = sum(emission, na.rm = T)) |> 
        mutate(source = "Total")
      
    } else if (hierachial_data_ghg()$level[point_index] ==  "child") {
      
      # So if it is the lowest level, its fine to keep the source description as the filter criteria as this is what it is based upon
      
      filtered_data <- ghg_data |> 
        filter(source_description == selected_label) |> 
        filter(pollutant == input$ghg) |>  
        left_join(colour_key, by = join_by(wide_col)) |> 
        mutate(source = paste(source_description, Activity, Source))
      
    } else if (hierachial_data_ghg()$level[point_index] ==  "parent") {
      
      # However, it needs to be different for the next level up.
      #### AT THE MOMENT we have the wrong order of what is being selected, in that we want to see breakdown befpre clicking. Parent and child should be the same (coode for the chuld atm)
      
      # Instead of plotting only the parent total,
      # get all of its children (source_description)
      filtered_data <- ghg_data |>
        filter(pollutant == input$ghg) |>
        filter(mid_col == selected_label) |> 
        group_by(source_description, year,  Units) |> 
        summarise(emission = sum(emission, na.rm = TRUE), .groups = "drop") |>
        rename(source = source_description)
      
    } else if (hierachial_data_ghg()$level[point_index] == "grandparent") {
      
      filtered_data <- ghg_data |> 
        filter(pollutant == input$ghg) |> 
        group_by(wide_col, year, Units) |> 
        summarise(emission = sum(emission, na.rm = T)) |> 
        rename(source = wide_col) |> 
        filter(source == selected_label)
    } 
    
    
    
    
    data_for_export  <<- filtered_data |> 
      mutate(pollutant = input$ghg) |> 
      rename(units = Units) |> 
      select(pollutant, source, year, emission, units)
    
    
    
    
    filtered_data_with_colours <- filtered_data |>  
      left_join(line_colour_map, by = "source") |>
      mutate(historic_colour = ifelse(is.na(historic_colour), grandparent_colour, historic_colour)) |> 
      # mutate(
      #   plot_colour = ifelse(status == "Historic", historic_colour, "darkgrey")
      # ) |> 
      mutate(plot_colour = ifelse(source == "Total"  & input$mode == "light", "#394F49", historic_colour))  |> 
      group_by(source)
    
    
    ggplotly(
      ggplot(filtered_data_with_colours) +
        geom_point(
          aes(
            x = as.Date(year),
            y = emission,
            colour = plot_colour, 
            group = source,   
            #shape = status,
            #linetype = status,
            text = paste0(
              "Year: ", substr(as.character(year),1,4),
              "<br>Emission: ", round(emission, 2), " ", Units,
             # "<br>Status: ", status,
              "<br>Source: ", source
            )
          )
        ) +
        geom_line(
          aes(
            x = as.Date(year),
            y = emission,
            colour = plot_colour,
            group = source,
            #linetype = status,
            text = paste0(
              "Year: ", substr(as.character(year),1,4),
              "<br>Emission: ", round(emission, 2), " ", Units,
             # "<br>Status: ", status,
              "<br>Source: ", source
            )
          )
        ) +
        scale_colour_identity() +
        scale_x_date(name = "Year", limits = c(as.Date("1990-01-01"), as.Date("2050-12-31"))) +
        scale_y_continuous(name = paste0("Emissions (", unique(filtered_data$Units), ")"), limits = c(0,NA)) +
        ggtitle(ggtitle(paste(unique(filtered_data$source), collapse = ", "))) +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(colour = "lightgrey"),
              plot.title = element_text(face = "bold"), 
              legend.position = "none", 
              panel.background = element_rect("white"), 
              plot.background = element_rect("white")), 
      tooltip = "text"
    ) |>
      layout(legend = list(
        x = 0.75,
        y = 0.85
      ), 
      paper_bgcolor = "rgba(0,0,0,0)",  # Fully transparent background
      plot_bgcolor = "rgba(0,0,0,0)" , # Background of the plotting region is transparent too ) ) 
      margin = list(t = 80) )
  })
  
  output$download_ghg <- downloadHandler(
    filename = paste0(input$ghg,"_data.csv"),
    content = function(file) {
      readr::write_csv(data_for_export, file)
    }
  )
  
  
  
  
  
  
  
  
  
  
}
  
  

# Run the application 
shinyApp(ui = ui, server = server)
