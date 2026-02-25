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

raw_data <- read.csv("combined_historic_and_projected.csv") |> 
  mutate(NFR_wide.y = ifelse(NFR_wide.y == "Fuel Combustion Activities", NFR_mid, NFR_wide.y)) |> 
  left_join(pollutants_and_units,join_by(pollutant == Pollutant))



# Make all the colours consistent across the sectors, even when pollutant selected changes 

colour_data <-  raw_data |> 
  select(c(NFR_wide.y, source_description)) |> 
  distinct()




# For the purposes of visualisation, I think we need to break up the Fuel Combustion Activities section... 


num_colours <- length(unique(raw_data$NFR_wide.y))
base_colours <- c("#B39DDB","#1F618D","#E573A0","#006400" , "#F4D03F", "#FF7F50",  "#76D7C4", "#C0392B",  "#5DADE2", "yellow", "green", "blue", "orange") # RColorBrewer::brewer.pal(9, "Set1")


colour_key <- data.frame(grandparent_colour = base_colours[1:num_colours], NFR_wide.y = unique(raw_data$NFR_wide.y))



# Function to generate lighter versions for children
lighten_colour <- function(colour, factor) {
  rgb_col <- col2rgb(colour) / 255
  rgb(rgb_col[1] + (1 - rgb_col[1]) * factor, 
      rgb_col[2] + (1 - rgb_col[2]) * factor, 
      rgb_col[3] + (1 - rgb_col[3]) * factor)
}


# Joining the colour data to the raw data set, adding the parent and child colours 

colour_data <- raw_data |> 
  left_join(colour_key, by = join_by(NFR_wide.y)) |> 
  mutate(
    parent_colour = sapply(grandparent_colour, lighten_colour, factor = 0.1),  
    child_colour = sapply(parent_colour, lighten_colour, factor = 0.2)  
  )


# and mapping colours for the line chart data

line_colour_map <- colour_data |>
  select(source, NFR_wide.y, source_description, child_colour) |>
  distinct() |>
  pivot_longer(cols = c(source, source_description, NFR_wide.y), names_to = "heirachy", values_to = "source") |> 
  select(source, child_colour) |> 
  rename(historic_colour = child_colour)




# Getting a list of each hierachial level and their colours 

grandparent_colours <- colour_data |> 
  select(NFR_wide.y, grandparent_colour) |> 
  distinct() |> 
  rename(label = NFR_wide.y, colour = grandparent_colour) |> 
  mutate(level = "grandparent")

parent_colours <- colour_data |> 
  select(NFR_mid, parent_colour) |> 
  distinct() |> 
  rename(label = NFR_mid, colour = parent_colour) |> 
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


sunburst_dataprocessing <- function(pollutant_species, selected_year){
  
  df <- read.csv("combined_historic_and_projected.csv") |> 
    mutate(NFR_wide.y = ifelse(NFR_wide.y == "Fuel Combustion Activities", NFR_mid, NFR_wide.y)) |> 
    filter(pollutant == pollutant_species) |> 
    filter(year == selected_year) |> 
    select(c(source_description, NFR_mid, NFR_wide.y, emission)) |> 
    rename(NFR_wide = NFR_wide.y) 
  
  
  # Preparing the data for the chart.... 
  
  # firstly get the total... 
  
  
  df_total <- df |> 
    summarise(emission = sum(emission, na.rm = T)) |> 
    mutate(label = "Total", parent = "") |> 
    mutate(colour = "rgba(0,0,0,0)")
  
  # And all the totals for the NFR_wide
  
  
  df_NFR_wide <- df |> 
    group_by(NFR_wide) |> 
    summarise(emission = sum(emission, na.rm = T)) |> 
    mutate(parent = "Total") |> 
    rename(label = NFR_wide) |> 
    left_join(filter(colour_mapping, level == "grandparent"), by = join_by(label))
  
  
  # And all the totals for the NFR_mid
  
  df_NFR_mid <- df |> 
    group_by(NFR_mid) |> 
    summarise(emission = sum(emission, na.rm = T)) |> 
    left_join(df, join_by(NFR_mid == NFR_mid)) |> 
    rename(parent = NFR_wide) |> 
    #mutate(parent = "Total") |> 
    select(NFR_mid, parent, emission.x) |> 
    rename(label = NFR_mid, emission = emission.x) |> 
    filter((label == parent) == F) |> 
    distinct() |> 
    left_join(filter(colour_mapping, level == "parent"), by = join_by(label))
  
  # And all the totals for the NFR_source
  
  df_source <- df |> 
    filter(is.na(emission) == F) |> 
    select(c(source_description, NFR_mid, emission)) |> 
    rename(label = source_description, parent = NFR_mid) |> 
    filter((label == parent) == F) |> 
    distinct() |> 
    left_join(filter(colour_mapping, level == "child"), by = join_by(label))
  
  # bind the rows and make available outside the link
  
  
  hierachial_data <<- bind_rows(
    df_total, 
    df_NFR_wide, 
    df_NFR_mid, 
    df_source
  ) 
  
  
  
}

# ---- Define UI ---- 
ui <- page_fluid(
  
  theme = bs_theme(
    version = 5,
    bg = "white",
    fg = "#2b2b2b",
    primary = "#9A7197",
    base_font = "Consolas"
  ),
  
  tags$style(HTML("
  /* General app font */
  body {
    font-family: Consolas, 'Courier New', monospace !important;
    font-size: 1.1em !important;
  }

  /* Verbatim outputs */
  pre,
  .shiny-text-output,
  .shiny-verbatim-text-output {
    font-family: Consolas, 'Courier New', monospace !important;
     font-size: 1.1em !important;
  }

  /* Plotly text (axes, hover, titles) */
  .js-plotly-plot .plotly text {
    font-family: Consolas, 'Courier New', monospace !important;
    font-size: 1.1em !important;
  }

  /* Plotly hover labels */
  .js-plotly-plot .hovertext {
    font-family: Consolas, 'Courier New', monospace !important;
    font-size: 1.1em !important;
  }

  /* D3 SVG text */
  svg text {
    font-family: Consolas, 'Courier New', monospace !important;
    font-size: 1.1em !important;
  }

  /* D3 HTML text elements */
  .d3-tip,
  .axis text {
    font-family: Consolas, 'Courier New', monospace !important;
    font-size: 1.1em !important;
  }
")),
  
  # Application title
  titlePanel("Sources of air pollutants"),
  
  fluidRow(
    column(12,
           div(class = "section",#"card p-3 mb-4 shadow-sm",  # Bootstrap classes
               style = "height: 20vh;", 
               h5("View emission proportions by pollutant and year:", style = "margin-bottom: 5px; font-weight: bold; font-size: 1.4em"),
               fluidRow(
                 column(4,
                        selectInput("pollutant",
                                   h5( "Pollutant:", style = "margin-bottom: 5px; font-weight: bold; font-size: 1.2em"),
                                    selected = "PM10",
                                    choices = unique(raw_data$pollutant))),
                 column(4,
                        selectInput("year",
                                    h5( "Year:", style = "margin-bottom: 5px; font-weight: bold; font-size: 1.2em"),
                                    selected = "2023",
                                    choices = substr(unique(raw_data$year), 1, 4)))
               )
           )
    )
  ), 
  

  
  # Show a plot of the generated distribution
  
  fluidRow(
    column(7, div(class = "section", #card p-3 mb-4 shadow-sm", 
                  plotlyOutput("sunburstplot", height = "70vh")) |>  withSpinner(color="#0dc5c1", type = 6)), 
    column(5, 
           div(class = "section", #card p-3 mb-4 shadow-lg",
               style = "height: 70vh;", 
           h5("Changes in the sources of air pollutants in the UK:", style = "margin-bottom: 20px; font-weight: bold;"),
           textOutput("commentary"), 
           plotlyOutput("totals_graph", height = "50vh"),
           downloadButton("download", label = "Download Plot Data")|>  
             withSpinner(color="#0dc5c1", type = 6)))
  ),
  
  
  tags$div(
    style = "margin-top: 1px; padding-top: 1px; border-top: 1px solid #ccc; text-align: right; font-size: 16px; color: #555;",
    "Source: ",
    tags$a(href = "https://naei.energysecurity.gov.uk/data/data-selector?view=air-pollutants", 
           "National Atmospheric Emissions Inventory", 
           target = "_blank", 
           style = "color: #555; text-decoration: underline;"),
    " | Made by: Lucy Webster"
  )
  
)


# ----- And the server function ------
server <- function(input, output) {
  thematic::thematic_shiny()
  
  
  # Run the function on the selected data 
  
  hierachial_data <- reactive({
    sunburst_dataprocessing(input$pollutant, paste0(input$year, "-01-01")) |> 
      mutate(
        percentage = ifelse(
          label == "Total",
          100,
          (emission / sum(emission[parent == "Total"], na.rm = TRUE)) * 100
        )
      )
  })
  
  
  # And the plot output 
  
  output$sunburstplot <- renderPlotly({
    
    colour_mapping <- hierachial_data()$colour
    
    
    
    plot_ly(
      labels = hierachial_data()$label, 
      parents = hierachial_data()$parent,
      values = hierachial_data()$emission,
      type = 'sunburst',
      source = "sunburst",
      branchvalues = 'total',
      marker = list(colors =  hierachial_data()$colour,
                    line = list(color = "white", width = 1)),
      insidetextorientation = 'radial', 
      text = paste0(
        hierachial_data()$label, "<br>",
        round(hierachial_data()$emission, 2), " ",
        unique(raw_data$Units[raw_data$pollutant == input$pollutant]), "<br>",
        round(hierachial_data()$percentage, 2), "%" 
      ), 
      hoverinfo = 'text', 
      textinfo = 'text',
    ) |> layout(
      paper_bgcolor = "rgba(0,0,0,0)",  # Fully transparent background
      plot_bgcolor = "rgba(0,0,0,0)"  # Background of the plotting region is transparent too )  
    )
    
    
  })
  
  
  # Also want to have graph and text that pops up depending upon what has been selected...... Idea would be to get like the wheredoesitallgo website....   
  
  
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
        paper_bgcolor = "rgba(0,0,0,0)"   # Transparent outer area
      )
    )
      
    } else if (selected_label == "Total") {  
      
      
      filtered_data <- raw_data |> 
        filter(pollutant == input$pollutant) |> 
        group_by(NFR_wide.y, year, status, Units) |> 
        summarise(emission = sum(emission, na.rm = T)) |> 
        rename(source = NFR_wide.y) |> 
        group_by(year, Units, status) |> 
        summarise(emission = sum(emission, na.rm = T)) |> 
        mutate(source = "Total")
      
    } else if (hierachial_data()$level[point_index] ==  "child") {
      
      # So if it is the lowest level, its fine to keep the source description as the filter criteria as this is what it is based upon
      
      filtered_data <- raw_data |> 
        filter(source_description == selected_label) |> 
        filter(pollutant == input$pollutant) |>  
        left_join(colour_key, by = join_by(NFR_wide.y)) |> 
        mutate(source = source_description)
      
    } else if (hierachial_data()$level[point_index] ==  "parent") {
      
      # However, it needs to be different for the next level up.
      #### AT THE MOMENT we have the wrong order of what is being selected, in that we want to see breakdown befpre clicking. Parent and child should be the same (coode for the chuld atm)
      
      # Instead of plotting only the parent total,
        # get all of its children (source_description)
        filtered_data <- raw_data |>
        filter(pollutant == input$pollutant) |>
        filter(NFR_mid == selected_label) |> 
        group_by(source_description, year, status, Units) |> 
        summarise(emission = sum(emission, na.rm = TRUE), .groups = "drop") |>
        rename(source = source_description)
      
    } else if (hierachial_data()$level[point_index] == "grandparent") {
      
      filtered_data <- raw_data |> 
        filter(pollutant == input$pollutant) |> 
        group_by(NFR_wide.y, year, status, Units) |> 
        summarise(emission = sum(emission, na.rm = T)) |> 
        rename(source = NFR_wide.y) |> 
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
      mutate(plot_colour = ifelse(source == "Total" & status == "Historic", "#394F49", plot_colour))
    
    
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
      )) 
    })
    
  output$download <- downloadHandler(
    filename = paste0(input$pollutant,"_data.csv"),
    content = function(file) {
      readr::write_csv(data_for_export, file)
    }
  )
  
  
  output$commentary <-  renderText({
    
    req(input$pollutant) 
    
    # Main source in 1990
    top_1990 <- raw_data |>
      filter(pollutant == input$pollutant, year == "1990-01-01") |>
      group_by(source_description, Units) |>
      summarise(total_emission = sum(emission, na.rm = TRUE)) |>
      arrange(desc(total_emission)) |> 
      head(n = 1)
    
    # Main source in 2050
    top_2050 <- raw_data |>
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
        top_2050$source_description, " (", round(top_2050$total_emission, 1), " ", top_2050$Units," )."
      )
    } else {
      paste0(
        "For ", input$pollutant, ": in 1990 the largest source was ",
        top_1990$source_description, " (", round(top_1990$total_emission, 1), " ", top_1990$Units," ). ",
        "The projections to 2050 are not available for this pollutant."
        
      )
    }
    
    
  })
  
  
  
}
  
  

# Run the application 
shinyApp(ui = ui, server = server)