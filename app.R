library(shinydashboard)
library(shiny)
library(plotly)
library(tidyverse)

# Doing some changes that may appear in the git 

# Pre-processing 

raw_data <- read.csv("combined_historic_and_projected.csv") |> 
  mutate(NFR_wide.y = ifelse(NFR_wide.y == "Fuel Combustion Activities", NFR_mid, NFR_wide.y)) 

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
ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "morph"),
  
  # Application title
  titlePanel("Sources of air pollutants"),
  
  fluidRow(
    column(12,
           div(class = "card p-3 mb-4 shadow-sm",  # Bootstrap classes
               h4(em("View emission proportions by pollutant and year:"), style = "margin-bottom: 20px; font-weight: bold;"),
               fluidRow(
                 column(4,
                        selectInput("pollutant",
                                    "Pollutant:",
                                    choices = unique(raw_data$pollutant))),
                 column(4,
                        selectInput("year",
                                    "Year:",
                                    choices = substr(unique(raw_data$year), 1, 4)))
               )
           )
    )
  ), 
  
  # Show a plot of the generated distribution
  
  fluidRow(
    column(7, plotlyOutput("sunburstplot", height = "900px")), 
    column(5, 
           div(class = "card p-3 mb-4 shadow-lg",
           h4(em("Changes in the sources of air pollutants in the UK:"), style = "margin-bottom: 20px; font-weight: bold;"),
           plotlyOutput("totals_graph", height = "500px")))
  ),
)


# ----- And the server function ------
server <- function(input, output) {
  thematic::thematic_shiny()
  
  # Run the function on the selected data 
  
  hierachial_data <- reactive({
    sunburst_dataprocessing(input$pollutant, paste0(input$year, "-01-01"))
  })
  
  
  # And the plot output 
  
  output$sunburstplot <- renderPlotly({
    
    colour_mapping <- hierachial_data()$colour
    
    
    plot_ly(
      labels = hierachial_data()$label, 
      parents = hierachial_data()$parent,
      values = hierachial_data()$emission,
      type = 'sunburst',
      branchvalues = 'total',
      marker = list(colors =  hierachial_data()$colour,
                    line = list(color = "white", width = 1)),
      insidetextorientation = 'radial', 
      textinfo = 'label+percent parent', 
      hoverinfo = 'label+percent parent+value'
    ) |> layout(
      paper_bgcolor = "rgba(0,0,0,0)",  # Fully transparent background
      plot_bgcolor = "rgba(0,0,0,0)",  # Background of the plotting region is transparent too 
      font = list(color = "black", size = 18, family = "Arial")  
    )
    
    
  })
  
  
  # Also want to have graph and text that pops up depending upon what has been selected...... Idea would be to get like the wheredoesitallgo website....   
  
  
  output$totals_graph <- renderPlotly({
    
    # Need a pollutant to be selected to run the code 
    
    req(input$pollutant) 
    
    # If the user clicks on the plot, records the info
    
    click_event <- event_data("plotly_click")  # Capture click event
    req(click_event)
    
    point_index <- click_event$pointNumber + 1
    
    selected_label <- hierachial_data()$label[point_index]  # Extract the correct label
    
    parent_of_selection <- hierachial_data()$parent[point_index] # And the parent
    
    if (hierachial_data()$level[point_index] ==  "child") {
      
      # So if it is the lowest level, its fine to keep the source description as the filter criteria as this is what it is based upon
      
      filtered_data <- raw_data |> 
        filter(source_description == selected_label) |> 
        filter(pollutant == input$pollutant) |>  
        left_join(colour_key, by = join_by(NFR_wide.y)) |> 
        mutate(source = source_description)
      
    } else if (hierachial_data()$level[point_index] ==  "parent") {
      
      # However, it needs to be different for the next level up.
      #### AT THE MOMENT we have the wrong order of what is being selected, in that we want to see breakdown befpre clicking. Parent and child should be the same (coode for the chuld atm)
      
      filtered_data <- raw_data |>
        filter(pollutant == input$pollutant) |>
        group_by(NFR_mid, year, status) |>
        summarise(emission = sum(emission, na.rm = T)) |>
        rename(source = NFR_mid) |>
        filter(source == selected_label)
      
    } else if (hierachial_data()$level[point_index] == "grandparent") {
      
      filtered_data <- raw_data |> 
        filter(pollutant == input$pollutant) |> 
        group_by(NFR_wide.y, year, status) |> 
        summarise(emission = sum(emission, na.rm = T)) |> 
        rename(source = NFR_wide.y) |> 
        filter(source == selected_label)
    }
    
    
    ggplotly(  
      ggplot(filtered_data) +
        geom_point(aes(x = as.Date(year), y = emission, colour = status, group = source, shape = status, linetype = status)) +
        geom_line(aes(x = as.Date(year), y = emission, colour = status, group = source, shape = status, linetype = status)) +
        scale_colour_manual(values = c("#3E5622", "darkgrey")) +
        scale_x_date(name = "Year", limits = c(as.Date("1990-01-01"), as.Date("2050-12-31"))) +
        scale_y_continuous(name = "Emissions") +
        ggtitle(unique(filtered_data$source)) +
        theme(panel.grid.major.x = element_blank(), 
              panel.grid.major.y = element_line(colour = "lightgrey"), 
              plot.title = element_text(face = "bold"))) |> 
      layout(legend = list(
        x = 0.75, 
        y = 0.85
      ))
  }) 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)