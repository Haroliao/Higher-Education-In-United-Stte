library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(stringr)
library(readr)
library(maps)        # For map data
library(htmltools)   # For HTML labels
library(tidyr)       # For replace_na
library(plotly)      # For interactive plots

# ----------------------------
# 1. Load and Process Data
# ----------------------------

# Load PollData
PollData <- read.csv("nces330_20.csv", stringsAsFactors = FALSE)

# Load UniData
UniData <- read.csv("heis_usa.csv", stringsAsFactors = FALSE)

# ----------------------------
# 1.1 Process UniData
# ----------------------------

# Clean the 'region' column: trim whitespace and convert to lowercase
UniData <- UniData %>%
  mutate(
    region = str_trim(region, side = "both"),  # Remove leading and trailing spaces
    region = tolower(region)                   # Convert to lowercase for consistency
  )

# ----------------------------
# 1.2 Handle Special Cases and Verify Regions
# ----------------------------

# Handle special cases:
# - Include Washington, D.C.
# - Standardize "new york state" to "new york"
UniData <- UniData %>%
  mutate(
    region = case_when(
      region == "dc" | region == "washington, d.c." ~ "washington, d.c.",  # Keep Washington, D.C.
     
      
      TRUE ~ region
    )
  )


# ----------------------------
# 1.3 Aggregate Universities by State
# ----------------------------

# Aggregate Total Universities per State
total_uni <- UniData %>%
  group_by(region) %>%                # Group by state
  summarise(total_count = n()) %>%    # Count total universities
  ungroup()

# Aggregate Public Universities per State
public_uni <- UniData %>%
  filter(str_to_lower(category) == "public") %>%    # Case-insensitive filtering for public universities
  group_by(region) %>%                              # Group by state
  summarise(public_count = n()) %>%                 # Count public universities
  ungroup()

# Merge Total and Public Counts
uni_aggregation <- total_uni %>%
  left_join(public_uni, by = "region") %>%
  mutate(
    public_count = ifelse(is.na(public_count), 0, public_count),    # Replace NA with 0 for states with no public universities
    percentage = ifelse(total_count > 0, (public_count / total_count) * 100, 0)  # Calculate percentage, avoid division by zero
  )

# ----------------------------
# 1.4 Verify Data Consistency
# ----------------------------

# List of unique regions in uni_aggregation
unique_regions <- unique(uni_aggregation$region)

# List of states from the maps package (lowercase)
map_states <- tolower(state.name)

# Include Washington, D.C. in map_states if necessary
map_states_extended <- c(map_states, "washington, d.c.")

# Identify any regions not matching the map states
mismatched_regions <- setdiff(unique_regions, map_states_extended)

if(length(mismatched_regions) > 0){
  warning("The following regions do not match any state names in the map data (including Washington, D.C.):")
  print(mismatched_regions)
}

# ----------------------------
# 2. Define UI Components
# ----------------------------

# Plot Tab: Tuition Fees and Room/Board Trends (Public Only) with Bar Plot
plot_tab <- tabPanel(
  title = "Tuition Fees by State",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "margin-top: 200px;",  # Increased top margin to push content further down
      fluidRow(
        column(
          width = 10,
          selectInput(
            inputId = "selected_state",
            label = "Select a State:",
            choices = sort(unique(tolower(PollData$State))),  # Ensure state names are lowercase for consistency
            selected = "alabama"
          )
        )
      ),
      fluidRow(
        column(12)  
      ),
      fluidRow(
        column(
          width = 10,
          actionButton("toggle_type", "Switch to Public Out-of-State")
        )
      )
    ),
    mainPanel(
      width = 9,  # Increased main panel width
      plotlyOutput('plot_fee', height = "300px"),
      tags$hr(),
      br(),
      fluidRow(
        column(
          width = 10,
          align = "left",
          div(class = "left-aligned-plot", plotlyOutput('plot_total_fees', height = "300px"))
        )
      )
    )
  )
)

# Map Tab: Public Universities by State
map_tab <- tabPanel(
  title = "Map of Public Universities",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "margin-top: 30px;",
      radioButtons(
        inputId = "show_mode",
        label = "Display:",
        choices = c("Percentage" = "percentage", "Number" = "number"),
        selected = "percentage",
        inline = TRUE
      ),
      hr(),
      uiOutput("public_uni_list")
    ),
    mainPanel(
      width = 9,
      leafletOutput("uni_map", height = "600px")
    )
  )
)

# Define the overall UI with a multi-tab layout and custom CSS
ui <- navbarPage(
  title = div(
    "American Public",  
    tags$br(),          
    "Tertiary Education" 
  ),
  header=tags$head(
    tags$style(HTML("
      .left-aligned-plot {
        text-align: left;
        margin-left: 0px; 
        width: 100%;      
      }
      .sidebar .well {
        padding-top: 20px; 
      }
    "))
  ),
  plot_tab,
  map_tab
)

# ----------------------------
# 3. Define Server Logic
# ----------------------------

server <- function(input, output, session) {
  
  # ----------------------------
  # 3.1 Toggle Between In-State and Out-of-State
  # ----------------------------
  
  current_type <- reactiveVal("Public In-State")
  
  observeEvent(input$toggle_type, {
    if (current_type() == "Public In-State") {
      current_type("Public Out-of-State")
      updateActionButton(session, "toggle_type", label = "Switch to Public In-State")
    } else {
      current_type("Public In-State")
      updateActionButton(session, "toggle_type", label = "Switch to Public Out-of-State")
    }
  })
  
  # ----------------------------
  # 3.2 Reactive Expression for Line Chart Data
  # ----------------------------
  
  filtered_data <- reactive({
    PollData %>%
      filter(
        Expense %in% c("Fees/Tuition", "Room/Board"),
        Length == "4-year",
        Type == current_type(),
        tolower(State) == input$selected_state,  # Ensure case-insensitive matching
        Year >= 2013, Year <= 2020
      ) %>%
      mutate(
        Expense = case_when(
          Expense == "Fees/Tuition" ~ "Tuition Fees",
          Expense == "Room/Board" ~ "Room and Board",
          TRUE ~ Expense
        )
      )
  })
  
  # ----------------------------
  # 3.3 Render the Line Plot with Plotly
  # ----------------------------
  
  output$plot_fee <- renderPlotly({
    data <- filtered_data()
    
    if(nrow(data) == 0){
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No data available for the selected options."))
    }
    
    p <- ggplot(data, aes(x = Year, y = Value, color = Expense)) +
      geom_line(linewidth = 1.2) +
      labs(
        x = "Year",
        y = "Cost (USD)",
        title = paste("Trend of Annual Tuition and Boarding Fees for a 4-Year Public Program in", str_to_title(input$selected_state)),
        color = "Expense Type"
      ) +
      scale_x_continuous(breaks = seq(2013, 2020, by = 1)) +
      scale_color_manual(values = c("Tuition Fees" = "blue", "Room and Board" = "red")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11)
      )
    
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(
        legend = list(title = list(text = 'Expense Type')),
        title = list(
          text = paste("Trend of Annual Tuition and Boarding Fees for a 4-Year", 
                       current_type(), "<br>Public Program in", str_to_title(input$selected_state)),
          x = 0.5,
          xanchor = "center"
        )
      )
  })
  
  # ----------------------------
  # 3.4 Reactive Expression for Bar Plot Data
  # ----------------------------
  
  total_fees_data <- reactive({
    filtered_data() %>%
      group_by(Year) %>%
      summarise(
        Tuition_Fees = sum(Value[Expense == "Tuition Fees"], na.rm = TRUE),
        Room_and_Board = sum(Value[Expense == "Room and Board"], na.rm = TRUE),
        Fees = Tuition_Fees + Room_and_Board
      ) %>%
      ungroup()
  })
  
  # ----------------------------
  # 3.5 Render the Horizontal Bar Plot
  # ----------------------------
  
  output$plot_total_fees <- renderPlotly({
    data <- total_fees_data()
    
    if(nrow(data) == 0){
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No data available for the selected options."))
    }
    
    p <- ggplot(data, aes(x = Year, y = Fees, fill = "Total Fees")) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        x = "Year",
        y = "Total Fees (USD)",
        title = paste("Total Annual Fees for a 4-Year Public Program in", 
                      str_to_title(input$selected_state), "(", current_type(), ")"),
        fill = ""
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12)
      )
    
    ggplotly(p, tooltip = c("y", "x")) %>%
      layout(
        legend = list(orientation = "h", x = 0.4, y = 1.1),
        title = list(
          text = paste("Total Annual Fees for a 4-Year", 
                       current_type(), "<br>Public Program in", str_to_title(input$selected_state)),
          x = 0.5,
          xanchor = "center"
        )
      )
  })
  
  # ----------------------------
  # 3.6 Render the Leaflet Map
  # ----------------------------
  
  output$uni_map <- renderLeaflet({
    # Get US states map data
    states_map <- map("state", fill = TRUE, plot = FALSE)
    
    # Extract main state names by removing subregions
    states_map$main_state <- sapply(strsplit(states_map$names, ":"), `[`, 1)
    
    # Prepare data for mapping
    map_data <- uni_aggregation %>%
      mutate(region = tolower(region))
    
    # Create a data frame with unique main_state names
    unique_states <- unique(states_map$main_state)
    
    # Include Washington, D.C. if it's present in the data
    if("washington, d.c." %in% unique(map_data$region)){
      unique_states_extended <- c(unique_states, "washington, d.c.")
    } else {
      unique_states_extended <- unique_states
    }
    
    # Aggregate public_uni counts by state
    aggregated_public_uni <- map_data %>%
      filter(region %in% unique_states_extended) %>%
      group_by(region) %>%
      summarise(
        public_count = sum(public_count),
        percentage = mean(percentage)
      ) %>%
      ungroup()
    # Merge map data with public university counts
    merged_map <- data.frame(main_state = states_map$main_state, stringsAsFactors = FALSE) %>%
      left_join(aggregated_public_uni, by = c("main_state" = "region"))
  
    
    # Replace NA counts and percentages with 0
    merged_map$public_count[is.na(merged_map$public_count)] <- 0
    merged_map$percentage[is.na(merged_map$percentage)] <- 0
    
    
    # Define color palette based on user selection
    if (input$show_mode == "percentage") {
      pal <- colorNumeric(palette = "YlGnBu", domain = merged_map$percentage)
      legend_title <- "Percentage of Public Universities by State"
      values_to_use <- merged_map$percentage
    } else {
      pal <- colorNumeric(palette = "YlGnBu", domain = merged_map$public_count)
      legend_title <- "Number of Public Universities by State"
      values_to_use <- merged_map$public_count
    }
    
    # Create labels
    labels <- sprintf(
      "<strong>%s</strong><br/>Public Universities: %g<br/>Percentage: %.2f%%",
      tools::toTitleCase(merged_map$main_state),
      merged_map$public_count,
      merged_map$percentage
    ) %>% lapply(htmltools::HTML)
    
    # Initialize Leaflet map
    leaflet(states_map) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addPolygons(
        fillColor = ~pal(values_to_use),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = ~main_state
      ) %>%
      addLegend(
        pal = pal,
        values = values_to_use,
        opacity = 0.7,
        title = legend_title,
        position = "bottomright"
      ) %>%
      
      # Manually add Washington, D.C. polygon if necessary
      {
        if("washington, d.c." %in% unique(map_data$region)){
          addPolygons(
            .,
            lng = c(-77.1198, -76.9094, -76.9094, -77.1198),  # Approximate boundaries for Washington, D.C.
            lat = c(38.7916, 38.7916, 38.9955, 38.9955),
            color = "white",
            fillColor = pal(merged_map$percentage[merged_map$main_state == "washington, d.c."]),
            fillOpacity = 0.7,
            popup = "Washington, D.C.",
            highlightOptions = highlightOptions(
              weight = 2,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE
            )
          )
        } else {
          .
        }
      }
  })
  
  # ----------------------------
  # 3.7 Handle Map Clicks and Display University List
  # ----------------------------
  
  output$public_uni_list <- renderUI({
    click <- input$uni_map_shape_click
    
    if(is.null(click)){
      return(HTML("<em>Click on a state in the map to see the local public universities.</em>"))
    }
    
    state_clicked <- tolower(click$id)  # Ensure lowercase for consistency
    
    if(is.null(state_clicked) || state_clicked == ""){
      return(HTML("<em>Invalid state selection. Please try again.</em>"))
    }
    
    uni_list <- UniData %>%
      filter(region == state_clicked, str_to_lower(category) == "public") %>%
      select(name)
    
    if(nrow(uni_list) == 0){
      return(HTML("<em>No public universities found in this state.</em>"))
    }
    
    tagList(
      h4(paste("Public Universities in", tools::toTitleCase(state_clicked))),
      tags$ul(
        lapply(uni_list$name, function(x) tags$li(x))
      )
    )
  })
  
}

# ----------------------------
# 4. Run the Shiny App
# ----------------------------

shinyApp(ui, server)




