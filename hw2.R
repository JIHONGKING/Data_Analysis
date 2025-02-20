# HW2 - Starbucks Global Store Analysis
# Author: Jihong Min
# Date: 2025-02-20

# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(leaflet.extras)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load Starbucks data
starbucks_data <- read_csv("https://raw.githubusercontent.com/JIHONGKING/Min/main/startbucks.csv")

# Data Preprocessing
starbucks_data <- starbucks_data %>%
  select(storeNumber, countryCode, ownershipTypeCode, latitude, longitude, streetAddressLine1, streetAddressLine2) %>%
  mutate(full_address = ifelse(is.na(streetAddressLine2), streetAddressLine1, 
                               paste(streetAddressLine1, streetAddressLine2, sep=", "))) %>%
  na.omit()

# UI Component
ui <- fluidPage(
  titlePanel("Starbucks Global Store Analysis"),
  
  # Arrange filters horizontally
  fluidRow(
    column(4, selectInput("selected_country", "Select Country:",
                          choices = c("ALL", unique(starbucks_data$countryCode)),  
                          selected = "ALL")),
    column(4, selectInput("selected_ownership", "Select Ownership Type:",
                          choices = c("ALL", "Company Owned (CO)", "Licensed Store (LS)"),
                          selected = "ALL"))
  ),
  
  # Tabs for different maps
  div(style = "margin-top: 20px; margin-bottom: 30px;",
      tabsetPanel(
        tabPanel("Store Location Map", leafletOutput("map", height = "600px")),
        tabPanel("Choropleth Map", leafletOutput("choropleth_map", height = "600px"))
      )
  ),

  # Store table output
  fluidRow(
    column(12, tableOutput("store_table"))
  )
)

# Server Component
server <- function(input, output, session) {
  
  # Reactive values to store map bounds
  bounds <- reactiveValues(minLat = -90, maxLat = 90, minLon = -180, maxLon = 180)
  
  # Update map bounds when user moves the map
  observe({
    if (!is.null(input$map_bounds)) {
      bounds$minLat <- input$map_bounds$south
      bounds$maxLat <- input$map_bounds$north
      bounds$minLon <- input$map_bounds$west
      bounds$maxLon <- input$map_bounds$east
    }
  })
  
  # Reactive function to filter data
  filtered_data <- reactive({
    data <- starbucks_data
    
    # Filter by country
    if (input$selected_country != "ALL") {
      data <- data %>% filter(countryCode == input$selected_country)
    }
    
    # Filter by ownership type
    if (input$selected_ownership == "Company Owned (CO)") {
      data <- data %>% filter(ownershipTypeCode == "CO")
    } else if (input$selected_ownership == "Licensed Store (LS)") {
      data <- data %>% filter(ownershipTypeCode == "LS")
    }
    
    # Filter based on map bounds
    data <- data %>%
      filter(latitude >= bounds$minLat & latitude <= bounds$maxLat,
             longitude >= bounds$minLon & longitude <= bounds$maxLon) %>%
      mutate(full_address = ifelse(is.na(streetAddressLine2), streetAddressLine1, 
                                   paste(streetAddressLine1, streetAddressLine2, sep=", ")))
    
    req(data)  # Ensure data is not empty
    return(data)
  })
  
  # Render Store Location Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(lng1 = min(starbucks_data$longitude, na.rm = TRUE),
                lat1 = min(starbucks_data$latitude, na.rm = TRUE),
                lng2 = max(starbucks_data$longitude, na.rm = TRUE),
                lat2 = max(starbucks_data$latitude, na.rm = TRUE))
  })
  
  # Update Store Location Map
  observe({
    data <- filtered_data()
    req(nrow(data) > 0)  # Ensure there are data points to display
    
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(~longitude, ~latitude, popup = ~paste("Address:", full_address),
                       radius = 3, color = "blue", fillOpacity = 0.7)
  })
  
  # Render Choropleth Map
  output$choropleth_map <- renderLeaflet({
    # Compute Starbucks store count by country
    country_summary <- starbucks_data %>%
      group_by(countryCode) %>%
      summarise(store_count = n(), .groups = "drop")
    
    # Load world map
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Merge with Starbucks data
    world_starbucks <- left_join(world, country_summary, by = c("iso_a2" = "countryCode"))
    
    # Define color palette
    color_palette <- colorNumeric(palette = "YlOrRd", domain = world_starbucks$store_count, na.color = "transparent")
    
    # Generate Choropleth map
    leaflet(world_starbucks) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~color_palette(store_count),
        weight = 1, color = "white", fillOpacity = 0.7,
        popup = ~paste(name, "<br><b>Stores:</b>", store_count),
        highlight = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(
        position = "topright",
        pal = color_palette,
        values = world_starbucks$store_count,
        title = "Starbucks Stores per Country",
        opacity = 1
      )
  })
  
  # Render Table
  output$store_table <- renderTable({
    data <- filtered_data()
    req(nrow(data) > 0)  # Ensure table is not empty
    data %>% select(storeNumber, full_address, ownershipTypeCode)
  })
}

# Run Shiny App
shinyApp(ui = ui, server = server)
