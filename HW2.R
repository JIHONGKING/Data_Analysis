# Load necessary packages
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(leaflet.extras)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load Data
starbucks_data <- read_csv("https://raw.githubusercontent.com/JIHONGKING/Min/refs/heads/main/startbucks.csv")

# Data Preprocessing
starbucks_data <- starbucks_data %>%
  select(storeNumber, countryCode, ownershipTypeCode, latitude, longitude, streetAddressLine1, streetAddressLine2) %>%
  mutate(full_address = ifelse(is.na(streetAddressLine2), streetAddressLine1, 
                               paste(streetAddressLine1, streetAddressLine2, sep=", "))) %>%
  na.omit()

# UI Component
ui <- fluidPage(
  titlePanel("Starbucks Global Store Analysis"),
  
  fluidRow(
    column(4, selectInput("selected_country", "Select Country:",
                          choices = c("ALL", unique(starbucks_data$countryCode)),  
                          selected = "ALL")),
    column(4, selectInput("selected_ownership", "Select Ownership Type:",
                          choices = c("ALL", "Company Owned (CO)", "Licensed Store (LS)"),
                          selected = "ALL"))
  ),
  
  tabsetPanel(
    tabPanel("Store Location Map", 
             leafletOutput("map", height = "800px")),
    
    tabPanel("Choropleth Map", 
             leafletOutput("choropleth_map", height = "800px"))
  ),
  
  fluidRow(
    column(12, 
           h3("Store Details"),
           tableOutput("store_table"))
  )
)

# Server logic
server <- function(input, output, session) {
  
  bounds <- reactiveValues(minLat = -90, maxLat = 90, minLon = -180, maxLon = 180)
  
  observe({
    if (!is.null(input$map_bounds)) {
      bounds$minLat <- input$map_bounds$south
      bounds$maxLat <- input$map_bounds$north
      bounds$minLon <- input$map_bounds$west
      bounds$maxLon <- input$map_bounds$east
    }
  })
  
  filtered_data <- reactive({
    data <- starbucks_data
    
    if (input$selected_country != "ALL") {
      data <- data %>% filter(countryCode == input$selected_country)
    }
    
    if (input$selected_ownership == "Company Owned (CO)") {
      data <- data %>% filter(ownershipTypeCode == "CO")
    } else if (input$selected_ownership == "Licensed Store (LS)") {
      data <- data %>% filter(ownershipTypeCode == "LS")
    }
    
    data %>%
      filter(latitude >= bounds$minLat & latitude <= bounds$maxLat,
             longitude >= bounds$minLon & longitude <= bounds$maxLon)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(lng1 = min(starbucks_data$longitude, na.rm = TRUE),
                lat1 = min(starbucks_data$latitude, na.rm = TRUE),
                lng2 = max(starbucks_data$longitude, na.rm = TRUE),
                lat2 = max(starbucks_data$latitude, na.rm = TRUE)) %>%
      addScaleBar(position = "bottomleft")
  })
  
  observe({
    data <- filtered_data()
    
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        popup = ~paste("<b>Store Number:</b>", storeNumber,
                      "<br><b>Address:</b>", full_address,
                      "<br><b>Type:</b>", ownershipTypeCode),
        radius = 4,
        color = ~ifelse(ownershipTypeCode == "CO", "blue", "red"),
        fillOpacity = 0.7,
        stroke = FALSE
      )
  })
  
  output$choropleth_map <- renderLeaflet({
    country_summary <- starbucks_data %>%
      group_by(countryCode) %>%
      summarise(store_count = n())
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    world_starbucks <- left_join(world, country_summary, 
                                by = c("iso_a2" = "countryCode"))
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = world_starbucks$store_count,
      na.color = "transparent"
    )
    
    leaflet(world_starbucks) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(store_count),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        popup = ~paste(name, "<br>Stores:", store_count),
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.8,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~store_count,
        title = "Number of Stores",
        opacity = 0.7
      ) %>%
      addScaleBar(position = "bottomleft")
  })
  
  output$store_table <- renderTable({
    filtered_data() %>%
      select(storeNumber, full_address, ownershipTypeCode) %>%
      rename(
        "Store Number" = storeNumber,
        "Address" = full_address,
        "Ownership Type" = ownershipTypeCode
      )
  })
}

# Run Shiny App with specified dimensions
options(shiny.height = 1200)
shinyApp(ui = ui, server = server)
