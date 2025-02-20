# Load necessary packages
library(shiny)
library(leaflet)
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(readr)
library(leaflet.extras)
library(sf)
## Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE
library(rnaturalearth)
library(rnaturalearthdata)
## 
## Attaching package: 'rnaturalearthdata'
## The following object is masked from 'package:rnaturalearth':
## 
##     countries110
# Load Data
starbucks_data <- read_csv("https://raw.githubusercontent.com/JIHONGKING/Min/refs/heads/main/startbucks.csv")
## New names:
## • `` -> `...1`
## Rows: 28289 Columns: 17
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (13): storeNumber, countryCode, ownershipTypeCode, schedule, slug, stree...
## dbl  (4): ...1, latitude, longitude, currentTimeOffset
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
# Data Preprocessing
starbucks_data <- starbucks_data %>%
  select(storeNumber, countryCode, ownershipTypeCode, latitude, longitude, streetAddressLine1, streetAddressLine2) %>%
  mutate(full_address = ifelse(is.na(streetAddressLine2), streetAddressLine1, 
                               paste(streetAddressLine1, streetAddressLine2, sep=", "))) %>%
  na.omit()
# UI Component
# UI Component
# UI Component
ui <- fluidPage(
  titlePanel("Starbucks Global Store Analysis"),
  
  # Add spacing around filters
  div(style = "margin-bottom: 30px;"),  # Extra spacing
  
  # Arrange filters horizontally
  fluidRow(
    column(4, selectInput("selected_country", "Select Country:",
                          choices = c("ALL", unique(starbucks_data$countryCode)),  
                          selected = "ALL")),
    column(4, selectInput("selected_ownership", "Select Ownership Type:",
                          choices = c("ALL", "Company Owned (CO)", "Licensed Store (LS)"),
                          selected = "ALL"))
  ),
  
  div(style = "margin-bottom: 40px;"),  # Extra spacing before maps
  
  # Tabs to switch between maps
  tabsetPanel(
    tabPanel("Store Location Map", 
             div(style = "margin-bottom: 20px;"),  # Spacing above map
             leafletOutput("map", height = "auto")),
    
    tabPanel("Choropleth Map", 
             div(style = "margin-bottom: 20px;"),  # Spacing above map
             leafletOutput("choropleth_map", height = "auto"))
  ),
  
  div(style = "margin-bottom: 40px;"),  # Extra spacing before table
  
  # Display the filtered store table
  fluidRow(
    column(12, tableOutput("store_table"))
  ),
  
  div(style = "margin-bottom: 800px;")  # Extra spacing at the bottom
)
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
  
  # Reactive function to filter data based on selections & visible area
  filtered_data <- reactive({
    data <- starbucks_data

    # Filter by country if not "ALL"
    if (input$selected_country != "ALL") {
      data <- data %>% filter(countryCode == input$selected_country)
    }

    # Filter by ownership type if not "ALL"
    if (input$selected_ownership == "Company Owned (CO)") {
      data <- data %>% filter(ownershipTypeCode == "CO")
    } else if (input$selected_ownership == "Licensed Store (LS)") {
      data <- data %>% filter(ownershipTypeCode == "LS")
    }

    # Filter data based on map bounds (only show visible stores)
    data <- data %>%
      filter(latitude >= bounds$minLat & latitude <= bounds$maxLat,
             longitude >= bounds$minLon & longitude <= bounds$maxLon)

    return(data)
  })
  
  # Initialize Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(lng1 = min(starbucks_data$longitude, na.rm = TRUE),
                lat1 = min(starbucks_data$latitude, na.rm = TRUE),
                lng2 = max(starbucks_data$longitude, na.rm = TRUE),
                lat2 = max(starbucks_data$latitude, na.rm = TRUE))
  })
  
  # Update store location map when filters change
  observe({
    data <- filtered_data()

    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(~longitude, ~latitude, popup = ~paste("Address:", full_address),
                       radius = 3, color = "blue", fillOpacity = 0.7)
  })
  
  # Display only visible store list
  output$store_table <- renderTable({
    filtered_data() %>% select(storeNumber, full_address, ownershipTypeCode)
  })
  
  # Choropleth Map (Heat Map)
  # Choropleth Map (Heat Map with improved colors and legend)
output$choropleth_map <- renderLeaflet({
  # Compute country-level Starbucks store count
  country_summary <- starbucks_data %>%
    group_by(countryCode) %>%
    summarise(store_count = n())

  # Load world map data
  world <- ne_countries(scale = "medium", returnclass = "sf")

  # Merge with Starbucks data
  world_starbucks <- left_join(world, country_summary, by = c("iso_a2" = "countryCode"))

  # Define new color palette (Yellow → Orange → Red)
  color_palette <- colorNumeric(palette = "YlOrRd", domain = world_starbucks$store_count, na.color = "transparent")

  # Generate Choropleth map with a legend
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

}

# Run Shiny App
shinyApp(ui = ui, server = server)
