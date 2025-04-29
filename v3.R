# Improved interactive map for CO2 emissions
library(tidyverse) 
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(DT)

# Load and preprocess data
co2_data <- read_csv("https://raw.githubusercontent.com/JIHONGKING/Data_Analysis/refs/heads/main/carbon.csv") %>%
  rename(
    country    = Country,
    region     = Region,
    date       = Date,
    emissions  = `Kilotons of Co2`,
    per_capita = `Metric Tons Per Capita`
  ) %>%
  mutate(
    year = case_when(
      grepl("^\\d{2}-\\d{2}-(\\d{4})$", date)  ~ as.numeric(sub("^\\d{2}-\\d{2}-(\\d{4})$", "\\1", date)),
      grepl("^(\\d{4})-\\d{2}-\\d{2}$", date)  ~ as.numeric(sub("^(\\d{4})-\\d{2}-\\d{2}$", "\\1", date)),
      TRUE                                     ~ NA_real_
    )
  ) %>%
  filter(!is.na(year))

# Load world map data and exclude Antarctica
world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")

# Country name mapping
country_name_field <- "name"
country_mapping_list <- list(
  "United States of America" = "United States",
  "United Kingdom"          = "United Kingdom of Great Britain and Northern Ireland",
  "Czech Republic"          = "Czechia"
)

# Apply mapping
for (i in seq_along(country_mapping_list)) {
  old_name <- names(country_mapping_list)[i]
  new_name <- country_mapping_list[[i]]
  world_map[[country_name_field]][world_map[[country_name_field]] == old_name] <- new_name
}

# Compute unique years
unique_years <- sort(unique(co2_data$year))
max_year     <- max(unique_years)

# ----------------------- UI -----------------------
ui <- fluidPage(
  titlePanel("Global CO2 Emissions Explorer"),
  
  # Introductory text
  div(
    class = "intro-text",
    style = "margin-bottom: 20px; background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
    p("This interactive map visualizes CO2 emissions across countries from 1990 to 2019. 
      Toggle between total emissions and per capita view to explore different perspectives on climate responsibility.")
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", 
                  choices = unique_years, 
                  selected = max_year),
      
      radioButtons("view_type", "Emission View Type:",
                   choices = list(
                     "Total Emissions (Kilotons)" = "total", 
                     "Per Capita Emissions (Metric Tons)" = "per_capita"
                   )),
      hr(),
      
      # Region filter
      checkboxGroupInput("region_filter", "Filter by Region:",
                         choices = sort(unique(co2_data$region)),
                         selected = sort(unique(co2_data$region))),
      hr(),
      
      selectInput(
        "country_select",
        "Navigate to Country:",
        choices  = list("Select a country" = ""),
        selected = "",
        selectize = TRUE
      ),
      
      # Download button
      downloadButton("downloadData", "Download Current Data"),
      hr(),
      
      helpText("Click on countries to see detailed information. Use the filter options to focus on specific regions or emission types.")
    ),
    
    mainPanel(
      # Tabs: map, table, statistics
      tabsetPanel(
        tabPanel("Map View", 
                 leafletOutput("co2_map", height = "500px"),
                 div(style = "margin-top: 20px;"),
                 uiOutput("selected_country_info")),
        
        tabPanel("Data Table", 
                 div(style = "margin-top: 20px;"),
                 DTOutput("country_data")),
        
        tabPanel("Statistics", 
                 div(style = "margin-top: 20px;"),
                 plotOutput("stats_plot", height = "400px"),
                 uiOutput("stats_summary"))
      )
    )
  )
)

# --------------------- SERVER ---------------------
server <- function(input, output, session) {
  
  # Filtered data for selected year/region
  filteredData <- reactive({
    req(input$year, input$region_filter)
    
    year_data <- co2_data %>% 
      filter(year == input$year, region %in% input$region_filter)
    
    temp_data <- year_data
    names(temp_data)[names(temp_data) == "country"] <- country_name_field
    
    left_join(world_map, temp_data, by = country_name_field)
  })
  
  # Store selected country
  selectedCountry <- reactiveVal(NULL)
  
  # Update country dropdown
  observe({
    data <- co2_data %>% 
      filter(year == input$year, region %in% input$region_filter) %>%
      arrange(desc(if (input$view_type == "total") emissions else per_capita))
    
    country_list      <- data$country
    names(country_list) <- data$country
    
    updateSelectInput(session, "country_select", 
                      choices  = c("Select a country" = "", country_list),
                      selected = if (is.null(selectedCountry())) "" else selectedCountry())
  })
  
  # Render map
  output$co2_map <- renderLeaflet({
    data <- filteredData()
    
    # Set scale & palette
    if (input$view_type == "total") {
      bins   <- c(0, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, Inf)
      labels <- c("< 1k", "1k-5k", "5k-10k", "10k-50k", "50k-100k", "100k-500k", "500k-1M", "> 1M")
      pal    <- colorBin("YlOrRd", domain = c(0, max(data$emissions,   na.rm = TRUE)), bins = bins)
      title  <- "CO2 Emissions (kilotons)"
      valueCol <- "emissions"
    } else {
      bins   <- c(0, 1, 3, 5, 7, 10, 15, 20, Inf)
      labels <- c("< 1", "1-3", "3-5", "5-7", "7-10", "10-15", "15-20", "> 20")
      pal    <- colorBin("YlOrRd", domain = c(0, max(data$per_capita, na.rm = TRUE)), bins = bins)
      title  <- "CO2 Emissions per Capita (metric tons)"
      valueCol <- "per_capita"
    }
    
    # Generate tooltips
    tooltips <- vector("character", nrow(data))
    for (i in seq_len(nrow(data))) {
      country <- data[[country_name_field]][i]
      
      if (input$view_type == "total") {
        value <- data$emissions[i]
        tooltips[i] <- if (!is.na(value)) {
          paste0(
            "<strong>", country, "</strong><br/>",
            "Region: ", data$region[i], "<br/>",
            "CO2 Emissions: ", format(value, big.mark = ","), " kt"
          )
        } else {
          paste0("<strong>", country, "</strong><br/>No emission data")
        }
      } else {
        value <- data$per_capita[i]
        tooltips[i] <- if (!is.na(value)) {
          paste0(
            "<strong>", country, "</strong><br/>",
            "Region: ", data$region[i], "<br/>",
            "CO2 per Capita: ", round(value, 2), " t/capita"
          )
        } else {
          paste0("<strong>", country, "</strong><br/>No emission data")
        }
      }
    }
    
    # Create map
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%  # Center view
      addPolygons(
        fillColor   = ~pal(get(valueCol)),
        weight      = 1,
        opacity     = 1, 
        color       = "white",
        dashArray   = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight      = 2,
          color       = "#666",
          dashArray   = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = lapply(tooltips, HTML),
        labelOptions = labelOptions(
          style = list(
            "font-family"      = "Arial, sans-serif",
            "font-size"        = "12px",
            "padding"          = "6px 10px",
            "background-color" = "white", 
            "box-shadow"       = "0 0 15px rgba(0,0,0,0.2)",
            "border-radius"    = "4px"
          ),
          direction = "auto",
          offset    = c(0, -5),
          opacity   = 0.9,
          textsize  = "12px"
        ),
        layerId = ~name   # ID for click events
      ) %>%
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = bins,
        title    = title,
        opacity  = 0.7,
        labFormat = function(type, cuts, p) { labels }
      )
  })
  
  # Map click event
  observeEvent(input$co2_map_shape_click, {
    click <- input$co2_map_shape_click
    country_name <- click$id
    
    if (!is.null(country_name)) {
      selectedCountry(country_name)
      updateSelectInput(session, "country_select", selected = country_name)
    }
  })
  
  # Dropdown selection event
  observeEvent(input$country_select, {
    if (input$country_select != "") {
      selectedCountry(input$country_select)
      
      data <- filteredData()
      country_data <- data[data[[country_name_field]] == input$country_select, ]
      
      if (nrow(country_data) > 0) {
        tryCatch({
          bounds <- st_bbox(st_geometry(country_data))
          
          leafletProxy("co2_map") %>%
            fitBounds(
              bounds[["xmin"]] - 1, bounds[["ymin"]] - 1,
              bounds[["xmax"]] + 1, bounds[["ymax"]] + 1
            )
        }, error = function(e) {
          leafletProxy("co2_map") %>% 
            setView(lng = 0, lat = 20, zoom = 2)
        })
      }
    }
  }, ignoreInit = TRUE)
  
  # Selected country information panel
  output$selected_country_info <- renderUI({
    req(selectedCountry())
    
    country_info <- co2_data %>% 
      filter(country == selectedCountry(), year == input$year)
    
    if (nrow(country_info) > 0) {
      info <- country_info[1,]
      
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
        h3(paste("Selected Country:", info$country)),
        p(paste("Region:", info$region)),
        p(paste("Year:", input$year)),
        p(paste("Total CO2 Emissions:", format(info$emissions, big.mark = ","), "kilotons")),
        p(paste("CO2 Emissions per Capita:", round(info$per_capita, 2), "metric tons")),
        
        # Historical time-series plot
        h4("Historical Data:"),
        plotOutput("selected_country_trend", height = "200px")
      )
    } else {
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
        h3("No country selected or no data available for the selected country.")
      )
    }
  })
  
  # Historical trend for selected country
  output$selected_country_trend <- renderPlot({
    req(selectedCountry())
    
    trend_data <- co2_data %>% 
      filter(country == selectedCountry())
    
    if (nrow(trend_data) > 0) {
      ggplot(trend_data, aes(x = year)) +
        geom_line(aes(y = if (input$view_type == "total") emissions else per_capita), 
                  color = "#FF5252", size = 1) +
        theme_minimal() +
        labs(
          y = if (input$view_type == "total") "CO2 Emissions (kilotons)" else "CO2 per Capita (metric tons)",
          title = paste("Historical Trend for", selectedCountry())
        ) +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text  = element_text(size = 10)
        )
    } else {
      ggplot() + theme_void() + 
        annotate("text", x = 0, y = 0, label = "No historical data available", size = 5)
    }
  })
  
  # Data table
  output$country_data <- renderDT({
    data <- co2_data %>% 
      filter(year == input$year, region %in% input$region_filter) %>%
      arrange(desc(if (input$view_type == "total") emissions else per_capita)) %>%
      select(country, region, emissions, per_capita) %>%
      rename("Country"          = country, 
             "Region"           = region,
             "CO2 Emissions (kt)" = emissions, 
             "CO2 per Capita (t)" = per_capita)
    
    datatable(
      data, 
      options = list(
        pageLength = 15,
        autoWidth  = TRUE,
        scrollX    = TRUE,
        dom        = 'Bfrtip',
        buttons    = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE
    ) %>%
      formatRound("CO2 per Capita (t)", digits = 2) %>%
      formatCurrency("CO2 Emissions (kt)", currency = "", digits = 0, mark = ",")
  })
  
  # Statistics plot
  output$stats_plot <- renderPlot({
    data <- co2_data %>% 
      filter(year == input$year, region %in% input$region_filter) %>%
      group_by(region) %>%
      summarise(
        total_emissions = sum(emissions, na.rm = TRUE),
        avg_per_capita  = mean(per_capita, na.rm = TRUE)
      ) %>%
      arrange(desc(if (input$view_type == "total") total_emissions else avg_per_capita))
    
    if (input$view_type == "total") {
      ggplot(data, aes(x = reorder(region, -total_emissions), y = total_emissions)) +
        geom_bar(stat = "identity", fill = "#FF5252") +
        theme_minimal() +
        labs(
          title = paste("Total CO2 Emissions by Region in", input$year),
          x = "Region",
          y = "CO2 Emissions (kilotons)"
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title  = element_text(size = 16, face = "bold")
        ) +
        scale_y_continuous(labels = scales::comma)
    } else {
      ggplot(data, aes(x = reorder(region, -avg_per_capita), y = avg_per_capita)) +
        geom_bar(stat = "identity", fill = "#2196F3") +
        theme_minimal() +
        labs(
          title = paste("Average CO2 Emissions per Capita by Region in", input$year),
          x = "Region",
          y = "CO2 per Capita (metric tons)"
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title  = element_text(size = 16, face = "bold")
        )
    }
  })
  
  # Statistics summary
  output$stats_summary <- renderUI({
    data <- co2_data %>% 
      filter(year == input$year, region %in% input$region_filter)
    
    if (nrow(data) > 0) {
      total_global  <- sum(data$emissions,   na.rm = TRUE)
      avg_per_capita <- mean(data$per_capita, na.rm = TRUE)
      
      top_emitter   <- data %>% arrange(desc(emissions))  %>% slice(1)
      top_per_capita <- data %>% arrange(desc(per_capita)) %>% slice(1)
      
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 20px;",
        h4(paste("Summary Statistics for", input$year)),
        
        div(style = "display: flex; flex-wrap: wrap;",
            div(style = "flex: 50%; padding: 10px;",
                h5("Total Emissions"),
                p(paste("Global total:", format(total_global, big.mark = ","), "kilotons")),
                p(paste("Top emitter:", top_emitter$country, "-", format(top_emitter$emissions, big.mark = ","), "kt"))
            ),
            div(style = "flex: 50%; padding: 10px;",
                h5("Per Capita Emissions"),
                p(paste("Global average:", round(avg_per_capita, 2), "metric tons")),
                p(paste("Highest per capita:", top_per_capita$country, "-", round(top_per_capita$per_capita, 2), "t"))
            )
        )
      )
    } else {
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 20px;",
        h4("No data available for the selected filters")
      )
    }
  })
  
  # Data download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("co2_emissions_", input$year, ".csv", sep = "")
    },
    content = function(file) {
      data <- co2_data %>% 
        filter(year == input$year, region %in% input$region_filter) %>%
        select(country, region, emissions, per_capita, year)
      write.csv(data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)
