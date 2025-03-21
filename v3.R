# app.R 파일로 저장할 독립 실행형 Shiny 앱 코드

# 필요한 라이브러리 로드
library(tidyverse) 
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(DT)

# Global variable to avoid jsonlite warnings
country_name_field <- "name"

# 데이터 로드 및 전처리
co2_data <- read_csv("https://raw.githubusercontent.com/JIHONGKING/Data_Analysis/refs/heads/main/carbon.csv") %>%
  rename(
    country = Country,
    region = Region,
    date = Date,
    emissions = `Kilotons of Co2`,
    per_capita = `Metric Tons Per Capita`
  ) %>%
  mutate(
    year = case_when(
      grepl("^\\d{2}-\\d{2}-(\\d{4})$", date) ~ as.numeric(sub("^\\d{2}-\\d{2}-(\\d{4})$", "\\1", date)),
      grepl("^(\\d{4})-\\d{2}-\\d{2}$", date) ~ as.numeric(sub("^(\\d{4})-\\d{2}-\\d{2}$", "\\1", date)),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(year))

# 세계 지도 데이터 로드
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# 국가명 매핑 정의
country_mapping_list <- list(
  "United States of America" = "United States",
  "United Kingdom" = "United Kingdom of Great Britain and Northern Ireland",
  "Czech Republic" = "Czechia"
)

# 루프를 사용한 매핑 적용
for (i in 1:length(country_mapping_list)) {
  old_name <- names(country_mapping_list)[i]
  new_name <- country_mapping_list[[i]]
  world_map[[country_name_field]][world_map[[country_name_field]] == old_name] <- new_name
}

# 성능 최적화를 위한 연도 사전 계산
unique_years <- sort(unique(co2_data$year))
max_year <- max(unique_years)

# UI 정의
ui <- fluidPage(
  titlePanel("Global CO2 Emissions Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", 
                  choices = unique_years, 
                  selected = max_year),
      
      radioButtons("view_type", "View Type:",
                   choices = list(
                     "Total Emissions" = "total", 
                     "Per Capita Emissions" = "per_capita"
                   )),
      
      hr(),
      
      selectInput(
        "country_select",
        "Quick Navigate to Top Emitters:",
        choices = list("Select a country" = ""),
        selected = "",
        selectize = FALSE,
        size = 10,
        width = "100%"
      ),
      
      hr(),
      
      helpText("This map shows CO2 emissions by country. Select a year to see how emissions change over time.")
    ),
    mainPanel(
      leafletOutput("co2_map", height = "500px"),
      div(style = "margin-top: 20px;"),
      DTOutput("country_data")
    )
  )
)

# 서버 정의
server <- function(input, output, session) {
  
  getData <- reactive({
    year_data <- co2_data %>% 
      filter(year == input$year)
    
    temp_data <- year_data
    names(temp_data)[names(temp_data) == "country"] <- country_name_field
    
    left_join(world_map, temp_data, by = country_name_field)
  })
  
  observe({
    data <- co2_data %>% 
      filter(year == input$year) %>%
      arrange(desc(if(input$view_type == "total") emissions else per_capita)) %>%
      head(20)
    
    country_list <- as.list(data$country)
    names(country_list) <- data$country
    
    choices_list <- c(list("Select a country" = ""), country_list)
    
    updateSelectInput(session, "country_select", 
                      choices = choices_list,
                      selected = "")
  })
  
  output$co2_map <- renderLeaflet({
    data <- getData()
    
    if(input$view_type == "total") {
      domain <- c(0, max(data$emissions, na.rm = TRUE))
      pal <- colorNumeric(palette = "YlOrRd", domain = domain, na.color = "#CCCCCC")
      title <- "CO2 Emissions (kt)"
      valueCol <- "emissions"
    } else {
      domain <- c(0, max(data$per_capita, na.rm = TRUE))
      pal <- colorNumeric(palette = "YlOrRd", domain = domain, na.color = "#CCCCCC")
      title <- "CO2 Emissions per Capita (t)"
      valueCol <- "per_capita"
    }
    
    if(input$view_type == "total") {
      fillColors <- ifelse(is.na(data$emissions), "#CCCCCC", pal(data$emissions))
    } else {
      fillColors <- ifelse(is.na(data$per_capita), "#CCCCCC", pal(data$per_capita))
    }
    
    tooltips <- vector("character", nrow(data))
    
    for(i in 1:nrow(data)) {
      country <- data[[country_name_field]][i]
      
      if(input$view_type == "total") {
        value <- data$emissions[i]
        if(!is.na(value)) {
          tooltips[i] <- paste0(
            "<strong>", country, "</strong><br/>",
            "CO2 Emissions: ", format(value, big.mark = ","), " kt"
          )
        } else {
          tooltips[i] <- paste0("<strong>", country, "</strong><br/>No emission data")
        }
      } else {
        value <- data$per_capita[i]
        if(!is.na(value)) {
          tooltips[i] <- paste0(
            "<strong>", country, "</strong><br/>",
            "CO2 per Capita: ", round(value, 2), " t/capita"
          )
        } else {
          tooltips[i] <- paste0("<strong>", country, "</strong><br/>No emission data")
        }
      }
    }
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = fillColors,
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
        label = lapply(tooltips, HTML),
        labelOptions = labelOptions(
          style = list(
            "font-family" = "Arial, sans-serif",
            "font-size" = "12px",
            "padding" = "6px 10px",
            "background-color" = "white", 
            "box-shadow" = "0 0 15px rgba(0,0,0,0.2)",
            "border-radius" = "4px"
          ),
          direction = "auto",
          offset = c(0, -5),
          opacity = 0.9,
          textsize = "12px"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = data[[valueCol]],
        title = title,
        opacity = 0.7,
        na.label = "No data",
        labFormat = labelFormat(
          prefix = "",
          suffix = if(input$view_type == "total") " kt" else " t/capita"
        )
      )
  })
  
  observeEvent(input$country_select, {
    if(input$country_select != "") {
      data <- getData()
      country_data <- data[data[[country_name_field]] == input$country_select, ]
      
      if(nrow(country_data) > 0) {
        tryCatch({
          bounds <- st_bbox(st_geometry(country_data))
          
          leafletProxy("co2_map") %>%
            fitBounds(
              bounds[["xmin"]] - 1, bounds[["ymin"]] - 1,
              bounds[["xmax"]] + 1, bounds[["ymax"]] + 1
            )
        }, error = function(e) {
          leafletProxy("co2_map") %>% 
            setView(lng = 0, lat = 0, zoom = 2)
        })
      }
    }
  }, ignoreInit = TRUE)
  
  output$country_data <- renderDT({
    co2_data %>% 
      filter(year == input$year) %>%
      arrange(desc(if(input$view_type == "total") emissions else per_capita)) %>%
      select(country, emissions, per_capita) %>%
      rename("Country" = country, 
             "CO2 Emissions (kt)" = emissions, 
             "CO2 per Capita (t)" = per_capita)
  }, options = list(pageLength = 10))
}

# 앱 실행
shinyApp(ui = ui, server = server)
