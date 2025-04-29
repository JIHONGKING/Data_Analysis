# Fully implemented Shiny app for CO2 emission visualization
# Group 18: CO2 Emissions Exploratory Interface

# Load required libraries
library(shiny)
library(tidyverse)
library(DT)
library(scales)
library(plotly)

# Define UI
ui <- fluidPage(
  # App title and description
  titlePanel("Global CO2 Emissions Explorer (1990-2019)"),
  p("This interactive dashboard visualizes CO2 emissions across regions and countries over time, 
    allowing comparison of emission patterns from multiple perspectives."),
  
  # Tab layout
  tabsetPanel(
    # Heatmap tab
    tabPanel("Regional Heatmap", 
             sidebarLayout(
               sidebarPanel(
                 h4("Year range:"),
                 sliderInput("year_range",
                             "",
                             min = 1990,
                             max = 2019,
                             value = c(1990, 2019),
                             step = 1,
                             sep = ""),
                 
                 h4("Regions to display:"),
                 checkboxGroupInput('regions_selected', '',
                                    c('Africa', 'Americas', 'Asia', 'Europe', 'Oceania'),
                                    selected = c('Africa', 'Americas', 'Asia', 'Europe', 'Oceania')),
                 
                 radioButtons("data_type", "Data type:",
                              choices = list(
                                "Total CO₂ Emissions (Kilotons)" = "total", 
                                "Per Capita CO₂ Emissions (Metric Tons)" = "per_capita"
                              ),
                              selected = "total"),
                 
                 radioButtons("sort_by", "Sort regions by:",
                              choices = list(
                                "Total Emissions (Descending)" = "desc",
                                "Total Emissions (Ascending)" = "asc",
                                "Name (A-Z)" = "name"
                              ),
                              selected = "desc"),
                 
                 selectInput("color_scheme", "Color scheme:",
                             choices = list(
                               "Yellow-Orange-Red" = "YlOrRd",
                               "Red-Yellow-Blue" = "RdYlBu",
                               "Red-Yellow-Green" = "RdYlGn",
                               "Viridis" = "viridis"
                             ),
                             selected = "YlOrRd"),
                 
                 checkboxInput("use_log_scale", "Use logarithmic scale", FALSE),
                 
                 hr(),
                 downloadButton("download_heatmap_data", "Download Data"),
                 br(), br(),
                 actionButton("reset_filters", "Reset All Filters", 
                              icon = icon("refresh"), 
                              style = "width: 100%")
               ),
               
               mainPanel(
                 plotOutput("heatmap_plot", height = "400px"),
                 br(),
                 h4("Data Table"),
                 DT::dataTableOutput("heatmap_table"),
                 br(),
                 h4("Insights"),
                 htmlOutput("heatmap_insights")
               )
             )
    ),
    
    # Trend chart tab
    tabPanel("Trend Analysis", 
             sidebarLayout(
               sidebarPanel(
                 h4("Select year range:"),
                 sliderInput("trend_year_range", "",
                             min = 1990, 
                             max = 2019,
                             value = c(1990, 2019),
                             step = 1,
                             sep = ""),
                 
                 h4("Select regions:"),
                 checkboxGroupInput("trend_regions", "",
                                    c('Africa', 'Americas', 'Asia', 'Europe', 'Oceania'),
                                    selected = c('Africa', 'Americas', 'Asia', 'Europe', 'Oceania')),
                 
                 radioButtons("trend_data_type", "Data type:",
                              choices = list(
                                "Total CO₂ Emissions (Kilotons)" = "total", 
                                "Per Capita CO₂ Emissions (Metric Tons)" = "per_capita"
                              ),
                              selected = "total"),
                 
                 radioButtons("trend_chart_type", "Chart type:",
                              choices = list(
                                "Line Chart" = "line",
                                "Area Chart" = "area",
                                "Stacked Area Chart" = "stacked"
                              ),
                              selected = "line"),
                 
                 checkboxInput("show_global_avg", "Show global average", TRUE),
                 
                 hr(),
                 downloadButton("download_trend_data", "Download Data"),
                 br(), br(),
                 actionButton("reset_trend", "Reset Trend Filters", 
                              icon = icon("refresh"), 
                              style = "width: 100%")
               ),
               
               mainPanel(
                 plotlyOutput("trend_plot", height = "500px"),
                 br(),
                 h4("Trend Insights"),
                 htmlOutput("trend_insights"),
                 br(),
                 h4("Regional Comparison"),
                 plotOutput("region_comparison", height = "300px")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load and preprocess data
  load_data <- reactive({
    read_csv("https://raw.githubusercontent.com/JIHONGKING/Data_Analysis/refs/heads/main/carbon.csv") %>% 
      mutate(Year = as.numeric(substring(Date, 7, 10)))
  })
  
  # Aggregate data by region
  region_data <- reactive({
    data <- load_data()
    
    if(input$data_type == "total") {
      data %>% 
        group_by(Region, Year) %>% 
        summarise(value = sum(`Kilotons of Co2`, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(metric = "Total CO₂ Emissions (Kilotons)")
    } else {
      data %>% 
        group_by(Region, Year) %>% 
        summarise(value = mean(`Metric Tons Per Capita`, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(metric = "Per Capita CO₂ Emissions (Metric Tons)")
    }
  })
  
  # Data for heatmap
  filtered_heatmap_data <- reactive({
    df <- region_data() %>%
      filter(Year >= input$year_range[1], 
             Year <= input$year_range[2], 
             Region %in% input$regions_selected)
    
    if(input$sort_by == "desc") {
      df %>% 
        group_by(Region) %>% 
        mutate(total_value = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(Region = fct_reorder(Region, total_value, .desc = TRUE)) %>%
        select(-total_value)
    } else if(input$sort_by == "asc") {
      df %>% 
        group_by(Region) %>% 
        mutate(total_value = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(Region = fct_reorder(Region, total_value)) %>%
        select(-total_value)
    } else {
      df %>% mutate(Region = factor(Region, levels = sort(unique(Region))))
    }
  })
  
  # Data for trend analysis
  filtered_trend_data <- reactive({
    if(input$trend_data_type == "total") {
      load_data() %>%
        filter(Year >= input$trend_year_range[1],
               Year <= input$trend_year_range[2],
               Region %in% input$trend_regions) %>%
        group_by(Region, Year) %>%
        summarise(value = sum(`Kilotons of Co2`, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(metric = "Total CO₂ Emissions (Kilotons)")
    } else {
      load_data() %>%
        filter(Year >= input$trend_year_range[1],
               Year <= input$trend_year_range[2],
               Region %in% input$trend_regions) %>%
        group_by(Region, Year) %>%
        summarise(value = mean(`Metric Tons Per Capita`, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(metric = "Per Capita CO₂ Emissions (Metric Tons)")
    }
  })
  
  # Global average data
  global_avg_data <- reactive({
    if(input$trend_data_type == "total") {
      load_data() %>%
        filter(Year >= input$trend_year_range[1],
               Year <= input$trend_year_range[2]) %>%
        group_by(Year) %>%
        summarise(value = sum(`Kilotons of Co2`, na.rm = TRUE) / n_distinct(Region),
                  .groups = "drop") %>%
        mutate(Region = "Global Average",
               metric = "Total CO₂ Emissions (Kilotons)")
    } else {
      load_data() %>%
        filter(Year >= input$trend_year_range[1],
               Year <= input$trend_year_range[2]) %>%
        group_by(Year) %>%
        summarise(value = mean(`Metric Tons Per Capita`, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(Region = "Global Average",
               metric = "Per Capita CO₂ Emissions (Metric Tons)")
    }
  })
  
  # Render heatmap
  output$heatmap_plot <- renderPlot({
    req(filtered_heatmap_data())
    df <- filtered_heatmap_data()
    
    if(input$use_log_scale && all(df$value > 0)) {
      df$value <- log10(df$value)
      value_label <- paste("Log10(", unique(df$metric), ")")
    } else {
      value_label <- unique(df$metric)
    }
    
    color_function <- if(input$color_scheme == "viridis") {
      scale_fill_viridis_b(option = "plasma", n.breaks = 7)
    } else {
      scale_fill_distiller(
        palette = input$color_scheme, 
        direction = if(input$color_scheme == "RdYlBu") 1 else -1,
        n.breaks = 7
      )
    }
    
    ggplot(df, aes(x = Year, y = Region, fill = value)) +
      geom_tile(color = "white", size = 0.2) +
      color_function +
      labs(
        title = paste("CO₂ Emissions by Region (", input$year_range[1], "-", input$year_range[2], ")", sep = ""),
        x = "Year",
        y = "Region",
        fill = value_label
      ) +
      scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2], by = 2)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = 'black', size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_blank()
      )
  })
  
  # Render heatmap data table
  output$heatmap_table <- DT::renderDataTable({
    req(filtered_heatmap_data())
    df <- filtered_heatmap_data() %>%
      pivot_wider(names_from = Year, values_from = value) %>%
      select(-metric)
    
    datatable(
      df,
      options = list(
        pageLength = 5,
        dom = 'tp',
        scrollX = TRUE
      )
    ) %>%
      formatRound(columns = as.character(seq(input$year_range[1], input$year_range[2])), digits = 2)
  })
  
  # Generate heatmap insights
  output$heatmap_insights <- renderUI({
    req(filtered_heatmap_data())
    df <- filtered_heatmap_data()
    
    latest_year <- max(df$Year)
    latest_data <- df %>% filter(Year == latest_year)
    max_region <- latest_data %>% arrange(desc(value)) %>% slice(1)
    
    start_year <- input$year_range[1]
    change_data <- df %>%
      filter(Year %in% c(start_year, latest_year)) %>%
      pivot_wider(names_from = Year, values_from = value) %>%
      mutate(change_pct = (!!sym(as.character(latest_year)) - !!sym(as.character(start_year))) /
                           !!sym(as.character(start_year)) * 100)
    largest_change <- change_data %>% arrange(desc(abs(change_pct))) %>% slice(1)
    
    HTML(paste0(
      "<p>In ", latest_year, ", <strong>", max_region$Region, "</strong> had the highest ", 
      tolower(unique(df$metric)), " at <strong>",
      format(round(max_region$value, 2), big.mark = ","), "</strong>.</p>",
      "<p>From ", start_year, " to ", latest_year, ", <strong>", largest_change$Region, "</strong> recorded the largest ", 
      ifelse(largest_change$change_pct >= 0, "increase", "decrease"), 
      " (", round(abs(largest_change$change_pct), 2), "%).</p>",
      "<p><em>Darker colors on the heatmap indicate higher emission values.</em></p>"
    ))
  })
  
  # Render trend plot
  output$trend_plot <- renderPlotly({
    req(filtered_trend_data())
    trend_data <- filtered_trend_data()
    
    if(input$show_global_avg) {
      trend_data <- rbind(trend_data, global_avg_data())
    }
    
    if(input$trend_chart_type == "line") {
      p <- ggplot(trend_data, aes(x = Year, y = value, color = Region, group = Region)) +
        geom_line(size = 1.2) +
        geom_point(size = 2)
      
      if(input$show_global_avg) {
        p <- p + geom_line(data = subset(trend_data, Region == "Global Average"),
                           aes(x = Year, y = value),
                           linetype = "dashed",
                           size = 1.5)
      }
      
    } else if(input$trend_chart_type == "area") {
      p <- ggplot(trend_data, aes(x = Year, y = value, fill = Region, group = Region)) +
        geom_area(alpha = 0.6, position = "identity")
      
    } else { # stacked
      p <- ggplot(trend_data, aes(x = Year, y = value, fill = Region)) +
        geom_area(alpha = 0.8, position = "stack")
    }
    
    p <- p +
      labs(
        title = paste("CO₂ Emissions Trends by Region (", 
                      input$trend_year_range[1], "-", input$trend_year_range[2], ")", sep = ""),
        x = "Year",
        y = unique(trend_data$metric),
        color = "Region",
        fill = "Region"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = 'black', size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
    
    ggplotly(p) %>% layout(hovermode = "x unified")
  })
  
  # Generate trend insights
  output$trend_insights <- renderUI({
    req(filtered_trend_data())
    df <- filtered_trend_data()
    
    start_year <- min(df$Year)
    end_year <- max(df$Year)
    
    growth_rates <- df %>%
      group_by(Region) %>%
      summarise(
        start_value = value[Year == start_year],
        end_value = value[Year == end_year],
        growth_rate = (end_value - start_value) / start_value * 100,
        .groups = "drop"
      ) %>%
      arrange(desc(growth_rate))
    
    highest_growth <- growth_rates %>% slice(1)
    
    total_start <- sum(growth_rates$start_value, na.rm = TRUE)
    total_end <- sum(growth_rates$end_value, na.rm = TRUE)
    total_change <- (total_end - total_start) / total_start * 100
    
    HTML(paste0(
      "<p>Between ", start_year, " and ", end_year, ", <strong>", highest_growth$Region, 
      "</strong> showed the highest growth rate at <strong>", 
      round(highest_growth$growth_rate, 2), "%</strong>.</p>",
      "<p>Overall, total ", tolower(unique(df$metric)), " across all selected regions ", 
      ifelse(total_change >= 0, "increased", "decreased"), " by <strong>",
      round(abs(total_change), 2), "%</strong> during this period.</p>",
      "<p>The chart reveals ",
      ifelse(input$trend_data_type == "total", 
             "how Asia's emissions accelerated dramatically after 2000, while Europe's growth moderated.",
             "significant variations in per capita emissions, with Europe showing a declining trend while Asia continues to rise."),
      "</p>"
    ))
  })
  
  # Render regional comparison plot
  output$region_comparison <- renderPlot({
    req(filtered_trend_data())
    df <- filtered_trend_data()
    
    start_year <- min(df$Year)
    end_year <- max(df$Year)
    comparison_data <- df %>%
      filter(Year %in% c(start_year, end_year)) %>%
      mutate(Year = as.factor(Year))
    
    ggplot(comparison_data, aes(x = Region, y = value, fill = Year)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      scale_fill_brewer(palette = "Set1") +
      labs(
        title = paste("Regional Comparison:", start_year, "vs", end_year),
        x = "Region",
        y = unique(df$metric),
        fill = "Year"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10)
      )
  })
  
  # Download handler for heatmap data
  output$download_heatmap_data <- downloadHandler(
    filename = function() {
      paste("co2_emissions_heatmap_", input$year_range[1], "-", input$year_range[2], ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_heatmap_data(), file, row.names = FALSE)
    }
  )

  # Download handler for trend data
  output$download_trend_data <- downloadHandler(
    filename = function() {
      paste("co2_emissions_trends_", input$trend_year_range[1], "-", input$trend_year_range[2], ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_trend_data(), file, row.names = FALSE)
    }
  )
  
  # Reset filters for heatmap
  observeEvent(input$reset_filters, {
    updateSliderInput(session, "year_range", value = c(1990, 2019))
    updateCheckboxGroupInput(session, "regions_selected", 
                             selected = c('Africa', 'Americas', 'Asia', 'Europe', 'Oceania'))
    updateRadioButtons(session, "data_type", selected = "total")
    updateRadioButtons(session, "sort_by", selected = "desc")
    updateSelectInput(session, "color_scheme", selected = "YlOrRd")
    updateCheckboxInput(session, "use_log_scale", value = FALSE)
  })
  
  # Reset filters for trend tab
  observeEvent(input$reset_trend, {
    updateSliderInput(session, "trend_year_range", value = c(1990, 2019))
    updateCheckboxGroupInput(session, "trend_regions", 
                             selected = c('Africa', 'Americas', 'Asia', 'Europe', 'Oceania'))
    updateRadioButtons(session, "trend_data_type", selected = "total")
    updateRadioButtons(session, "trend_chart_type", selected = "line")
    updateCheckboxInput(session, "show_global_avg", value = TRUE)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
