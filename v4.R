# Load required packages
library(shiny)
library(tidyverse)
library(plotly)
library(scales)

# Define UI
ui <- fluidPage(
  # Set main title
  titlePanel("Annual CO₂ emissions by world region"),
  
  # Add descriptive text about the data
  p("Emissions from fossil fuels and industry are included, but not land-use change emissions. 
    International aviation and shipping are included as separate entities, as they are not included in any country's emissions."),
  
  # Text output area to display selected year
  h2(textOutput("selected_year_text"), align = "center"),
  
  # Main chart display area (using plotly)
  plotlyOutput("co2_plot", height = "500px"),
  
  # Year selection slider - with animation capabilities
  sliderInput("selected_year", "Select year:", 
              min = 1990, max = 2019,
              value = 2019, 
              step = 1,
              width = "100%",
              animate = animationOptions(interval = 800, loop = FALSE)),
  
  # Data details panel (table and summary)
  wellPanel(
    h3("Emissions details"),
    uiOutput("emissions_table"),
    htmlOutput("insight_text")
  ),
  
  # Source information
  p(em("Data source: Global Carbon Budget (2024)"), style = "font-size: 12px; color: #666;")
)

# Define server
server <- function(input, output, session) {
  
  # Reactive function for loading and processing data
  carbon_data <- reactive({
    # Load dataset
    data <- read_csv("https://raw.githubusercontent.com/JIHONGKING/Data_Analysis/refs/heads/main/carbon.csv")
    
    # Extract year from date
    data <- data %>% 
      mutate(Year = strtoi(substring(Date, 7, 10)))
    
    # Calculate CO2 emissions by region (convert kilotons to million tons)
    region_totals <- data %>%
      group_by(Region, Year) %>%
      summarise(Emissions = sum(`Kilotons of Co2`, na.rm = TRUE) / 1000) %>%
      ungroup()
    
    return(region_totals)
  })
  
  # Function to assign colors to regions
  get_region_colors <- function(regions) {
    # Default color palette
    default_colors <- c(
      "Asia" = "#FF5252",
      "Europe" = "#2196F3",
      "Americas" = "#4CAF50",
      "Africa" = "#FFC107",
      "Oceania" = "#9C27B0"
    )
    
    # Generate colors for remaining regions
    missing_regions <- setdiff(regions, names(default_colors))
    if (length(missing_regions) > 0) {
      extra_colors <- colorRampPalette(c("#607D8B", "#795548", "#FF9800", "#CDDC39"))(length(missing_regions))
      names(extra_colors) <- missing_regions
      colors <- c(default_colors, extra_colors)
    } else {
      colors <- default_colors
    }
    
    # Return colors only for available regions
    colors[names(colors) %in% regions]
  }
  
  # Update year range based on available data
  observe({
    data <- carbon_data()
    
    if (nrow(data) > 0) {
      min_year <- min(data$Year, na.rm = TRUE)
      max_year <- max(data$Year, na.rm = TRUE)
      
      # Update slider with actual data range
      updateSliderInput(session, "selected_year",
                        min = min_year,
                        max = max_year,
                        value = max_year,
                        step = 1)
    }
  })
  
  # Display selected year
  output$selected_year_text <- renderText({
    paste("Year:", input$selected_year)
  })
  
  # Generate main chart
  output$co2_plot <- renderPlotly({
    data <- carbon_data()
    
    # Handle case with no data
    if (nrow(data) == 0) {
      return(plot_ly() %>% 
               layout(title = "No data available"))
    }
    
    # Get all unique regions
    all_regions <- unique(data$Region)
    
    # Transform data to wide format for plotting
    data_wide <- data %>%
      pivot_wider(names_from = Region, values_from = Emissions, values_fill = 0)
    
    # Calculate Y-axis maximum based on emissions
    emission_sums <- data %>%
      group_by(Year) %>%
      summarise(total = sum(Emissions, na.rm = TRUE))
    
    max_emissions <- max(emission_sums$total, na.rm = TRUE)
    
    # Round Y-axis maximum to nice values for readability
    y_max <- ceiling(max_emissions / 5000) * 5000
    if (y_max < 30000) y_max <- 30000  # Set minimum scale
    
    # Calculate tick intervals
    y_tick_interval <- y_max / 7
    y_tick_interval <- round(y_tick_interval / 1000) * 1000
    
    # Assign colors to regions
    colors <- get_region_colors(all_regions)
    
    # Create base plotly object
    p <- plot_ly() %>%
      layout(
        title = list(
          text = "Annual CO₂ emissions by world region",
          font = list(size = 24)
        ),
        xaxis = list(
          title = "",
          range = c(min(data_wide$Year), max(data_wide$Year)),
          tickmode = "auto",
          showgrid = TRUE,
          gridcolor = "#E5E5E5"
        ),
        yaxis = list(
          title = list(
            text = "Million tonnes of CO₂",
            font = list(size = 14)
          ),
          range = c(0, y_max),
          tickmode = "linear",
          dtick = y_tick_interval,
          tickformat = ",~f",
          gridcolor = "#E5E5E5"
        ),
        hovermode = "closest",
        showlegend = TRUE,
        legend = list(x = 1.02, y = 0.5),
        margin = list(l = 70, r = 100, b = 50, t = 80)
      )
    
    # Create stacked area chart
    cumulative_values <- rep(0, nrow(data_wide))
    
    # Set transparency
    opacity <- 0.7
    
    # Add each region as a layer
    for (region in all_regions) {
      if (region %in% names(data_wide)) {
        region_values <- data_wide[[region]]
        new_cumulative <- cumulative_values + region_values
        
        # Create hover information
        hover_text <- paste0(
          region, "\n",
          "Year: ", data_wide$Year, "\n",
          "Emissions: ", format(round(region_values, 1), big.mark = ","), " million tonnes"
        )
        
        # Add trace for this region
        p <- p %>% add_trace(
          x = data_wide$Year,
          y = new_cumulative,
          type = 'scatter',
          mode = 'none',
          fill = 'tonexty',
          fillcolor = adjustcolor(colors[region], alpha.f = opacity),
          line = list(color = colors[region], width = 0.5),
          name = region,
          hoverinfo = "text",
          text = hover_text
        )
        
        cumulative_values <- new_cumulative
      }
    }
    
    # Add vertical line for selected year
    p <- p %>% add_trace(
      x = c(input$selected_year, input$selected_year),
      y = c(0, y_max),
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'black', width = 2, dash = 'dot'),
      showlegend = FALSE,
      hoverinfo = "text",
      text = paste("Selected year:", input$selected_year)
    )
    
    return(p)
  })
  
  # Create emissions table
  output$emissions_table <- renderUI({
    data <- carbon_data()
    
    if (nrow(data) == 0) {
      return(HTML("<p>No data available</p>"))
    }
    
    # Filter data for selected year
    year_data <- data %>% 
      filter(Year == input$selected_year) %>%
      arrange(Region)
    
    # Calculate total emissions
    total_emissions <- sum(year_data$Emissions, na.rm = TRUE)
    
    # Create HTML table
    table_html <- '<table style="width:100%; border-collapse: collapse; margin-bottom: 20px; background-color: #f9f9f9;">
      <thead>
        <tr>
          <th style="text-align: left; padding: 8px; border-bottom: 2px solid #ddd;">Region</th>
          <th style="text-align: right; padding: 8px; border-bottom: 2px solid #ddd;">Million tonnes</th>
          <th style="text-align: right; padding: 8px; border-bottom: 2px solid #ddd;">% of global</th>
        </tr>
      </thead>
      <tbody>'
    
    # Add rows for each region
    for (i in 1:nrow(year_data)) {
      region <- year_data$Region[i]
      emissions <- year_data$Emissions[i]
      share <- emissions / total_emissions * 100
      
      table_html <- paste0(table_html, '
        <tr>
          <td style="text-align: left; padding: 8px; border-bottom: 1px solid #eee;">', region, '</td>
          <td style="text-align: right; padding: 8px; border-bottom: 1px solid #eee;">', format(round(emissions, 1), big.mark = ","), '</td>
          <td style="text-align: right; padding: 8px; border-bottom: 1px solid #eee;">', format(round(share, 1), nsmall = 1), '</td>
        </tr>')
    }
    
    # Add total row
    table_html <- paste0(table_html, '
        <tr>
          <td style="text-align: left; padding: 8px; border-bottom: 1px solid #eee;">Total global emissions</td>
          <td style="text-align: right; padding: 8px; border-bottom: 1px solid #eee;">', format(round(total_emissions, 1), big.mark = ","), '</td>
          <td style="text-align: right; padding: 8px; border-bottom: 1px solid #eee;">100.0</td>
        </tr>
      </tbody>
    </table>')
    
    return(HTML(table_html))
  })
  
  # Generate insights text
  output$insight_text <- renderUI({
    data <- carbon_data()
    
    if (nrow(data) == 0) {
      return(HTML("<p>No data available</p>"))
    }
    
    # Filter data for selected year
    year_data <- data %>% 
      filter(Year == input$selected_year) %>%
      arrange(desc(Emissions))
    
    # Calculate total emissions
    total_emissions <- sum(year_data$Emissions, na.rm = TRUE)
    
    # Top emitter information
    top_region <- year_data$Region[1]
    top_emissions <- year_data$Emissions[1]
    top_share <- top_emissions / total_emissions * 100
    
    # Calculate year-over-year change
    change_text <- ""
    if (input$selected_year > min(data$Year)) {
      prev_year <- input$selected_year - 1
      prev_data <- data %>% 
        filter(Year == prev_year) %>%
        summarise(total = sum(Emissions, na.rm = TRUE)) %>%
        pull(total)
      
      if (!is.na(prev_data) && prev_data > 0) {
        change_pct <- (total_emissions - prev_data) / prev_data * 100
        change_dir <- ifelse(change_pct >= 0, "increased", "decreased")
        change_text <- paste0("Global emissions ", change_dir, " by ", 
                              format(abs(round(change_pct, 1)), nsmall = 1), 
                              "% compared to ", prev_year, ".")
      }
    }
    
    # Create text with CO2 subscript
    HTML(paste0(
      "<p>In ", input$selected_year, ", ", top_region, " was the largest emitter, accounting for ", 
      format(round(top_share, 1), nsmall = 1), "% of global CO<sub>2</sub> emissions. ",
      change_text, "</p>"
    ))
  })
}

# Run the Shiny app
shinyApp(ui, ui, server)
