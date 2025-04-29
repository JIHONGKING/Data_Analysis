# ------------------------------------------------------------------
# 0. Load required packages
# ------------------------------------------------------------------
library(shiny)      # ← Comment out if you don’t need the Shiny dashboard
library(tidyverse)
library(plotly)
library(DT)

# ------------------------------------------------------------------
# 1. Load and preprocess CO₂ data
# ------------------------------------------------------------------
co2 <- read_csv(
  "https://raw.githubusercontent.com/JIHONGKING/Data_Analysis/refs/heads/main/carbon.csv",
  show_col_types = FALSE
) |>
  mutate(Year = as.integer(substr(Date, nchar(Date) - 3, nchar(Date))))

co2_sum <- co2 |>
  group_by(Region, Year) |>
  summarise(
    per_capita = mean(`Metric Tons Per Capita`, na.rm = TRUE),
    total_kt   = sum(`Kilotons of Co2`,          na.rm = TRUE),
    .groups = "drop"
  )

regions <- sort(unique(co2_sum$Region))
yr_min  <- min(co2_sum$Year)
yr_max  <- max(co2_sum$Year)

# ------------------------------------------------------------------
# 2. Interactive line chart (no Shiny required)
# ------------------------------------------------------------------
p <- ggplot(co2_sum, aes(Year, per_capita, color = Region)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Average CO₂ Emissions per Capita (1990–2019)",
    x = NULL,
    y = "Metric tons per capita",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggplotly(p, tooltip = c("Year", "per_capita", "Region"))

# ------------------------------------------------------------------
# 3. (Optional) Shiny dashboard with filters and data table
# ------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Global CO₂ Emissions Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yr", "Year range:",
                  min = yr_min, max = yr_max,
                  value = c(yr_min, yr_max), step = 1),
      checkboxGroupInput("reg", "Regions:",
                         choices = regions, selected = regions),
      hr(),
      downloadButton("dl", "Download filtered data")
    ),
    mainPanel(
      plotlyOutput("lineplot", height = "450px"),
      hr(),
      DTOutput("tbl")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filter
  filtered <- reactive({
    co2_sum |>
      filter(Year >= input$yr[1], Year <= input$yr[2],
             Region %in% input$reg)
  })
  
  # Line chart
  output$lineplot <- renderPlotly({
    gg <- ggplot(filtered(), aes(Year, per_capita, color = Region)) +
      geom_line(linewidth = 1) +
      scale_color_brewer(palette = "Set1") +
      labs(x = NULL, y = "t CO₂ per capita") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
    ggplotly(gg, tooltip = c("Year", "per_capita", "Region"))
  })
  
  # Data table
  output$tbl <- renderDT({
    datatable(
      filtered() |> arrange(desc(per_capita)),
      options  = list(pageLength = 15, dom = "tip"),
      rownames = FALSE
    ) |>
      formatRound(c("per_capita", "total_kt"), 2)
  })
  
  # CSV download
  output$dl <- downloadHandler(
    filename = function() sprintf("co2_emissions_%s-%s.csv", input$yr[1], input$yr[2]),
    content  = function(file) write_csv(filtered(), file)
  )
}

# Launch the Shiny app (comment out if not needed)
# shinyApp(ui, server)
