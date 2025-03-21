ui <- fluidPage(
  
  
  titlePanel("Kilotons of Co2 per Region"),
  
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("Year",
                  "Year",
                  min = 1990,
                  max = 2019,
                  value = c(1990,2019),
                  sep = ""),
      checkboxGroupInput('Region','Region',c('Africa','Americas','Asia','Europe','Oceania'),
                         selected = c('Africa','Americas','Asia','Europe','Oceania'))
    ),
    
    
    mainPanel(
      plotOutput("heat"),
      tableOutput('table')
    )
  )
)


server <- function(input, output) {
  
  data = read_csv("https://raw.githubusercontent.com/JIHONGKING/Data_Analysis/refs/heads/main/carbon.csv") %>% 
    mutate(Year = strtoi(substring(Date, 7,10)))
  
  continent = data %>% 
    group_by(Region, Year) %>% 
    summarise(`Kilotons of CO2` = sum(`Kilotons of Co2`))
  
  outdata = reactive(continent %>% 
                       filter(Year >= input$Year[1], Year <= input$Year[2], Region %in% input$Region))
  
  output$heat <- renderPlot({
    
    
    ggplot(outdata())+
      geom_tile(aes(Year,Region, fill = `Kilotons of CO2`, col= `Kilotons of CO2`))+
      scale_color_gradient2()+
      scale_fill_gradient2()+
      theme_minimal()+
      theme(axis.text = element_text(color = 'black', size = 12), axis.title = element_text(size = 12))
  })
  
  output$table = renderTable({outdata()})
}


shinyApp(ui, server)