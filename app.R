# 필요한 패키지 로드
library(shiny)
library(tidyverse)
library(plotly)
library(scales)

# UI 정의
ui <- fluidPage(
  # 메인 제목 설정
  titlePanel("Annual CO₂ emissions by world region"),
  
  # 데이터에 대한 설명 텍스트 추가
  p("Emissions from fossil fuels and industry are included, but not land-use change emissions. 
    International aviation and shipping are included as separate entities, as they are not included in any country's emissions."),
  
  # 선택된 연도를 표시하는 텍스트 출력 영역
  h2(textOutput("selected_year_text"), align = "center"),
  
  # 주요 차트 표시 영역 (plotly 사용)
  plotlyOutput("co2_plot", height = "500px"),
  
  # 연도 선택 슬라이더 - 애니메이션 기능 포함
  sliderInput("selected_year", "Select year:", 
              min = 1990, max = 2019,
              value = 2019, 
              step = 1,
              width = "100%",
              animate = animationOptions(interval = 800, loop = FALSE)),
  
  # 데이터 세부 정보 패널 (표와 요약)
  wellPanel(
    h3("Emissions details"),
    uiOutput("emissions_table"),
    htmlOutput("insight_text")
  ),
  
  # 출처 정보
  p(em("Data source: Global Carbon Budget (2024)"), style = "font-size: 12px; color: #666;")
)

# 서버 정의
server <- function(input, output, session) {
  
  # 데이터 로드 및 처리를 위한 반응형 함수
  carbon_data <- reactive({
    # 데이터셋 로드
    data <- read_csv("https://raw.githubusercontent.com/JIHONGKING/Data_Analysis/refs/heads/main/carbon.csv")
    
    # 날짜에서 연도 추출
    data <- data %>% 
      mutate(Year = strtoi(substring(Date, 7, 10)))
    
    # 지역별 CO2 배출량 계산 (킬로톤을 백만톤으로 변환)
    region_totals <- data %>%
      group_by(Region, Year) %>%
      summarise(Emissions = sum(`Kilotons of Co2`, na.rm = TRUE) / 1000) %>%
      ungroup()
    
    return(region_totals)
  })
  
  # 지역별 색상 할당 함수
  get_region_colors <- function(regions) {
    # 기본 색상 팔레트
    default_colors <- c(
      "Asia" = "#FF5252",
      "Europe" = "#2196F3",
      "Americas" = "#4CAF50",
      "Africa" = "#FFC107",
      "Oceania" = "#9C27B0"
    )
    
    # 나머지 지역에 대한 색상 생성
    missing_regions <- setdiff(regions, names(default_colors))
    if (length(missing_regions) > 0) {
      extra_colors <- colorRampPalette(c("#607D8B", "#795548", "#FF9800", "#CDDC39"))(length(missing_regions))
      names(extra_colors) <- missing_regions
      colors <- c(default_colors, extra_colors)
    } else {
      colors <- default_colors
    }
    
    # 사용 가능한 지역에 대한 색상만 반환
    colors[names(colors) %in% regions]
  }
  
  # 사용 가능한 데이터에 따라 연도 범위 업데이트
  observe({
    data <- carbon_data()
    
    if (nrow(data) > 0) {
      min_year <- min(data$Year, na.rm = TRUE)
      max_year <- max(data$Year, na.rm = TRUE)
      
      # 실제 데이터 범위로 슬라이더 업데이트
      updateSliderInput(session, "selected_year",
                        min = min_year,
                        max = max_year,
                        value = max_year,
                        step = 1)
    }
  })
  
  # 선택된 연도 표시
  output$selected_year_text <- renderText({
    paste("Year:", input$selected_year)
  })
  
  # 메인 차트 생성
  output$co2_plot <- renderPlotly({
    data <- carbon_data()
    
    # 데이터가 없는 경우 처리
    if (nrow(data) == 0) {
      return(plot_ly() %>% 
               layout(title = "No data available"))
    }
    
    # 모든 고유 지역 가져오기
    all_regions <- unique(data$Region)
    
    # 플롯을 위해 데이터를 wide 형식으로 변환
    data_wide <- data %>%
      pivot_wider(names_from = Region, values_from = Emissions, values_fill = 0)
    
    # 배출량에 따라 Y축 최대값 계산
    emission_sums <- data %>%
      group_by(Year) %>%
      summarise(total = sum(Emissions, na.rm = TRUE))
    
    max_emissions <- max(emission_sums$total, na.rm = TRUE)
    
    # 가독성을 위해 Y축 최대값을 깔끔한 값으로 반올림
    y_max <- ceiling(max_emissions / 5000) * 5000
    if (y_max < 30000) y_max <- 30000  # 최소 스케일 설정
    
    # 눈금 간격 계산
    y_tick_interval <- y_max / 7
    y_tick_interval <- round(y_tick_interval / 1000) * 1000
    
    # 지역에 색상 할당
    colors <- get_region_colors(all_regions)
    
    # 기본 plotly 객체 생성
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
    
    # 누적 영역 차트 생성
    cumulative_values <- rep(0, nrow(data_wide))
    
    # 투명도 설정
    opacity <- 0.7
    
    # 각 지역을 레이어로 추가
    for (region in all_regions) {
      if (region %in% names(data_wide)) {
        region_values <- data_wide[[region]]
        new_cumulative <- cumulative_values + region_values
        
        # 호버 정보 생성
        hover_text <- paste0(
          region, "\n",
          "Year: ", data_wide$Year, "\n",
          "Emissions: ", format(round(region_values, 1), big.mark = ","), " million tonnes"
        )
        
        # 지역 추적 추가
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
    
    # 선택된 연도에 수직선 추가
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
  
  # 배출량 테이블 생성
  output$emissions_table <- renderUI({
    data <- carbon_data()
    
    if (nrow(data) == 0) {
      return(HTML("<p>No data available</p>"))
    }
    
    # 선택된 연도에 대한 데이터 필터링
    year_data <- data %>% 
      filter(Year == input$selected_year) %>%
      arrange(Region)
    
    # 총 배출량 계산
    total_emissions <- sum(year_data$Emissions, na.rm = TRUE)
    
    # HTML 테이블 생성
    table_html <- '<table style="width:100%; border-collapse: collapse; margin-bottom: 20px; background-color: #f9f9f9;">
      <thead>
        <tr>
          <th style="text-align: left; padding: 8px; border-bottom: 2px solid #ddd;">Region</th>
          <th style="text-align: right; padding: 8px; border-bottom: 2px solid #ddd;">Million tonnes</th>
          <th style="text-align: right; padding: 8px; border-bottom: 2px solid #ddd;">% of global</th>
        </tr>
      </thead>
      <tbody>'
    
    # 각 지역에 대한 행 추가
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
    
    # 총계 행 추가
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
  
  # 통찰력 텍스트 생성
  output$insight_text <- renderUI({
    data <- carbon_data()
    
    if (nrow(data) == 0) {
      return(HTML("<p>No data available</p>"))
    }
    
    # 선택된 연도에 대한 데이터 필터링
    year_data <- data %>% 
      filter(Year == input$selected_year) %>%
      arrange(desc(Emissions))
    
    # 총 배출량 계산
    total_emissions <- sum(year_data$Emissions, na.rm = TRUE)
    
    # 최고 배출자 정보
    top_region <- year_data$Region[1]
    top_emissions <- year_data$Emissions[1]
    top_share <- top_emissions / total_emissions * 100
    
    # 전년 대비 변화 계산
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
    
    # CO2 아래 첨자를 포함한 텍스트 생성
    HTML(paste0(
      "<p>In ", input$selected_year, ", ", top_region, " was the largest emitter, accounting for ", 
      format(round(top_share, 1), nsmall = 1), "% of global CO<sub>2</sub> emissions. ",
      change_text, "</p>"
    ))
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)