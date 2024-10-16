library(shiny)
library(ggplot2)

# 메니스커스 형태를 계산하는 함수 (이전과 동일)
calculate_meniscus <- function(x, contact_angle, fill_height, width) {
  angle_rad <- contact_angle * pi / 180
  center <- width / 2
  
  R <- abs(center / cos(angle_rad))
  
  if (contact_angle < 90) {
    y <- -(fill_height - (R - sqrt(pmax(0, R^2 - (x - center)^2))))
  } else if (contact_angle > 90) {
    y <- -(fill_height + (R - sqrt(pmax(0, R^2 - (x - center)^2))) - 2 * (R - fill_height))
  } else {
    y <- rep(-fill_height, length(x))
  }
  
  y <- pmax(-fill_height, pmin(y, 0))
  return(y)
}

# 유체량에 따른 접촉각 조정 함수 (이전과 동일)
adjust_contact_angle <- function(contact_angle, fill_height, width) {
  min_height <- 0.1
  if (fill_height <= min_height) {
    return(10)
  }
  max_angle <- asin(min(1, 2 * fill_height / width)) * 180 / pi
  return(min(contact_angle, max_angle))
}

ui <- fluidPage(
  titlePanel("음각 렌즈  메니스커스 시뮬레이션"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("contact_angle", "접촉각 (도)", 
                  min = 10, max = 170, value = 45),
      sliderInput("fill_height", "유체 높이 (μm)", 
                  min = 0.1, max = 10, value = 5, step = 0.1),
      selectInput("air_color", "공기 색상",
                  choices = c("하늘색" = "skyblue", "연보라" = "lavender", "연노랑" = "lightyellow", "연분홍" = "pink"),
                  selected = "skyblue")
    ),
    
    mainPanel(
      plotOutput("structurePlot"),
      textOutput("adjustedAngleText")
    )
  )
)

server <- function(input, output) {
  output$structurePlot <- renderPlot({
    width <- 20
    height <- 10
    
    adjusted_angle <- adjust_contact_angle(input$contact_angle, input$fill_height, width)
    
    x <- seq(0, width, length.out = 200)
    y <- calculate_meniscus(x, adjusted_angle, input$fill_height, width)
    
    ggplot() +
      # 전체 영역을 공기 색으로 채움
      geom_rect(aes(xmin = 0, xmax = width, ymin = -height, ymax = 0), 
                fill = input$air_color) +
      # 외부 구조 (검은색 테두리)
      geom_rect(aes(xmin = 0, xmax = width, ymin = -height, ymax = 0), 
                fill = NA, color = "black", size = 1) +
      # 세로 구분선
      geom_hline(yintercept = c(-height/3, -2*height/3), linetype = "dashed") +
      # 유체 (투명한 흰색)
      geom_ribbon(aes(x = x, ymin = -height, ymax = y), fill = "white", alpha = 0.7) +
      # 메니스커스 선
      geom_line(aes(x = x, y = y), color = "black", size = 1) +
      coord_fixed(ratio = 1) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, width)) +
      scale_y_continuous(expand = c(0, 0), limits = c(-height, 0)) +
      theme_minimal() +
      labs(x = "너비 (μm)", y = "높이 (μm)") +
      ggtitle("음각 렌즈 시뮬레이션")
  })
  
  output$adjustedAngleText <- renderText({
    adjusted_angle <- adjust_contact_angle(input$contact_angle, input$fill_height, 9)
    paste("조정된 접촉각:", round(adjusted_angle, 2), "도")
  })
}

shinyApp(ui = ui, server = server)
