library(shiny)
library(ggplot2)

# 메니스커스 형태를 계산하는 함수
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

# 스넬의 법칙을 사용한 굴절각 계산 함수
calculate_refraction <- function(angle_in, n1, n2) {
  sin_out <- (n1 / n2) * sin(angle_in)
  if (abs(sin_out) > 1) {
    return(NA)  # 전반사
  }
  return(asin(sin_out))
}

ui <- fluidPage(
  titlePanel("음각 렌즈 메니스커스 광선 굴절 시뮬레이션"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("contact_angle", "접촉각 (도)", 
                  min = 10, max = 170, value = 27),
      sliderInput("fill_height", "유체 높이 (μm)", 
                  min = 0.1, max = 10, value = 7.5, step = 0.1),
      numericInput("n1", "하부막 굴절률:", 1.5, min = 1.0, max = 2.0, step = 0.01),
      numericInput("n2", "상부막 굴절률:", 1.7, min = 1.0, max = 2.0, step = 0.01)
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
    
    x <- seq(0, width, length.out = 400)
    y <- calculate_meniscus(x, input$contact_angle, input$fill_height, width)
    
    # 광선 시뮬레이션
    n_rays <- 21
    ray_x_start <- seq(0, width, length.out = n_rays)
    ray_y_start <- rep(-10, n_rays)
    
    ray_x_end <- numeric(n_rays)
    ray_y_end <- numeric(n_rays)
    refracted_x <- numeric(n_rays)
    refracted_y <- numeric(n_rays)
    
    for (i in 1:n_rays) {
      # 메니스커스와의 교차점 찾기
      intersection <- approx(x, y, xout = ray_x_start[i])$y
      ray_x_end[i] <- ray_x_start[i]
      ray_y_end[i] <- intersection
      
      # 법선 벡터 계산
      idx <- which.min(abs(x - ray_x_start[i]))
      if (idx > 1 && idx < length(x)) {
        dx <- x[idx + 1] - x[idx - 1]
        dy <- y[idx + 1] - y[idx - 1]
        normal_angle <- atan2(dy, dx) + pi/2  # 법선 각도
        
        # 입사각 계산
        incident_angle <- abs(normal_angle - pi/2)
        
        # 굴절각 계산
        refracted_angle <- calculate_refraction(incident_angle, input$n1, input$n2)
        
        if (!is.na(refracted_angle)) {
          # 굴절된 광선
          ray_angle <- normal_angle - refracted_angle
          refracted_x[i] <- ray_x_end[i] + cos(ray_angle) * height
          refracted_y[i] <- ray_y_end[i] + sin(ray_angle) * height
        } else {
          # 전반사
          refracted_x[i] <- ray_x_end[i]
          refracted_y[i] <- ray_y_end[i]
        }
      } else {
        # 경계 케이스 처리
        refracted_x[i] <- ray_x_end[i]
        refracted_y[i] <- 0
      }
    }

    # x > 10인 경우의 굴절광만 선택하고 대칭 처리
    valid_rays <- which(ray_x_end > 10)
    ray_x_end_valid <- ray_x_end[valid_rays]
    ray_y_end_valid <- ray_y_end[valid_rays]
    refracted_x_valid <- refracted_x[valid_rays]
    refracted_y_valid <- refracted_y[valid_rays]
    

    
    # 대칭 처리
    ray_x_end_symmetric <- c(ray_x_end_valid, 20 - rev(ray_x_end_valid))
    ray_y_end_symmetric <- c(ray_y_end_valid, rev(ray_y_end_valid))
    refracted_x_symmetric <- c(refracted_x_valid, 20 - rev(refracted_x_valid))
    refracted_y_symmetric <- c(refracted_y_valid, rev(refracted_y_valid))
    
    ggplot() +
      # 외부 구조
      geom_rect(aes(xmin = 0, xmax = width, ymin = -10, ymax = 0), 
                fill = NA, color = "black", size = 1) +
      # 유체
      geom_ribbon(aes(x = x, ymin = -10, ymax = y), fill = "lightblue", alpha = 0.5) +
      # 메니스커스 선
      geom_line(aes(x = x, y = y), color = "blue", size = 1) +
      # 입사 광선 (모든 광선 표시)
      geom_segment(aes(x = ray_x_start, y = ray_y_start, xend = ray_x_end, yend = ray_y_end),
                   color = "red", arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed")) +
      # 굴절된 광선 (대칭 처리된 광선만 표시)
      geom_segment(aes(x = ray_x_end_symmetric, y = ray_y_end_symmetric, 
                       xend = refracted_x_symmetric, yend = refracted_y_symmetric),
                   color = "green", arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed")) +
      coord_fixed(ratio = 1) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, width)) +
      scale_y_continuous(expand = c(0, 0), limits = c(-10, 5)) +
      theme_minimal() +
      labs(x = "너비 (μm)", y = "높이 (μm)") +
      ggtitle("음각렌즈 ray 시뮬레이션")
  })
  
  output$adjustedAngleText <- renderText({
    paste("메니스커스 접촉각:", round(input$contact_angle, 2), "도")
  })
}
shinyApp(ui = ui, server = server)
