if(!require("shiny")){
  install.packages("shiny")
}

library(shiny)
source("functions.R")

ui <- fluidPage(
  
  titlePanel("日本の公的統計"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("stat", "統計：",
                  choices = c("消費者物価指数（総合）",
                              "消費者物価指数（生鮮食品を除く総合）")),
      
      sliderInput("range", "年：",
                  min = 1980, max = lubridate::year(Sys.Date()) |> as.numeric(),
                  value = c(2001, lubridate::year(Sys.Date()) |> as.numeric())
                  
      )
      
    ),
    

  
  
    mainPanel(
      
      plotOutput("plot")
      
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    if(input$stat %in% c("消費者物価指数（総合）", "消費者物価指数（生鮮食品を除く総合）")){
    
      CPI(from = input$range[1], to = input$range[2],
          sogo = ifelse(input$stat == "消費者物価指数（総合）", "総合", "生鮮食品を除く総合"))
      
    }
    
  })
  
}

shinyApp(ui, server)