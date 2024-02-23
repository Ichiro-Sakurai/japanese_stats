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
                              "消費者物価指数（生鮮食品を除く総合）",
                              "消費者物価指数（生鮮食品を除く総合、財・サービス）",
                              "需給ギャップ")),
      
      sliderInput("range", "年：",
                  min = 1980, max = lubridate::year(Sys.Date()) |> as.numeric(),
                  value = c(2001, lubridate::year(Sys.Date()) |> as.numeric())
                  
      ),
      
      radioButtons("style", "Style: ",
                   c("Normal" = "normal",
                     "Excel" = "excel",
                     "Economist" = "economist"))
      
    ),
    

  
  
    mainPanel(
      
      plotOutput("plot")
      
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    if(input$stat %in% c("消費者物価指数（総合）", "消費者物価指数（生鮮食品を除く総合）")){
    
      p <- CPI(from = input$range[1], to = input$range[2],
               sogo = ifelse(input$stat == "消費者物価指数（総合）", "総合", "生鮮食品を除く総合"))
      
    } else if (input$stat == "消費者物価指数（生鮮食品を除く総合、財・サービス）") {
      
      p <- CPI(from = input$range[1], to = input$range[2],
               sogo = "生鮮食品を除く総合(財・サービス)")
    
    }　else if (input$stat == "需給ギャップ") {
      
      p <- gap(from = input$range[1], to = input$range[2])
      
    }
    
    if(input$style == "excel") {
      p <- p + theme_excel() + scale_fill_excel()
    } else if (input$style == "economist") {
      p <- p + theme_economist() + scale_fill_economist()
    }
    
    p
    
  })
  
}

shinyApp(ui, server)
