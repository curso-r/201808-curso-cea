library(shiny)

ui <- fluidPage(
  
  titlePanel("Meu primeiro shiny app!"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(
        inputId = "obs",
        label = "Número de observações:",
        min = 1,
        max = 1000,
        value = 555
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "hist"),
      textOutput(outputId = "text")
    )
    
  )
)

server <- function(input, output) {
  
  n_2 <- reactive({
    
    input$obs*2
    
  })
  
  output$hist <- renderPlot({
    
    hist(
      rnorm(input$obs), 
      col = "#75AADB", 
      border = "white",
      xlab = "Distribuição normal",
      main = sprintf(
        "Histograma da distribuição normal com %s observações", input$obs
      )
    )
    
  })
  
  output$text <- renderText({
    paste("Oi!", n_2())
  })
  
}

shinyApp(ui=ui, server = server)

