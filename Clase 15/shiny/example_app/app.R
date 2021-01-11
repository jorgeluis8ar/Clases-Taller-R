library(shiny)

# User interface
ui = fluidPage(
      numericInput(inputId = "n", "Sample size", value = 25),  
      plotOutput(outputId = "hist")
)

# Server
server = function(input, output) {
output$hist = renderPlot({
                          hist(rnorm(n = input$n , mean = 0, sd = 10))
                })
}

# Run the application 
shinyApp(ui = ui, server = server)
