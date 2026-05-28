library(shiny)

function(input, output, session) {

    data <- read.csv("./pokemon.csv")

  
    
    output$pokePlot <- renderPlot({
      data <- data[as.numeric(data$generation) %in% input$gens, ]
      hist(x=data$base_total, xlab="Base Total Stats", main="Histogram of Base Total per Generation")
    })

}
