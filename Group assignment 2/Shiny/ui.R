#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Interactive Pokemon Data"),
    p("Gen. 1 - 7"),

    # Sidebar with a slider input for number of bins
    # The snippet from here :
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(
              "gens", 
              "Generations:", 
              selected = c("1", "2", "3", "4", "5", "6", "7"),
              choiceNames = c("1", "2", "3", "4", "5", "6", "7"),
              choiceValues = c(1, 2, 3, 4, 5, 6, 7)
              ),
            sliderInput(
              inputId = "bins",
              label = "Number of bins:",
              min = 1,
              max = 50,
              value = 30
            )
        ),
        mainPanel(
            plotOutput("pokePlot")
        )
    )
)
