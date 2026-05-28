library(shiny)

data <- read.csv("./pokemon.csv")


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
            ),
            selectInput(
              inputId = "pokeName",
              label = "Name of pokemon",
              choices = data$name,
              selected = ""
            )
        ),
        mainPanel(
            plotOutput("pokePlot"),
            textOutput("pokemonName")
        )
    )
)
