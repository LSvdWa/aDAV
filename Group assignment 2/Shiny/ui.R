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
            selectInput(
              inputId = "pokeName",
              label = "Pokemon name",
              choices = data$name,
              selected = "Beedrill"
            ),
            selectInput(
              inputId = "pokeName1",
              label = "Pokemon to compare with",
              choices = data$name,
              selected = "Shuckle"
            ),
            selectInput(
              "graphType",
              "Graph Type",
              choices = c("Radar", "Bar")
            )
            
        ),
        mainPanel(
          plotOutput("comparisonPlot")
        )
    )
)
