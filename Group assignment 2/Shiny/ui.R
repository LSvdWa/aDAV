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
           titlePanel("First graph"),
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
            ),
           titlePanel("Second graph"),
           selectInput(
              "xStat",
              "X Axis",
              choices = c(
                "hp",
                "attack",
                "defense",
                "sp_attack",
                "sp_defense",
                "speed"
              )
            ),
            
            selectInput(
              "yStat",
              "Y Axis",
              choices = c(
                "hp",
                "attack",
                "defense",
                "sp_attack",
                "sp_defense",
                "speed"
              )
            ),
            selectInput(
              "Colour",
              "Colour",
              choices = c(
                "None",
                "generation",
                "classification",
                "type1",
                "type2",
                "abilities"
              )
            ),
            
            selectInput(
              "FilterON",
              "Filter",
              choices = c(
                "None",
                "Is legendary",
                "weight_kg",
                "pokedex_number",
                "hp",
                "attack",
                "defense",
                "sp_attack",
                "sp_defense",
                "speed"
              )
            ),
            conditionalPanel(
              condition = "
              input.FilterON == 'weight_kg' ||
              input.FilterON == 'pokedex_number' ||
              input.FilterON == 'hp' ||
              input.FilterON == 'attack' ||
              input.FilterON == 'defense' ||
              input.FilterON == 'sp_attack' ||
              input.FilterON == 'sp_defense' ||
              input.FilterON == 'speed'
              ",
              
              selectInput(
                "filterDirection",
                "Higher or Lower",
                choices = c(
                  "Higher than",
                  "Lower than" 
                )
              ),
              
              numericInput(
                "filterValue",
                "Filter Value",
                value = 0,
                min = 0
              )
            )
            
            
            
        ),
        mainPanel(
          plotOutput("comparisonPlot"),
          plotOutput("scatterPlot")
        )
    )
)
