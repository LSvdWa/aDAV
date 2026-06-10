library(shiny)

# data loading
data <- read.csv("./pokemon.csv")


fluidPage(
  # Title and subtitle
  titlePanel("Interactive Pokemon Data"),
  p("(Generations 1 - 7)"),

  sidebarLayout(
    
    # this is where the inputs are
    sidebarPanel(
      
      # Radar or bar chart
      titlePanel("Stat overview"),
      
      # first pokemon
      selectInput(
        inputId = "pokeName",
        label = "Pokemon name",
        choices = data$name,
        selected = "Beedrill"
      ),
      # second pokemon
      selectInput(
        inputId = "pokeName1",
        label = "Pokemon to compare with",
        choices = data$name,
        selected = "Shuckle"
      ),
      # choose to see radar or bar
      selectInput(
        "graphType",
        "Graph Type",
        choices = c("Radar", "Bar")
      ),
       
      # second diagram
      titlePanel("2D dynamic overview of the dataset"),
      
      # choose stat for x-axis
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
        
      # choose stat for y-axis
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
        ),
        selected = "attack"
      ),
      
      # choose colours based on 1 of three categories
      selectInput(
        "Colour",
        "Colour",
        choices = c(
          "None",
          "generation",
          "type1",
          "type2"
        )
      ),
      
      # choose whether to see a filter
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
          "speed",
          "generation"
        )
      ),
      
      # if a numerical filter is chosen, add the sliderInput
      conditionalPanel(
        condition = "
        input.FilterON == 'weight_kg' ||
        input.FilterON == 'pokedex_number' ||
        input.FilterON == 'hp' ||
        input.FilterON == 'attack' ||
        input.FilterON == 'defense' ||
        input.FilterON == 'sp_attack' ||
        input.FilterON == 'sp_defense' ||
        input.FilterON == 'speed' ||
        input.FilterON == 'generation'
        ",
        
        sliderInput(
          "range", 
          "Range:",
          min = 0, max = 100,
          value = c(10, 90))
      ),
      
      #third diagram
      titlePanel("Comparing regressions for Base_Total"),
      sliderInput("trainSplit", "Proportion of the train set", min = 0.5, max = 0.99, value = 0.8)
      
    ),
    
    # This is where the plots are
    mainPanel(
      plotOutput("comparisonPlot"),
      textOutput("selectedPokeText"),
      plotOutput("scatterPlot"),
      textOutput("regressionText"),
      plotOutput("msePlot"),
      textOutput("mseInterpretation")
    )
  )
)

