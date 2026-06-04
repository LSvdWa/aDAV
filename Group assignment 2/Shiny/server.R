library(shiny)
library(fmsb)
library(dplyr)
library(ggplot2)


function(input, output, session) {

  # data loading
  data <- read.csv("./pokemon.csv")
  
  # the radar chart or two bar charts
  output$comparisonPlot <- renderPlot({
    
    # preprocessing
    stat_df <- data %>%
      filter(name %in% c(input$pokeName, input$pokeName1)) %>%
      select(name, hp, sp_attack, sp_defense,
             speed, defense, attack)
    
    names <- stat_df$name
    stats_df <- stat_df %>% select(-name)
    
    # radar chart
    if(input$graphType == "Radar") {
      
      # define minima and maxima for normalization
      max_vals <- c(255, 194, 230, 180, 230, 185)
      min_vals <- rep(0, ncol(stats_df))
      
      radar_df <- rbind(max_vals, min_vals, stats_df)
      rownames(radar_df) <- c("Max", "Min", names)
      
      radarchart(
        radar_df,
        pcol = c("#00AFBB", "#E7B800"),
        pfcol = scales::alpha(c("#00AFBB", "#E7B800"), 0.5),
        plwd = 2
      )
    } 
    # bar chart
    else {
      
      # make a bar chart for both pokemon
      par(mfrow = c(1, 2))
      
      # bar plot for pokemon 1
      barplot(
        as.numeric(stats_df[1, ]),
        horiz = TRUE,
        names.arg = c("HP","Sp Atk","Sp Def",
                      "Speed","Defense","Attack"),
        col = "#00AFBB",
        xlim = c(0,255),
        main = names[1],
        las = 1
      )
      
      # barplot for pokemon 2
      barplot(
        as.numeric(stats_df[2, ]),
        horiz = TRUE,
        names.arg = c("HP","Sp Atk","Sp Def",
                      "Speed","Defense","Attack"),
        col = "#E7B800",
        xlim = c(0,255),
        main = names[2],
        las = 1
      )
    }
  })
  
  # take the values from the slider (if FilterON and a numeric filter category is selected)
  sliderValues <- reactive({
    data.frame(
      Name = c("Range"),
      Value = as.character(paste(input$range, collapse = " ")),
      stringsAsFactors = FALSE)
  })
  
  # make the filtered dataset for the second plot
  filteredData <- reactive({
    
    df <- data
    
    # exclude legendary and none as they are not numeric and therefore don't need a slider
    if (input$FilterON != "Is legendary" && 
        input$FilterON != "None") {
      
      col <- input$FilterON
      
      df <- df %>% 
        filter(data[[col]] > input$range[1] & 
                            data[[col]] < input$range[2])
  
    }
    
    # makes the filter work for 'Is legendary'
    else if (input$FilterON == "Is legendary") {
      df <- df[df$is_legendary == 1, ]
    }
    
    # returns the filtered dataset
    df
  })
  
  # observes activity of the filter and updates the slider range for the selected category
  observeEvent(input$FilterON, {
    
    if (input$FilterON %in% c(
      "weight_kg",
      "pokedex_number",
      "hp",
      "attack",
      "defense",
      "sp_attack",
      "sp_defense",
      "speed"
      )) {
      
      vals <- data[[input$FilterON]]
      
      updateSliderInput(
        session,
        "range",
        min = floor(min(vals, na.rm = TRUE)),
        max = ceiling(max(vals, na.rm = TRUE)),
        value = c(
          floor(min(vals, na.rm = TRUE)),
          ceiling(max(vals, na.rm = TRUE))
        )
      )
    }
  })
  
  # this makes the final second plot
  output$scatterPlot <- renderPlot({
    
    df <- filteredData()
    if (input$Colour != "None") {
      var_colour <- df[[input$Colour]]
    }
    else {
      var_colour <- c("Select a colour")
    }
    
    ggplot(data=df,
           aes_string(x = input$xStat,
                      y = input$yStat,
                      colour = as.factor(var_colour))) +
      geom_point(size = 3, alpha = 0.7) +
      labs(title = paste(input$xStat, 
                         "vs", 
                         input$yStat),
           x = input$xStat,
           y = input$yStat,
           colour = input$Colour) +
      geom_smooth(method="lm", aes(group=1), colour="black") +
      theme_minimal()
  })
}
