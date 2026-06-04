library(shiny)
library(fmsb)
library(dplyr)

function(input, output, session) {

    data <- read.csv("./pokemon.csv")

    output$radarPlot <- renderPlot({
      stat_df <- data %>%
        filter(name %in% c(input$pokeName, input$pokeName1)) %>%
        select(name, hp, sp_attack, sp_defense, speed, defense, attack)
      
      names <- stat_df$name
      
      stats_df <- stat_df %>%
        select(-name)
      
      max_vals <- c(255, 194, 230, 180, 230, 185)
      min_vals <- rep(0, ncol(stats_df))

      radar_df <- rbind(max_vals, min_vals, stats_df)
      rownames(radar_df) <- c("Max", "Min", names)
      
      radarchart(radar_df, 
                 pcol = c("#00AFBB", "#E7B800"), 
                 pfcol = scales::alpha(c("#00AFBB", "#E7B800"), 0.5), 
                 plwd = 2, 
                 plty = 1,
                 vlabels = c("HP", "Special Attack", "Special Defense", 
                             "Speed", "Defense", "Attack"),
                 vlcex = 0.7,
                 cglcol = "grey", 
                 cglty = 1, 
                 cglwd = 0.8
      )
    })
    
    output$comparisonPlot <- renderPlot({
      
      stat_df <- data %>%
        filter(name %in% c(input$pokeName, input$pokeName1)) %>%
        select(name, hp, sp_attack, sp_defense,
               speed, defense, attack)
      
      names <- stat_df$name
      stats_df <- stat_df %>% select(-name)
      
      if(input$graphType == "Radar") {
        
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
        
      } else if(input$graphType == "Bar") {
        
        par(mfrow = c(1, 2))
        
        barplot(
          as.numeric(stats_df[1, ]),
          horiz = TRUE,
          names.arg = c("HP","Sp Atk","Sp Def",
                        "Speed","Defense","Attack"),
          col = "#00AFBB",
          xlim = c(0,255),
          main = names[1]
        )
        
        barplot(
          as.numeric(stats_df[2, ]),
          horiz = TRUE,
          names.arg = c("HP","Sp Atk","Sp Def",
                        "Speed","Defense","Attack"),
          col = "#E7B800",
          xlim = c(0,255),
          main = names[2]
        )
      }
    })
}
