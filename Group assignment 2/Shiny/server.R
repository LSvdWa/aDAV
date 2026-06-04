library(shiny)
library(fmsb)
library(dplyr)
library(ggplot2)


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
          main = names[1],
          las = 1
        )
        
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
    
    #regression plot Attack vs HP
    output$statRegressionPlot <- renderPlot({
      
      stat_df <- data %>% 
        filter(name %in% c(input$pokeName, input$pokeName1))
      
      model_lin <- lm(attack ~ hp, data = stat_df)
      model_quad <- lm(attack ~ hp + I(hp^2), data = stat_df)
      model_cubic <- lm(attack ~ hp + I(hp^2) + I(hp^3), data = stat_df)
      
      hp_pred <- seq(min(stat_df$hp), max(stat_df$hp), length.out = 100)
      pred_df <- data.frame(hp = hp_pred)
      pred_df$attack_lin <- predict(model_lin, newdata = pred_df)
      pred_df$attack_quad <- predict(model_quad, newdata = pred_df)
      pred_df$attack_cubic <- predict(model_cubic, newdata = pred_df)
      
      plot(stat_df$hp, stat_df$attack,
           pch = 19, col = c("#00AFBB", "#E7B800"),
           xlab = "HP", ylab = "Attack",
           main = "Attack vs HP with Regression Lines")
      
      lines(pred_df$hp, pred_df$attack_lin, col = "blue", lwd = 2)
      lines(pred_df$hp, pred_df$attack_quad, col = "red", lwd = 2)
      lines(pred_df$hp, pred_df$attack_cubic, col = "green", lwd = 2)
      
      legend("topright", legend = c("Linear","Quadratic","Cubic"),
             col = c("blue","red","green"), lwd = 2)
  
    filteredData <- reactive({
      
      df <- data
      
      if (input$FilterON != "Is legendary" && input$FilterON != "None") {
        
        col <- input$FilterON
        
        if (input$filterDirection == "Higher than") {
          df <- df %>% filter(data[[col]] > input$filterValue)
        } else if (input$filterDirection == "Lower than") {
          df <- df %>% filter(data[[col]] < input$filterValue)
        }
        
      }
      else if (input$FilterON == "Is legendary") {
        df <- df[df$is_legendary == 1, ]
      }
      df
    })
    output$scatterPlot <- renderPlot({
      
      df <- filteredData()
      if (input$Colour != "None") {
        var_colour <- df[[input$Colour]]
      }
      else {
        var_colour <- c("Select a colour")
      }
      ggplot(
        df,
        aes_string(
          x = input$xStat,
          y = input$yStat,
          colour = as.factor(var_colour)
        )
      ) +
        geom_point(size = 3, alpha = 0.7) +
        labs(
          title = paste(input$xStat, "vs", input$yStat),
          x = input$xStat,
          y = input$yStat,
          colour = input$Colour
        ) +
        theme_minimal()
      
    })
  })
}
