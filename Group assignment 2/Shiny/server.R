library(shiny)
library(fmsb)
library(dplyr)
library(ggplot2)
library(glmnet)


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
  
  #reactive text of selected pokemons and their total stats
  output$selectedPokeText <- renderText({
    stat_df <- data %>%
      filter(name %in% c(input$pokeName, input$pokeName1)) %>%
      select(name, base_total)
    
    poke1 <- stat_df[1, ]
    poke2 <- stat_df[2, ]
    
    paste0("Selected Pokémon: ", poke1$name, " and ", poke2$name, ". ",
           poke1$name, " total stats = ", poke1$base_total, ", ",
           poke2$name, " total stats = ", poke2$base_total, 
           ". Difference = ", abs(poke1$base_total - poke2$base_total))
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
  
  #interactive text about regression line
  output$regressionText <- renderText({
    df <- filteredData()
    
    if(nrow(df) < 2) return("Not enough data to compute regression.")
    
    model <- lm(as.formula(paste(input$yStat, "~", input$xStat)), data=df)
    slope <- round(coef(model)[2], 2)
    intercept <- round(coef(model)[1], 2)
    
    paste0("Regression line for ", input$yStat, " vs ", input$xStat, 
           ": y = ", intercept, " + ", slope, " * x")
  })
  
  #this is the LASSO vs Ridge chart + interactive text
  
  lasso_ridge_results <- reactive({
    
    filtered <- data %>%
      select(base_total, base_egg_steps, base_happiness, capture_rate, classfication,
             experience_growth, height_m, is_legendary, type1, type2, weight_kg, generation)
    
    filtered <- na.omit(filtered)
    
    split_idx <- sample(1:nrow(filtered), input$trainSplit * nrow(filtered))
    x_train <- model.matrix(base_total ~ ., data=filtered)[split_idx, ]
    x_test  <- model.matrix(base_total ~ ., data=filtered)[-split_idx, ]
    y_train <- filtered$base_total[split_idx]
    y_test  <- filtered$base_total[-split_idx]
    
    # Cross-validated LASSO
    cv_lasso <- cv.glmnet(x_train, y_train, alpha=1)
    y_pred_lasso <- predict(cv_lasso, newx = x_test, s = "lambda.min")
    
    # Cross-validated Ridge
    cv_ridge <- cv.glmnet(x_train, y_train, alpha=0)
    y_pred_ridge <- predict(cv_ridge, newx = x_test, s = "lambda.min")
    
    mse <- function(actual, predicted) mean((actual - predicted)^2)
    mses <- c(LASSO = mse(y_test, y_pred_lasso),
              Ridge = mse(y_test, y_pred_ridge))
    
    list(cv_lasso=cv_lasso, cv_ridge=cv_ridge, mses=mses)
  })
  
  # Plot MSE bar chart
  output$msePlot <- renderPlot({
    res <- lasso_ridge_results()
    barplot(res$mses, col = c("#00AFBB","#E7B800"),
            main="Test Set MSE: LASSO vs Ridge", ylab="MSE")
  })
  
  # Reactive interpretation
  output$mseInterpretation <- renderText({
    res <- lasso_ridge_results()
    paste0("With a train-test split of ", round(input$trainSplit*100), "% train data: ",
           "Test MSE: LASSO = ", round(res$mses["LASSO"],1),
           ", Ridge = ", round(res$mses["Ridge"],1), 
           ". This section compares LASSO and Ridge regression performance on predicting total base stats. Adjust the train-test split to see the effect on test MSE.")
  })
}
