library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(class)
library(ggforce)
library(rsconnect)


# Load data
my_data_white <- read.csv("data/winequality-white.csv", sep = ";")
my_data_white['Label'] <- "W"
my_data_red <- read.csv("data/winequality-red.csv", sep = ";")
my_data_red['Label'] <- "R"

wine <- rbind(my_data_white, my_data_red)

cols <- c('fixed.acidity', 'volatile.acidity', 'citric.acid', 'residual.sugar', 'chlorides', 'free.sulfur.dioxide', 'total.sulfur.dioxide', 'density', 'pH', 'sulphates', 'alcohol')
wine[cols] <- lapply(wine[cols], as.numeric)

wine$quality_lab <- ifelse(wine$quality >= 8, "High",
                           ifelse(wine$quality >= 5, 'Medium', 'Low'))
numeric_features <- cols

# train test split
n_rows <- nrow(wine)

set.seed(42)
train_size <- round(0.80 * n_rows)
train_indices <- sample(seq_len(n_rows), size = train_size)

train_set <- wine[train_indices, ]
test_set <- wine[-train_indices, ]


# ui section
ui <- fluidPage(
  titlePanel("Case Study 3"),
  withMathJax(),
  
  tags$head(
    tags$style(HTML("
      .help-block {
        color: #000000 !important; 
      }
      
      .MathJax, .MathJax_Display {
        color: inherit !important;
      }
    "))
  ),
  
  navset_tab(
    id = "tab", 
    
    nav_panel(
      title = 'Intro', 
      wellPanel(
        h3('Dataset Info.'),
        p('The data set contains chemical information about different wines and their quality scores. It is found on the UCI Machine Learning repository at the following link:'),
        a('Dataset Link', href = "https://archive.ics.uci.edu/dataset/186/wine+quality", target = "_blank"),
        br(),
        p('The data set contains 11 features and the target variable, and there are 6497 rows. Additionally, I added the attributes \'Label\' to denote if the wine is white or red, and \'quality_lab\' to denote if the wine has high, medium, or low quality. The dataset can be explored in the table below.')
      ),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4('Filter Data:'),
          selectInput(
            inputId = 'labFilter',
            label = 'Label:',
            choices = c("All", unique(as.character(wine$Label))),
            selected = "All"
          ),
          selectInput(
            inputId = 'qualFilter',
            label = 'Quality:',
            choices = c("All", unique(as.character(wine$quality))),
            selected = "All"
          )
        ),
        mainPanel(
          width = 8,
          DT::dataTableOutput("table")
        )
      ),
      
      wellPanel(
        h3('Machine Learning Method'),
        p('For this case study I implement K-Nearest Neighbors Classification to predict quality scores of wine.'),
        br(),
        h4('Mathematical Details of KNN Classification:'),
        p('KNN Classification is a classification method which predicts the labels or category that a point falls in based on the training points it is closest to.'),
        p('In KNN classification, the value K denotes the number of neighbors used to determine the prediction value. In this method, nearest neighbors refer to the points which are closest to the point whose label is trying to be predicted. The definition of \'nearest\' is based on a specified measure of distance. In KNN, this distance is often euclidean distance which is defined as the following:'),
        helpText('$$ \\sqrt{\\sum_{i-1}^n (x_i - y_i)^2}$$'),
        helpText('In the context of KNN classification, \\(x_i\\) and \\(y_i\\) refer to the \\(i^{th}\\) entry of two training points.'),
        p('To make a prediction, KNN determines the \'K\' points closest to the point of interest and counts how many points have each possible label. The resulting prediction is the label that the majority of those \'K\' neighbors posseses.'),
        p('In this algorithm, a larger K will lead to a less flexible model that shows broader trends of the data. A smaller K will lead to a very flexible model that shows very local trends of the data.')
      )
    ),
    
    nav_panel(
      title = 'Visualizations',
      
      wellPanel(
        plotOutput("static_class_dist")
      ),
      
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            inputId = "wineType",
            label = "Select Wine Type",
            choices = c("White Wine" = "W", "Red Wine" = "R"),
            selected = c("W", "R")
          )
        ),
        mainPanel(
          plotOutput("distPlot")
        )
      ),
      
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "scatter_x",
            label = "Select x-axis Attribute",
            choices = colnames(wine)
          ),
          selectInput(
            inputId = "scatter_y",
            label = "Select y-axis Attribute",
            choices = colnames(wine)
          ),
          
        ),
        mainPanel(
          plotOutput("scatterPlot")
        )
      )
      
    ),
    
    nav_panel(
      title = 'KNN Classification',
      br(),
      p('In this tab you can visualize KNN classification on the dataset. The label of interest being predicted is \'quality_lab\'. The attribute \'quality_lab\' has three levels: \'Low\' corresponding to wines with quality 3 or 4, \'Medium\' corresponding to wines with quality 5, 6, 7, and \'High\' corresponding to wines with quality 8 or 9. You may select two attributes to base the KNN classification on, these two attributes will be what is used to calculate the distance between points. Additionally, you may select how many neighbors are used to decide the predicted label.'),
      br(),
      
      sidebarLayout(
        sidebarPanel(
          p('Adjust slider to select number of neighbors K'),
          selectInput(
            inputId = "knnx",
            label = "Select x-axis Attribute",
            choices = numeric_features
          ),
          selectInput(
            inputId = "knny",
            label = "Select y-axis Attribute",
            choices = numeric_features
          ),
          sliderInput(
            inputId = 'k_val',
            label = 'Number of Neighbors (K)',
            min = 1,
            max = 21,
            value = 3,
            step = 2
          ),
          
          p('Model Performance: '),
          verbatimTextOutput("accuracy_text")
          
        ),
        
        mainPanel(
          plotOutput("knnPlot")
        )
      )
    )
    
  )
)

# server section

server <- function(input, output, session) {
  
  filtered_data <- reactive ({
    data <- wine
    if (input$labFilter != "All") {
      data <- data |> filter(Label == input$labFilter)
    }
    if (input$qualFilter != "All") {
      data <- data |> filter(quality == input$qualFilter)
    }
    data
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(
      filtered_data(),
      options = list(pageLength = 10, scrollX = TRUE, dom = 't')
    )
  })
  
  output$static_class_dist <- renderPlot({
    
    ggplot(wine, aes(x = Label, fill = Label)) +
      geom_bar() +
      scale_fill_manual(values = c("R" = "darkred", "W" = "wheat")) +
      labs(
        title = "Class Distribution of White vs. Red Wine",
        x = "Wine Type",
        y = "Count"
      ) +
      theme_minimal()
    
  })
  
  output$distPlot <- renderPlot({
    req(input$wineType)
    
    filtered_wine <- wine[wine$Label %in% input$wineType, ]
    
    ggplot(filtered_wine, aes(x = quality, fill = Label)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = c("R" = "darkred", "W" = "wheat")) +
      labs(title = 'Distribution of Wine Quality Scores', x = 'Quality', y = 'Count', fill = "Wine Type") +
      theme_minimal()
    
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(wine, aes(x = .data[[input$scatter_x]], y = .data[[input$scatter_y]])) +
      geom_point() +
      labs(title = 'Scatter Plot Comparing Chosen Attributes')
    
  })
  
  knn_results <- reactive ({
    req(input$knnx, input$knny)
    
    validate(
      need(input$knnx != input$knny, "Must select 2 different features.")
    )
    
    train_x_raw <- as.numeric(train_set[[input$knnx]])
    train_y_raw <- as.numeric(train_set[[input$knny]])
    
    mean_x <- mean(train_x_raw, na.rm = TRUE)
    sd_x   <- sd(train_x_raw, na.rm = TRUE)
    mean_y <- mean(train_y_raw, na.rm = TRUE)
    sd_y   <- sd(train_y_raw, na.rm = TRUE)
    
    train_knn <- data.frame(
      X = (train_x_raw - mean_x) / sd_x,
      Y = (train_y_raw - mean_y) / sd_y,
      quality_lab = as.factor(train_set$quality_lab)
    )
    
    test_knn <- data.frame(
      X = as.numeric((test_set[[input$knnx]] - mean_x) / sd_x),
      Y = as.numeric((test_set[[input$knny]] - mean_y) / sd_y),
      quality_lab = as.factor(test_set$quality_lab)
    )
    
    
    test_predict <- class::knn(
      train = train_knn[, c('X', 'Y')],
      test = test_knn[, c('X', 'Y')],
      cl = train_knn$quality_lab,
      k = input$k_val
    )
    
    test_knn$predicted_qual <- test_predict
    
    cm <- table(Actual = test_knn$quality_lab, Predicted = test_predict)
    accuracy <- sum(diag(cm)) / sum(cm) * 100
    
    list(test_data = test_knn, accuracy = accuracy)
  })
  
  output$accuracy_text <- renderText({
    res <- knn_results()
    paste0("Test Accuracy: ", round(res$accuracy, 2), "%")
  })
  
  output$knnPlot <- renderPlot({
    res <- knn_results()
    
    ggplot(res$test_data, aes(x = X, y = Y)) +
      geom_point(aes(color = quality_lab, shape = predicted_qual), size = 3, alpha = 0.7) +
      scale_shape_manual(values = c('High' = 0, 'Low' = 4, 'Medium' = 19)) +
      labs(
        title = 'KNN Plot for Selected Attributes (Test Set)',
        x = paste(input$knnx, "(Normalized)"),
        y = paste(input$knny, "(Normalized)"),
        shape = 'Predicted Quality',
        color = 'True Quality'
      ) +
      scale_color_manual(values = c('#440154', '#2A788E', '#7AD151')) +
      theme_minimal() +
      theme(legend.position = 'right')
  })
  
  
}

shinyApp(ui = ui, server = server)
