library(shiny)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)


red_wines <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ";")
white_wines <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")

all_wines <- rbind(data.frame(append(red_wines, c(wine.type = 'red'), after = 0)),
                   data.frame(append(white_wines, c(wine.type = 'white'), after = 0)))

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}

ui <- fluidPage(theme = shinytheme("yeti"),
                sidebarPanel(
                  selectInput("display",
                              label = "Graphique",
                              choices = c("histogramme", "boxplot"),
                              selected = "histogramme"
                  ),
                  selectInput("dataset",
                              label = "Jeu de donnee",
                              choices = c("tous", "vins rouges", "vins blancs"),
                              selected = "all wines"
                  ),
                  conditionalPanel(condition = "input.dataset == 'tous'",
                                   checkboxInput("separate", "Separer les vins", TRUE)
                  ),
                  selectInput("parameter",
                              label = "Parametre",
                              choices = names(red_wines),
                              selected = "fixed.acidity"
                  )
                ),
                mainPanel(plotOutput("plot")),
                fluidRow(column(11, align="center", tableOutput("table1"))),
                fluidRow(column(11, align="center", tableOutput("table2")))
)

server <- function(input, output, session) {
  datasetInput <- reactive({
    if (input$dataset == "tous"){
      dataset <- all_wines
    }
    else if (input$dataset == "vins rouges"){
      dataset <- red_wines
    }
    else if (input$dataset == "vins blancs"){
      dataset <- white_wines
    }
    return(dataset)
  })
  
  displayInput <- reactive({
    if (input$display == "histogramme"){
      toplot <- ggplot(datasetInput()) +
        aes_string(x = input$parameter) +
        geom_histogram(color = "black", fill = "grey", bins = 40L) +
        theme_bw() +
        if (input$dataset == "tous" & input$separate == TRUE){
          facet_wrap(~ wine.type, scales = "free")
        }
    }
    else if (input$display == "boxplot"){
      toplot <- ggplot(datasetInput()) +
        aes_string(x = input$parameter) +
        geom_boxplot(color = "black", fill = "grey") +
        theme_bw() +
        if (input$dataset == "tous" & input$separate == TRUE){
          facet_wrap(~ wine.type, scales = "free")
        }
    }
    return(toplot)
  })
  output$plot <- renderPlot(displayInput())
  
  summaryInput <- reactive({
    if (input$dataset == "tous"){
      df <- datasetInput()[-1]
    }else{
      df <- datasetInput()
    }
    summary_df <- data.frame(matrix(NA, nrow = 4, ncol = length(df)))
    rownames(summary_df) <- c("moyenne", "1er quartile", "3eme quartile", "ecart type")
    colnames(summary_df) <- names(df)
    for (column in seq(length(df))){
      summary_df[,column] <- c(mean(df[,column]),
                               quantile(df[,column], .25),
                               quantile(df[,column], .75),
                               sd(df[,column]))
    }
    return (summary_df)
  })
  output$table1 <- renderTable({summaryInput()[0:5]}, rownames = TRUE)
  output$table2 <- renderTable({summaryInput()[6:12]})
}

shinyApp(ui, server, options = list(height = 750, width = 900))