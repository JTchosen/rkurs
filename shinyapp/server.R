library(shiny)
source("funcs.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  data <- read.table("movies.tab", sep="\t", header=TRUE, quote="", comment="")
  genres <- c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")
  data <- factorfunc(data, genres)

  #Generische Funktion um den Datensatz entsprechend der im UI 
  #festgelegten Kriterien zu filtern  
  queryfunc <- function(data, genre, anfangsjahr, endjahr, mindauer, maxdauer) {
    tmp <- subset(data, anfangsjahr<=data$year&data$year<=endjahr&data[[genre]]==1&
                    mindauer<=data$length&data$length<=maxdauer)
    return(tmp)
  }

  #Speichert in der Output Variable ImdbJahrLaenge einen Plot 
  #mit X-Achse Jahr und Y-Achse Filmlaenge.
  #Vorher wird der Datensatz aber anhand der im UI festgelegten Kriterien gefiltert
  #Shiny aktualisiert den Datensatz bei jeder Aenderung der Filterkriterien.
   output$ImdbJahrLaenge <- renderPlot({
     #data <- subset(data, {input$JahrRange[1]}<=data$year&data$year<={input$JahrRange[2]}&data[[input$Genre]]==1)
     varjl <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                        input$LengthRange[1], input$LengthRange[2])
     plot(varjl$year, varjl$length)
   })
   
   output$ImdbJahrRating <- renderPlot({
     #data <- subset(data, {input$JahrRange[1]}<=data$year&data$year<={input$JahrRange[2]}&data[[input$Genre]]==1)
     varjr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                        input$LengthRange[1], input$LengthRange[2])
     plot(varjr$year, varjr$rating)
   })
   
   output$ImdbJahrBudget <- renderPlot({
     #data <- subset(data, {input$JahrRange[1]}<=data$year&data$year<={input$JahrRange[2]}&data[[input$Genre]]==1)
     varjr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                        input$LengthRange[1], input$LengthRange[2])
     varjr <- subset(varjr, !is.na(varjr$budget))
     plot(varjr$year, varjr$budget)
   })
   
   output$ImdbBudgetRating <- renderPlot({
     #data <- subset(data, {input$JahrRange[1]}<=data$year&data$year<={input$JahrRange[2]}&data[[input$Genre]]==1)
     varbr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                        input$LengthRange[1], input$LengthRange[2])
     plot(varbr$budget, varbr$rating)
   })
   
   output$ImdbLengthRating <- renderPlot({
     #data <- subset(data, {input$JahrRange[1]}<=data$year&data$year<={input$JahrRange[2]}&data[[input$Genre]]==1)
     varlr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                        input$LengthRange[1], input$LengthRange[2])
     plot(varlr$length, varlr$rating)
   })
   
   #Analyse des Datensatzes hinsichtloch Durchschnitt, Median etc.
   output$textInfo <- renderTable({
     filteredData <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                                input$LengthRange[1], input$LengthRange[2])
     meanLength <- mean(filteredData[["length"]])
     medianLength <- median(filteredData[["length"]])
     length <- c(meanLength, medianLength)
     
     meanRating <- mean(filteredData[["rating"]])
     medianRating <- median(filteredData[["rating"]])
     rating <- c(meanRating, medianRating)
     
     filteredDataBudget <- subset(filteredData, !is.na(filteredData$budget))
     meanBudget <- mean(filteredDataBudget[["budget"]])
     medianBudget <- median(filteredDataBudget[["budget"]])
#      budget <- c(meanBudget, medianBudget)

     analysisTable <-  matrix(c(meanLength, medianLength, 
                              meanRating, medianRating,
                              meanBudget,medianBudget),
                             ncol=2,byrow=TRUE)
       
     analysisTable <- as.table(analysisTable)
#      colnames(analysisTable) <- c("Mean", "Median")
#      rownames(analysisTable) <- c("Length", "Rating", "Budget")
     
   })
  
  output$jahre <- renderPrint({input$JahrRange})
  output$genre <- renderPrint({input$Genre})
})