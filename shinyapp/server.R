library(shiny)
source("funcs.R")
# Define server logic required to draw a histogram

data <- read.table("movies.tab", sep="\t", header=TRUE, quote="", comment="")
genres <- c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")
data <- factorfunc(data, genres)
decades <- seq(1900, 1990, length.out=10)

shinyServer(function(input, output) {
  
  #Generische Funktion um den Datensatz entsprechend der im UI 
  #festgelegten Kriterien zu fi ltern  
  queryfunc <- function(data, genre, anfangsjahr, endjahr, mindauer=1, maxdauer=10000, minrating=1, maxrating=10) {
    if(genre=="Alle"){
      tmp <- subset(data, anfangsjahr<=data$year&data$year<=endjahr&
                      mindauer<=data$length&data$length<=maxdauer&minrating<=data$rating&data$rating<=maxrating)
    }
    else{  
      tmp <- subset(data, anfangsjahr<=data$year&data$year<=endjahr&data[[genre]]==1&
                      mindauer<=data$length&data$length<=maxdauer&minrating<=data$rating&data$rating<=maxrating)
    }
    return(tmp)
  }
  
  yearFilter <- function(data, anfangsjahr, endjahr){
    tmp <- subset(data, anfangsjahr<=data$year&data$year<=endjahr)
    return (tmp)
  }
  

  #--------------------------------------------------------------
  #Speichert in der Output Variable ImdbJahrLaenge einen Plot 
  #mit X-Achse Jahr und Y-Achse Filmlaenge.
  #Vorher wird der Datensatz aber anhand der im UI festgelegten Kriterien gefiltert
  #Shiny aktualisiert den Datensatz bei jeder Aenderung der Filterkriterien.
   output$ImdbJahrLaenge <- renderPlot({
     #data <- subset(data, {input$JahrRange[1]}<=data$year&data$year<={input$JahrRange[2]}&data[[input$Genre]]==1)
     varjl <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                        input$LengthRange[1], input$LengthRange[2],
                        input$RatingRange[1], input$RatingRange[2])
     fm <- lm(varjl$length ~ varjl$year)
     plot(varjl$year, varjl$length)
     abline(fm, col = "red")
   })
   
  
  output$statLength <- renderTable({

    averages <- c(1:10)
    for(i in 1:10){
      filteredData <- yearFilter(data, decades[0], decades[1])
      averages[i] <- mean(filteredData$length)
    }
    df <- data.frame(
      year=decades,
      mean=averages
    )
  })
  
  #--------------------------------------------------------------
   output$ImdbJahrRating <- renderPlot({
     #data <- subset(data, {input$JahrRange[1]}<=data$year&data$year<={input$JahrRange[2]}&data[[input$Genre]]==1)
     varjr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                        input$LengthRange[1], input$LengthRange[2])
     fm <- lm(varjr$rating ~ varjr$year)
     plot(varjr$year, varjr$rating)
     abline(fm, col = "red")
   })
   
  #--------------------------------------------------------------
  
   output$ImdbJahrBudget <- renderPlot({
     #data <- subset(data, {input$JahrRange[1]}<=data$year&data$year<={input$JahrRange[2]}&data[[input$Genre]]==1)
     varjr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                        input$LengthRange[1], input$LengthRange[2])
     varjr <- subset(varjr, !is.na(varjr$budget))
     plot(varjr$year, varjr$budget)
   })
   
  #--------------------------------------------------------------
  
   output$ImdbBudgetRating <- renderPlot({
     #data <- subset(data, {input$JahrRange[1]}<=data$year&data$year<={input$JahrRange[2]}&data[[input$Genre]]==1)
     varbr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                        input$LengthRange[1], input$LengthRange[2])
     plot(varbr$budget, varbr$rating)
   })
   
  #--------------------------------------------------------------
  
  output$ImdbJahreHaeufigkeit <- renderPlot({
    #data <- subset(data, {input$JahrRange[1]}<=data$year&data$year<={input$JahrRange[2]}&data[[input$Genre]]==1)
    varbr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                       input$LengthRange[1], input$LengthRange[2],
                       input$RatingRange[1], input$RatingRange[2])
    barplot(table(varbr$year))
  })
  
  
  #--------------------------------------------------------------   
   
  output$ImdbRatingHaeufigkeit <- renderTable({
  
    varbr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                       input$LengthRange[1], input$LengthRange[2],
                       input$RatingRange[1], input$RatingRange[2])
    counterRating <- c(1,1,1,1,1,1,1,1,1,1)
    for(i in 1:nrow(data)){
      list_rating <- varbr$rating
      temp_rating <- round(list_rating[i])
      counterRating[temp_rating] <- counterRating[temp_rating]+1
    }
    df <- data.frame(
      frequency=counterRating
    )
  })
  
  #--------------------------------------------------------------
  
#   output$Analyse <- renderTable({
#     model1 <- lm(data$rating ~ data$length + data$votes + data$length*data$votes)
#     correlation <- cor(data$length, data$votes)
#     df <- data.frame(
#       temp=model1
#     )
#   })
  #--------------------------------------------------------------
   
   #Analyse des Datensatzes hinsichtloch Durchschnitt, Median etc.
#    output$textLength <- renderTable({
#      filteredData <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
#                                 input$LengthRange[1], input$LengthRange[2])
#      meanLength <- mean(filteredData[["length"]])
#      medianLength <- median(filteredData[["length"]])
#      length <- c(meanLength, medianLength)
#      
#      meanRating <- mean(filteredData[["rating"]])
#      medianRating <- median(filteredData[["rating"]])
#      rating <- c(meanRating, medianRating)
#      
#      filteredDataBudget <- subset(filteredData, !is.na(filteredData$budget))
#      meanBudget <- mean(filteredDataBudget[["budget"]])
#      medianBudget <- median(filteredDataBudget[["budget"]])
# #      budget <- c(meanBudget, medianBudget)
# 
#      analysisTable <-  matrix(c(meanLength, medianLength, 
#                               meanRating, medianRating,
#                               meanBudget,medianBudget),
#                              ncol=2,byrow=TRUE)
#        
#      analysisTable <- as.table(analysisTable)
# #      colnames(analysisTable) <- c("Mean", "Median")
# #      rownames(analysisTable) <- c("Length", "Rating", "Budget")
#      
#    })
  
  
  
  output$jahre <- renderPrint({input$JahrRange})
  output$genre <- renderPrint({input$Genre})
})