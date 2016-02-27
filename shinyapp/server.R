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
  queryfunc <- function(data, genre="Alle", anfangsjahr=1893, endjahr=2005, mindauer=1, maxdauer=10000, minrating=1, maxrating=10) {
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
     plot(varjl$year, varjl$length, pch=16, xlab="Jahre", ylab="Filmlaenge", main="Jahre/Filmlaenge", cex=0.6)
     abline(fm, col = "red")
   })
   
  
  output$statLength <- renderTable({
    averages <- c(1:10)
    for(i in 1:10){
      filteredData <- yearFilter(data, decades[i], decades[i]+10)
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
                        input$LengthRange[1], input$LengthRange[2],
                        input$RatingRange[1], input$RatingRange[2])
     fm <- lm(varjr$rating ~ varjr$year)
     plot(varjr$year, varjr$rating, pch=16, xlab="Jahre", ylab="Rating", main="Jahre/Rating", cex=0.6)
     abline(fm, col = "red")
   })
   
  output$statRating <- renderTable({
    averages <- c(1:10)
    for(i in 1:10){
      filteredData <- yearFilter(data, decades[i], decades[i]+10)
      averages[i] <- mean(filteredData$rating)
    }
    df <- data.frame(
      year=decades,
      mean=averages
    )
  })
  #--------------------------------------------------------------
  
   output$ImdbJahrBudget <- renderPlot({
     #data <- subset(data, {input$JahrRange[1]}<=data$year&data$year<={input$JahrRange[2]}&data[[input$Genre]]==1)
     varjr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                        input$LengthRange[1], input$LengthRange[2],
                        input$RatingRange[1], input$RatingRange[2])
     varjr <- subset(varjr, !is.na(varjr$budget))
     plot(varjr$year, varjr$budget, pch=16, xlab="Jahre", ylab="Budget", main="Jahre/Budget", cex=0.6)
   })
  
  output$statBudget <- renderTable({
    averages <- c(1:10)
    for(i in 1:10){
      filteredData <- yearFilter(data, decades[i], decades[i]+10)
      averages[i] <- mean(filteredData$budget, na.rm=TRUE)
    }
    df <- data.frame(
      year=decades,
      mean=averages
    )
  })
   
  #--------------------------------------------------------------
  
#    output$ImdbBudgetRating <- renderPlot({
#      #data <- subset(data, {input$JahrRange[1]}<=data$year&data$year<={input$JahrRange[2]}&data[[input$Genre]]==1)
#      varbr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
#                         input$LengthRange[1], input$LengthRange[2])
#      fm <- lm(varbr$rating ~ varbr$budget)
#      plot(varbr$budget, varbr$rating, xlim=c(0, 50000000))
#      abline(fm, col = "red")
#    })
   
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
  
    counterRating <- rep(0,10)
    for(i in 1:nrow(data)){
      list_rating <- data$rating
      temp_rating <- round(list_rating[i])
      counterRating[temp_rating] <- counterRating[temp_rating]+1
    }
    rel_rating <- rep(0,10)
    for(i in 1:10){
      rel_rating[i] <- counterRating[i]/nrow(data)
    }
    df <- data.frame(
      absfrequency=counterRating,
      relfrequency=rel_rating
    )
  })
  
  #--------------------------------------------------------------
  output$AnalyseCor <- renderText({
    varan <- queryfunc(data, input$Genre)
    output <- c(cor(varan$length, varan$votes), input$Genre, input$JahrRange[1], input$JahrRange[2],
                input$RatingRange[1], input$RatingRange[2])
  })

  output$AnalysePlot <- renderPlot({
    varan <- queryfunc(data, input$Genre)
    y <- varan$rating
    x1 <- varan$length
    x2 <- varan$votes
    z <- lm(y ~ x1 + x2 + x1*x2)
    #print(summary(z))
    par(mfrow=c(2,2))
    plot(z)
  })
  
  output$AnalyseText <- renderTable({
    varan <- queryfunc(data, input$Genre)
    y <- varan$rating
    x1 <- varan$length
    x2 <- varan$votes
    z <- lm(y ~ x1 + x2 + x1*x2)
    sumlm <- summary(z)
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

})