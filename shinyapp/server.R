library(shiny)
source("funcs.R")


#Import Data, Define Possible Genres and transform the genres to Factors.
data <- read.table("movies.tab", sep="\t", header=TRUE, quote="", comment="",fileEncoding="UTF-8")
genres <- c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")
data <- factorfunc(data, genres)
decades <- seq(1900, 1990, length.out=10)
#Setzt das WD in den Überordner von "shinyapp", also in den Ordner Projekt

shinyServer(function(input, output) {
  
  #Generische Funktion um den Datensatz entsprechend der im UI 
  #festgelegten Kriterien zu filtern  
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
  
  #Wird genutzt um die Daten in Jahrzehnte einzuteilen
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
     varjl <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                        input$LengthRange[1], input$LengthRange[2],
                        input$RatingRange[1], input$RatingRange[2])
     fm <- lm(varjl$length ~ varjl$year)
     plot(varjl$year, varjl$length, pch=16, xlab="Jahre", ylab="Filmlänge", main="Jahre/Filmlänge", cex=0.6)
     abline(fm, col = "red")
   })
   
  #Berechnet die durchschnittliche Filmlänge je Jahrzehnt, nicht abhängig von Input sondern gibt einen allgemeinen Überblick
  output$statLength <- renderTable({
    averages <- c(1:10)
    for(i in 1:10){
      #Filtert das entsprechende Jahrzehnt und berechnet den Durchschnitt der Filmlänge
      filteredData <- yearFilter(data, decades[i], decades[i]+10)
      averages[i] <- mean(filteredData$length)
    }
    df <- data.frame(
      Dekade=decades,
      Durchschnitt=averages
    )
  })
  
  #--------------------------------------------------------------
  #Stellt den Verlauf der Ratings nach Jahren dar. Auch hier wird der Graph an die Inputwertde der ui datei angepasst
   output$ImdbJahrRating <- renderPlot({
     varjr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                        input$LengthRange[1], input$LengthRange[2],
                        input$RatingRange[1], input$RatingRange[2])
     fm <- lm(varjr$rating ~ varjr$year)
     plot(varjr$year, varjr$rating, pch=16, xlab="Jahre", ylab="Rating", main="Jahre/Rating", cex=0.6)
     abline(fm, col = "red")
   })
   #Gleiches Vorgehen wie bei der Filmlänge
  output$statRating <- renderTable({
    averages <- c(1:10)
    for(i in 1:10){
      filteredData <- yearFilter(data, decades[i], decades[i]+10)
      averages[i] <- mean(filteredData$rating)
    }
    df <- data.frame(
      Dekade=decades,
      Durchschnitt=averages
    )
  })
  #--------------------------------------------------------------
  #Stellt den Verlauf der Budgets über die Jahr dar. Viele Titel haben nur eine Angabe NA, deshalb diese rausfiltern
   output$ImdbJahrBudget <- renderPlot({
     varjr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                        input$LengthRange[1], input$LengthRange[2],
                        input$RatingRange[1], input$RatingRange[2])
     varjr <- subset(varjr, !is.na(varjr$budget))
     plot(varjr$year, varjr$budget, pch=16, xlab="Jahre", ylab="Budget", main="Jahre/Budget", cex=0.6)
   })
  
  #Gleiches Vorgehen wie bisher. Auch hier die NA Werte herausfiltern
  output$statBudget <- renderTable({
    averages <- c(1:10)
    for(i in 1:10){
      filteredData <- yearFilter(data, decades[i], decades[i]+10)
      averages[i] <- mean(filteredData$budget, na.rm=TRUE)
    }
    df <- data.frame(
      Dekade=decades,
      Durchschnitt=averages
    )
  })
   
  #--------------------------------------------------------------
  #Darstellung der Filme auf die Jahre verteilt. 
  output$ImdbJahreHaeufigkeit <- renderPlot({
    varbr <- queryfunc(data, input$Genre, input$JahrRange[1], input$JahrRange[2],
                       input$LengthRange[1], input$LengthRange[2],
                       input$RatingRange[1], input$RatingRange[2])
    barplot(table(varbr$year), xlab="Jahre", ylab="Filmanzahl", main="Jahre/Filmanzahl")
  })
  #Mit yearFilter werden die Filme pro Dekade gefiltert. nrow(filteredData) bestimmt die Zeilen der 1:10 Subsets.
  #Und ermittelt somit die Zahl der Filme pro Dekade
  output$statFilmanzahl <- renderTable({
    counter_decade <- rep(0,10)
    for(i in 1:10){
      filteredData <- yearFilter(data, decades[i], decades[i]+10)
      counter_decade[i] <- nrow(filteredData)
    }
    
    df <- data.frame(
      Dekade=decades,
      Summe=counter_decade
    )
  })
  #--------------------------------------------------------------   
  #Es wird ermittelt, wie sich Filme mit >1000 Votes auf die Bewertungen verteilen
  output$ImdbRatingHaeufigkeit <- renderTable({
  
    counterRating <- rep(0,10)
    data_votesok <- subset(data, data$votes>=1000)
    list_rating <- data_votesok$rating
    for(i in 1:nrow(data)){
      
      temp_rating <- round(list_rating[i])
      counterRating[temp_rating] <- counterRating[temp_rating]+1
    }
    rel_rating <- rep(0,10)
    for(i in 1:10){
      rel_rating[i] <- counterRating[i]/nrow(data)
    }
    df <- data.frame(
      Absolute_Häufigkeit=counterRating,
      Relative_Häufigkeit=rel_rating
    )
  })
  
  #--------------------------------------------------------------
  #Bildung des linearen Modells, zum einen der Plot und zum anderen die Tabelle, welche Standardfehler etc. darstellt
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


})