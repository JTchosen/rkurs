library(shiny)
source("funcs.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  data <- read.table("movies.tab", sep="\t", header=TRUE, quote="", comment="")
  genres <- c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")
  data <- factorfunc(data, genres)
  #subset <- queryfunc(data, "Action", {input$rangeSlider[1]}, {input$rangeSlider[2]})
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
   output$ImdbPlot <- renderPlot({
     #data <- queryfunc(data, input$Genre, {input$JahrRange[1]}, {input$JahrRange[2]})
     data <- subset(data, {input$JahrRange[1]}<=data$year&data$year<={input$JahrRange[2]}&data[[input$Genre]]==1)
     plot(data$year, data$length)
   })
  
  output$jahre <- renderPrint({input$JahrRange})
  output$genre <- renderPrint({input$Genre})
  output$test <- renderPrint({str(data)})
})