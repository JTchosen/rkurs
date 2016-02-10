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
  subset <- queryfunc(data, "Action", 2000, 2010)
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$text <- renderPrint({str(data)})
})