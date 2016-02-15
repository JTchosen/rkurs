library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Projekt R-Kurs"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(

      sliderInput("JahrRange", label = "Range:",
                  min = 1900, max = 2005, value = c(1990,2000)),
      
      selectInput("Genre", label = "Genre:", 
                  choices = list("Action" = "Action", 
                                 "Animation" = "Animation", "Comedy"= "Comedy",
                                 "Drama" = "Drama", "Documentary" = "Documentary", 
                                 "Romance" = "Romance", "Short" = "Short"
                                   ))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("ImdbPlot"),
      textOutput("genre"),
      textOutput("jahre"),
      textOutput("test")
    )
    
  )
))