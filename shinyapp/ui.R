library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Projekt R-Kurs"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(

      sliderInput("JahrRange", label = "Zeitspanne:",
                  min = 1900, max = 2005, value = c(1990,2000)),
      
      selectInput("Genre", label = "Genre:", 
                  choices = list("Action" = "Action", 
                                 "Animation" = "Animation", "Comedy"= "Comedy",
                                 "Drama" = "Drama", "Documentary" = "Documentary", 
                                 "Romance" = "Romance", "Short" = "Short"
                                   )),
      
      sliderInput("LengthRange", label = "Filmdauer:",
                  min = 1, max = 300, value = c(80,120))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Jahr/Laenge", plotOutput("ImdbJahrLaenge"), "Durchschnittslaenge
                 und Median der Laenge sind:"), 
        tabPanel("Jahr/Rating", plotOutput("ImdbJahrRating")),
        tabPanel("Jahr/Budget", plotOutput("ImdbJahrBudget")),
        tabPanel("Budget/Rating", plotOutput("ImdbBudgetRating")),
        tabPanel("Length/Rating", plotOutput("ImdbLengthRating")),
        tabPanel("Analyse", textOutput("budget"))
      ),
      tags$hr(),
      tableOutput("textInfo")
      #textOutput("budget")
      #textOutput("genre"),
    )
    
  )
))