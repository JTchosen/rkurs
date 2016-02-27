library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Projekt R-Kurs"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(

      sliderInput("JahrRange", label = "Zeitspanne:",
                  min = 1893, max = 2005, value = c(1900,2000)),
      
      selectInput("Genre", label = "Genre:", 
                  choices = list("Alle" = "Alle","Action" = "Action", 
                                 "Animation" = "Animation", "Comedy"= "Comedy",
                                 "Drama" = "Drama", "Documentary" = "Documentary", 
                                 "Romance" = "Romance", "Short" = "Short"
                                   )),
      
      sliderInput("LengthRange", label = "Filmdauer:",
                  min = 1, max = 300, value = c(30,120)),
      
      sliderInput("RatingRange", label = "Rating:",
                  min = 1, max = 10, value = c(1,10))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Jahr/Laenge", plotOutput("ImdbJahrLaenge"), 
                "Durchschnittliche Filmlaenge des jeweiligen Jahrzehntes ist:", 
                 tableOutput("statLength")
        ),
        
        tabPanel("Jahr/Rating", plotOutput("ImdbJahrRating"), 
                "Durchschnittliches Rating des jeweiligen Jahrzehntes ist:", 
                tableOutput("statRating")),
        
        tabPanel("Jahr/Budget", plotOutput("ImdbJahrBudget"),
                 "Durchschnittliches Budget des jeweiligen Jahrzehntes ist:", 
                 tableOutput("statBudget")),
        tabPanel("Budget/Rating", plotOutput("ImdbBudgetRating")),
        tabPanel("Jahre/Haeufigkeit", plotOutput("ImdbJahreHaeufigkeit")),
        tabPanel("Rating/Haeufigkeit", tableOutput("ImdbRatingHaeufigkeit")),
        tabPanel("Analyse", tableOutput("AnalyseText"), plotOutput("AnalysePlot"))
      ),
      tags$hr()
      #
      #textOutput("budget")
      #textOutput("genre"),
    )
    
  )
))