######################################################################################################################################
# Die Ergebnisse des WP Modells plotten und als Shiny App abbilden
# Shiny App bietet die M?glichkeit den WP Plot f?r ein Spiel abzurufen
# Au?erdem kann das Spiel weiter analysiert werden
#####################################################################################################################################
# Environment l?schen
rm(list = ls())

#Packages laden
library(dplyr)
library(plotly)
library(shiny)
#um einen lokalen Webserver einzurichten, port = 1410 (http://localhost:1410)
library(httpuv)
library(stringr)
# Daten laden
load("Daten.RData")

# Anpassung an der Spalte week, wenn week < 10 dann 0 davor
Vorhersagen <- Vorhersagen %>%
                 mutate(week = ifelse(week < 10, paste(0, week, sep = ""), as.character(week)))

# WP_Plot Funktion laden
load("WP_Plot.RData")

# Kleine Vorbereitung
# Spiele in Saison, Week und Team aufteilen
Saison = unique(sapply(strsplit(unique(Vorhersagen$game_id), "_"), `[[`, 1))
Week = unique(sapply(strsplit(unique(Vorhersagen$game_id), "_"), `[[`, 2))

# Jede Shiny App besteht aus drei Teilen (UI, Server, Verbindung)

# 1. UI erstellen
ui <- fluidPage(

    # Application title
    titlePanel("Siegwahrscheinlichkeit aller NFL Spiele"),
    
    sidebarLayout(
      sidebarPanel(
    
      # Spiel auswaehlen
      selectInput("Season", label = "Welche Saison?", Saison),
      selectInput("Week", label = "Welche Woche?", Week),
      uiOutput("secondSelection"),
      
      # Submit Button, der den Graph erstellt
      actionButton("plot", "Plot erstellen")),  
      
      # Hier soll der Plot angezeigt werden
      mainPanel(plotlyOutput(outputId = "Siegwahrscheinlichkeit"))
))



# 2. Funktionen die auf dem Server laufen (Serverlogik)
server <- function(input, output) {
    
  output$secondSelection <- renderUI({
    selectInput("Game", label = "Welches Spiel?", apply(unique(Vorhersagen[Vorhersagen$season == input$Season & Vorhersagen$week == input$Week, "game_id"]),
                                                        1, substr, 9, 20))
    
  })
    
  # Die Datan werden nur gefiltert, wenn der action Button gedrueckt wird (eventReactive())
  DataFunction <- eventReactive(input$plot, { Vorhersagen %>% 
    filter(game_id == paste(input$Season, input$Week, input$Game, sep = "_")) %>% 
    mutate(play = row_number())})
  
  
    # Plot wird erstellt, nur nachdem der action Button gedrueckt wird
    output$Siegwahrscheinlichkeit <- renderPlotly({ #output$Siegwahrscheinlichkeit ist das Objekt, was in der UI angelegt wurde, render Funktion "uebersetzt" in html

    #################################################################################################
    # Vorbereitung für den Plot
    #################################################################################################
    
    # Funktion DataFunction wird ausgefuehrt. Allerdings aendert sich nur was, wenn action Button gedrueckt wird
    Data <- DataFunction()
    
    # WP_Plot() Funktion aufrufen
    WP_Plot(Data = Data)
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
