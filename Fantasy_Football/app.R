######################################################################################################################################
# Shiny App um Draft Projections für einen Fantasy Football Draft anzuzeigen
# Daten wurden mit der ffanalytics Library erzeugt
# Tabelle darstellen und Filtermöglichkeiten liefern
# Eventuell Plotten
#####################################################################################################################################
# Environment l?schen
rm(list = ls())

#Packages laden
library(dplyr)
library(shiny)
#um einen lokalen Webserver einzurichten, port = 1410 (http://localhost:1410)
# Data Tables Library um HTML Tables darzustellen
library(DT)

# Funktionen und Daten laden
load("Predictions.RData")

load("ADP_APP.RData")

# Kleine Anpassungen an den Daten für die Anschaulichkeit
# Für Preds
Preds <- Preds %>%
  select(-c(pos, avg_type, drop_off, points_vor, floor_vor, floor_rank, ceiling_vor, ceiling_rank)) %>%
  relocate(rank, .before = id)



# ADP laden
ADP <- ADP_App

ADP <- ADP %>%
  mutate(Rank = Mean,
         Rank_Max = Max,
         Rank_Min = Min,
         Rank_Sd = Sd) %>%
  select(-c(Key, Mean, Max, Min, Sd)) %>%
  relocate(Rank, .before = Player)

# 1. UI erstellen
ui <- fluidPage(
  
  # Application title
  titlePanel("Fantasy Football Draft Projections"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Panel um spalten für Preds auszuwählen
      conditionalPanel(condition = 'input.datasets === "Preds"',
                       checkboxGroupInput("whichVars_Preds", "Select Columns for Preds:",
                       names(Preds), selected = names(Preds))),
      
      # Panel um spalten für ADP auszuwählen
      conditionalPanel(condition = 'input.datasets === "ADP"',
                       checkboxGroupInput("whichVars_ADP", "Select Columns for ADP:",
                                          names(ADP), selected = names(ADP)))
    ),
  mainPanel(
    tabsetPanel(
      id = "datasets",
      tabPanel("Preds", dataTableOutput("Predictions_Table")),
      tabPanel("ADP", dataTableOutput("ADP_Table"))
    )
  )
  )
  )



# 2. Funktionen die auf dem Server laufen (Serverlogik)
server <- function(input, output) {
  
    # Output für Predictions_Table
    output$Predictions_Table <- renderDataTable({
      
      # Festlegen wie viele Zeilen angezeigt werden
      datatable(Preds[,input$whichVars_Preds], options = list(pageLength = 50))
    })
  
  
   #Output für ADP_Table
   output$ADP_Table <- renderDataTable({
     
     # Festlegen wie viele Zeilen angezeigt werden
     datatable(ADP[,input$whichVars_ADP], options = list(pageLength = 50))
     
   })

}

# Die App mit den Einstellungen für uo und server starten
shinyApp(ui = ui, server = server)

















































