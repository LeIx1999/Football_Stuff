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
# Fuer den refresh Button
library(shinyjs)
#um einen lokalen Webserver einzurichten, port = 1410 (http://localhost:1410)
library(httpuv)
library(stringr)
library(tidymodels)
library(nflfastR)
# Daten laden
load("Predictions.RData")
load("wp_fit.RData")

# Anpassung an der Spalte week, wenn week < 10 dann 0 davor -----------
Vorhersagen <- Predictions %>%
  mutate(week = ifelse(week < 10, paste(0, week, sep = ""), as.character(week))) %>%
  filter(qtr <= 4)

# Aktuelle Play-by-Play Daten laden, die für die noch keine Predictions erstellt wurden ------------

# Allerdings nur, wenn die Daten an dem Tag noch nicht geupdatet wurden



if(Predictions[nrow(Predictions), "Update_Zeitpunkt"] <= Sys.Date()) {
  
  pbp_2021 <-  nflfastR::load_pbp(2021) %>%
    filter(week >= as.numeric(substr(Predictions[nrow(Predictions), "game_id"], 6, 7))) %>%
    mutate(
      home = ifelse(posteam == home_team, 1, 0)
    ) %>%
    # Funktion um Diff_Time_Ratio und spread_time zu kreieren
    nflfastR:::prepare_wp_data() %>%
    filter(
      !is.na(down),
      !is.na(game_seconds_remaining),
      !is.na(yardline_100),
      !is.na(score_differential),
    ) %>%
    select(
      # label und identifier
      game_id,
      play_id,
      season,
      
      #variablen
      receive_2h_ko,
      spread_time,
      home,
      half_seconds_remaining,
      game_seconds_remaining,
      Diff_Time_Ratio,
      score_differential,
      down,
      ydstogo,
      yardline_100,
      posteam_timeouts_remaining,
      defteam_timeouts_remaining,
      home_team, away_team, season_type,
      week, posteam, defteam, qtr, total_home_score, 
      total_away_score,
      desc
    ) 
  
  # Vorhersagen erstellen
  Predictions_new <- predict(wp_fit, type = "prob", new_data = pbp_2021[,1:15])
  
  # Zusätzliche Variablen für den Plot joinen
  Predictions_new <- cbind(pbp_2021, Predictions_new) %>%
    mutate(wp = .pred_1,
           label = ifelse(wp >= 0.5, 1, 0),
           home_wp = ifelse(posteam == home_team, .pred_1, .pred_0),
           away_wp = ifelse(posteam == home_team, .pred_0, .pred_1)) %>%
    left_join(select(teams_colors_logos, team_abbr,team_name, team_color, team_color2,
                     team_logo_wikipedia), by = c("posteam" = "team_abbr")) %>%
    select(-.pred_1, -.pred_0)
  
  # Update_Zeitpunkt als Var anhaengen und nur "neue" Daten uebernehmen
  Predictions_new <- 
    Predictions_new %>%
    mutate(Update_Zeitpunkt = Sys.Date(),
           key = paste0(game_id, play_id)) %>%
    filter(!key %in% paste0(Vorhersagen$game_id, Vorhersagen$play_id), qtr <= 4) %>% 
    select(-key)
    
  
  # Neue Predictions anhaengen
  Predictions <- rbind(Predictions, Predictions_new)
  
  # Dann Predictions speichern
  save(Predictions, file = "Predictions.RData")
  
  # Anpassung an der Spalte week, wenn week < 10 dann 0 davor -----------
  Vorhersagen <- Predictions %>%
    mutate(week = ifelse(week < 10, paste(0, week, sep = ""), as.character(week)))
    

}


# WP_Plot Funktion laden
load("WP_Plot.RData")

# Kleine Vorbereitung
# Spiele in Saison, Week und Team aufteilen
Saison = unique(sapply(strsplit(unique(Vorhersagen$game_id), "_"), `[[`, 1))
Week = unique(sapply(strsplit(unique(Vorhersagen$game_id), "_"), `[[`, 2))

# Jede Shiny App besteht aus drei Teilen (UI, Server, Verbindung)

# 1. UI erstellen
ui <- fluidPage(
    
  # Funktion die aufgerufen werden muss, damit shinyjs Funktionen laufen
    useShinyjs(),

    # Application title
    titlePanel("Siegwahrscheinlichkeit aller NFL Spiele"),
    
    sidebarLayout(
      sidebarPanel(
    
      # Spiel auswaehlen
      selectInput("Season", label = "Welche Saison?", Saison, selected = Vorhersagen[nrow(Vorhersagen), "season"]),
      #selectInput("Week", label = "Welche Woche?", Week),
      uiOutput("Week_Reactive"),
      uiOutput("secondSelection"),
      
      # Submit Button, der den Graph erstellt
      actionButton("plot", "Plot erstellen"), 
      
      # Aktualisierungs Button, um neue Daten zu laden
      actionButton("refresh", "Aktualisieren")),
      
      # Hier soll der Plot angezeigt werden
      mainPanel(plotlyOutput(outputId = "Siegwahrscheinlichkeit"))
))



# 2. Funktionen die auf dem Server laufen (Serverlogik)
server <- function(input, output, session) {
  
  
  # Reactive Button: Für die Wochen, die in der Saison schon gespielt wurden
  output$Week_Reactive <- renderUI({
    selectInput("Week", label = "Welche Woche?", unique(Vorhersagen[Vorhersagen$season == input$Season, "week"]))
  })
  
  # Reactive Button: Mit den vorhandenen Spielen für die Saison und Woche  
  output$secondSelection <- renderUI({
    selectInput("Game", label = "Welches Spiel?", apply(as.matrix(unique(Vorhersagen[Vorhersagen$season == input$Season & Vorhersagen$week == input$Week, "game_id"])),
                                                        2, substr, 9, 20))
    
  })
  # Refresh Button, der die Session neu laedt
  observeEvent(input$refresh, {
    refresh()
  })
    
  # Die Daten werden nur gefiltert, wenn der action Button "plot" gedrueckt wird (eventReactive())
  DataFunction <- eventReactive(input$plot, { Vorhersagen %>% 
    filter(game_id == paste(input$Season, input$Week, input$Game, sep = "_")) %>% 
    mutate(play = row_number())})
  
    # Plot wird erstellt, nur nachdem der action Button gedrueckt wird
    output$Siegwahrscheinlichkeit <- renderPlotly({ #output$Siegwahrscheinlichkeit ist das Objekt, was in der UI angelegt wurde, render Funktion "uebersetzt" in html
    
    # aktualisieren()
    #################################################################################################
    # Vorbereitung fÃ¼r den Plot
    #################################################################################################
    # Funktion DataFunction wird ausgefuehrt. Allerdings aendert sich nur was, wenn action Button gedrueckt wird
    Data <- DataFunction()
    
    # WP_Plot() Funktion aufrufen
    WP_Plot(Data = Data)
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

