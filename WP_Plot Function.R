#######################################################################################################
# Funktion die den WP Plot erstellt
# Input: Data Frame mit den Vorhersagen
# Output: plotly Plot
#######################################################################################################
# Environment l�schen
rm(list = ls())

# Packages laden
library(dplyr)
library(plotly)

WP_Plot<- function(Data){
  
  # Kennzeichnen ob über 0.5 oder nicht
  Data$home_wp_temp <- Data$home_wp
  Data$home_wp_temp <- ifelse(Data$home_wp_temp >= 0.5,Data$home_wp_temp *1, Data$home_wp_temp *-1 )
  
  # Die Punkte, an denen die WP kippt finden
  a1 <- Data$home_wp_temp
  a2 <- c(Data$home_wp_temp[1], Data$home_wp_temp[1:nrow(Data)-1])
  a3 <- sign(a1) != sign(a2)
  Data$a3 <- a3
  
  # an den Stellen eine 0 einfügen
  insertZeroRow <- function(df,i){ 
    n <- nrow(Data)
    ndf1 <- Data[1:i,] # note these overlap by 1
    ndf2 <- Data[i:n,] # that is the row we insert
    ndf1$home_wp_temp[i] <- 0
    ndf <- rbind(ndf1,ndf2)
  }
  
  i <- 1
  while(i<nrow(Data)){
    if (Data$a3[i]){
      Data <- insertZeroRow(Data,i)
      i <- i+1
    }
    i <- i+1
  }
  
  # Für die beiden Linien
  Data$pos <- ifelse(Data$home_wp_temp >=0.5 | Data$home_wp_temp == 0, Data$home_wp, NA)
  Data$neg <- ifelse(Data$home_wp_temp < 0.5, Data$home_wp, NA)
  Data <- Data %>% mutate(row = row_number())
  
  #################################################################################################
  # Plotten
  #################################################################################################
  
  # Ursprünglichen Plot mit Plotly erstellen (wegen aes(group = 1) Problem)
  # Infos für x-Achse definieren
  ax <- list(
    showline = TRUE,
    mirror = "ticks",
    gridcolor = toRGB("black"),
    gridwidth = 4,
    linecolor = toRGB("black"),
    linewidth = 4,
    title = "Quarter",
    range = c(0, length(unique(Data$play))),
    ticktext = list("Second","Third","Fourth","End"),
    tickvals = list(min(Data[Data$qtr == 2,]$play), min(Data[Data$qtr == 3,]$play), min(Data[Data$qtr == 4,]$play),max(Data$play)),
    tickmode = "array"
  )
  
  # Infos für y-Achse definieren
  ay <- list(
    showline = TRUE,
    mirror = "ticks",
    linecolor = toRGB("black"),
    linewidth = 4,
    title = "Siegwahrscheinlichkeit",
    range = c(0, 1),
    ticktext = list("100%", "75%", "50%", "75%", "100%"),
    tickvals = list(0.0, 0.25, 0.5, 0.75, 1.0),
    tickmode = "array"
  )
  
  # Infos für Title
  title <- list(
    text = paste("Siegwahrscheinlichkeit", Data$game_id),
    font = list(color = Data[Data$label == 1,]$team_color[1], size = 15)
  )
  
  # Plot erstellen
  fig <- plot_ly(data = Data) %>%
    # 0.5 Trennlinie
    add_segments(x = 0, xend = nrow(Data), y= 0.5, yend = 0.5, line = list(dash = "dot", color = "red", width = 4)) %>%
    # Heimteam Linie
    add_trace(Data, x = ~play, y = ~pos, type = "scatter", mode = "lines",
              line = list(color = Data[Data$home == 1,]$team_color[1], width = 4),
              hoverinfo = "skip") %>% 
    # Auswaertsteam Linie
    add_trace(Data, x = ~play, y = ~neg, type = "scatter", mode = "lines",
              line = list(color = Data[Data$home == 0,]$team_color[1], width = 4),
              hoverinfo = "skip")%>%
    # Interressante Plays
    add_trace(Data, x = Data[(abs(Data$home_wp[Data$row] - Data$home_wp[Data$row + 1]) > 0.05) == 1,]$play,
              y = Data[(abs(Data$home_wp[Data$row] - Data$home_wp[Data$row + 1]) > 0.05) == 1,]$home_wp, type = "scatter", mode = "markers",
              marker = list(color = Data[(abs(Data$home_wp[Data$row] - Data$home_wp[Data$row + 1]) > 0.05) == 1,]$team_color, size = 14),
              hovertemplate = paste("Spielzug:",Data[(abs(Data$home_wp[Data$row] - Data$home_wp[Data$row + 1]) > 0.05) == 1,]$desc, "<extra></extra>")) %>%
    # Layout
    layout(title = title,
           xaxis = ax,
           yaxis = ay,
           showlegend = FALSE) 
  
  # Logo des Heimteams
  fig %>% layout(images = list(list(
    source = Data[Data$home == 1,]$team_logo_wikipedia[1],
    x =  0, y = 0.9, 
    sizex =  0.2, sizey = 0.1,
    xref = "paper", yref = "paper", 
    xanchor = "left", yanchor = "bottom"
  ),
  # Logo des Auswärtsteam
  list(
    source = Data[Data$home == 0,]$team_logo_wikipedia[1],
    x =  0, y =0, 
    sizex =  0.2, sizey = 0.1,
    xref = "paper", yref = "paper", 
    xanchor = "left", yanchor = "bottom"
  )
  )) %>% style(showlegend = F)
  
}

save(WP_Plot, file = "C:/Users/janni/OneDrive - EUFH GmbH/R-Uebungen/Football Stuff/Football/Shiny_WP/WP_Plot.RData")


