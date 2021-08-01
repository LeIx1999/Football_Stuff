#########################################################################################################################
# Funktionen definieren um Fantasy Football Daten von verschiedenen Seiten zu scrapen
# Webscrpaping with R
# https://www.dataquest.io/blog/web-scraping-in-r-rvest/
# Nutzung des rvest Packages
#########################################################################################################################
# Environment löschen
rm(list = ls())

# Packages laden
library(rvest)
library(stringr)
library(dplyr)

# CBS Funktion-------------------------------------------------------------------------------------------------------------------------------
CBS_load <- function(){
  
  # Ergebnis Liste
  Ergebnis_List <- list()
  # Daten Anfragen mit read_html
  FF_html <- read_html("https://www.cbssports.com/fantasy/football/draft/averages/?print_rows=9999")
  
  # Nach tag Suchen und text extrahieren
  Res <- FF_html %>%
    html_nodes("tr") %>%
    html_text()
          
  # Durch jeden Spieler iterieren und die Infos als DF ausgeben
  for (i in 2:length(Res)) {
    
    # in einzelne character splitten
    x <- as.character(str_split(Res[i], "\n"))
    
    # character bereinigen
    x2 <- str_replace_all(x, "\"\"|\"| |c\\(|\\)|\n", "")
    
    # als Data Frame und DF bereinigen (grepl() um alles zu entfernen wo ":" enthalten ist)
    x3 <- as.data.frame(str_split(x2, ","), col.names = "Info") %>% filter(Info != "", !grepl(":", Info))
    
    # Ergebnis DF bilden und Spaltennamen definieren
    x4 <- data.frame(Rank = x3[1,], Player = x3[5,], Position = x3[6,], Team = x3[7,], Trend = x3[8,], AVGPOS = x3[9,], HILO = x3[10,], PCT = x3[11,])
    
    # DF an Liste anhängen
    Ergebnis_List[[i-1]] <- x4
    
  }
  # Liste als DF umformen
  Ergebnis <- bind_rows(Ergebnis_List)
  
  # Ergebnisse ausgeben
  return(Ergebnis)
  
}

# freedraftguide Funktion--------------------------------------------------------------------------------------------------------------
FDG_load <- function(){
  
  # Ergebnis Liste
  Ergebnis_List <- list() 
  # Content auslesen
  Res <- httr::content(httr::GET("https://www.freedraftguide.com/football/adp-aav-provider.php?NUM=&STYLE=0&AAV="))
  
  # Schleife über alle Spieler
  for (i in 1:length(Res$player_list)) {
    
    # einzelner Spieler
    x <- Res[[3]][[i]] 
    
    # Daten werden als Liste abgerufen. Liste in DF umwandeln
    x2 <- data.frame(Rank = round(as.numeric(x$avg)), Player = as.character(x$name), Position = as.character(x$position),
                     Team = as.character(x$team), Trend = as.numeric(x$change), AVGPOS = as.numeric(x$avg))
    
    # DF an Liste anhängen
    Ergebnis_List[[i]] <- x2
    
  }
  
  # Liste als Df
  Ergebnis <- bind_rows(Ergebnis_List)
  
  # Ergebnis ausgeben
  return(Ergebnis)
  
}

# NFL Funktion---------------------------------------------------------------------------------------------
NFL_load <- function(){
  
  # Ergebnis Liste erstellen
  Ergebnis_List <- list()
  
  # HTML auslesen
  FF_html <- read_html("https://fantasy.nfl.com/research/rankings?leagueId=0&statType=draftStats")
  
  # Nach dem passenden Tag suchen
  Res <- FF_html %>%
          html_node(xpath = '//*[@id="primaryContent"]') %>%
          html_text()
  
  # String als Liste splitten
  x <- str_split(Res, "--")[[1]][-101]
  
  # Die Nummern aus den Strings entfernen
  x2 <- gsub('[0-9]+', "", x)
  
  # Erstes Listenelement bereinigen
  x2[[1]][1] <- substr(x2[[1]][1], 494, nchar(x2[[1]][1]))
  
  # Spieler Listenelement splitten
  x3 <- str_split(x2, " ") 
  
  # Durch jeden Spieler iterieren
  
  for(i in 1:length(x3)){
    
    # DF für jeden Spieler
    Ergebnis_List[[i]] <- data.frame(Rank = i,
                                     Player = paste(x3[[i]][1], x3[[i]][2], sep = " "),
                                     Position = x3[[i]][3],
                                     Team = x3[[i]][5])
  }
  
  # Ergebnis als DF
  Ergebnis <- bind_rows(Ergebnis_List)

}







