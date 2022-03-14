#########################################################################################################################
# Funktionen definieren um Fantasy Football Daten von verschiedenen Seiten zu scrapen
# Webscrpaping with R
# https://www.dataquest.io/blog/web-scraping-in-r-rvest/
# Nutzung des rvest Packages
# Quellen = c("CBS", "freedraftguide", "fantasy football calculator", "NFL", "Fantasypros")
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
    x <- str_split(Res[i], "/n")
    
    # character bereinigen
    x2 <- str_replace_all(x, "\"\"|\"| |c\\(|\\)|\n", ",")
    
    # als Data Frame und DF bereinigen (grepl() um alles zu entfernen wo ":" enthalten ist)
    x3 <- as.data.frame(str_split(x2, ","), col.names = "Info") %>% filter(Info != "", !grepl(":", Info))
    
    # Informationen über verletzte Spieler zerstören die Struktur sowie Defenses
    
    # Abfrage für verletzte Spieler
    if(nrow(x3) > 13){
      
    # Nur die Inforamtionen für den Verletzungsinformationen nehmen
      x4 <- data.frame(Rank = x3[1,], Player = paste(x3[2,], x3[3,], sep = " "), Position = x3[4,], Team = x3[5,], Trend = NA,
                       NA, HILO = NA, PCT = NA)
      
    # Abfrage für Defenses  
    } else if(x3[3, ] == "DST"){
      
      x4 <- data.frame(Rank = x3[1,], Player = paste(x3[4,], x3[3,], sep = " "), Position = x3[3,], Team = x3[4,], Trend = x3[8,],
                       AVGPOS = x3[9,], HILO = x3[10,], PCT = x3[11,])
      
    } else {
    
      # Ergebnis DF bilden und Spaltennamen definieren
      x4 <- data.frame(Rank = x3[1,], Player = paste(x3[6,], x3[7,], sep = " "), Position = x3[8,], Team = x3[9,], Trend = x3[10,],
                       AVGPOS = x3[11,], HILO = x3[12,], PCT = x3[13,])
    }
    # DF an Liste anhängen
    Ergebnis_List[[i-1]] <- x4
    
  }
  # Liste als DF umformen
  Ergebnis <- bind_rows(Ergebnis_List)
  
  # Jaguars JAC zu JAX
  Ergebnis <- Ergebnis %>%
    mutate(Team = as.character(Team))
  
  Ergebnis[Ergebnis$Team == "JAC", ]$Team <- "JAX"
  
  # Schlüssel bilden (Anfangsbuchstabe Vorname, Nachname, Team, Position)
  Ergebnis$Name <- paste(substr(sapply(str_split(Ergebnis$Player, " "), `[[`, 1), 1, 1 ), sapply(str_split(Ergebnis$Player, " "),`[[`, 2), 
                         Ergebnis$Team, Ergebnis$Position)
  
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
  
  # Schlüssel bilden (Anfangsbuchstabe Vorname, Nachname, Team, Position)
  Ergebnis$Name <- paste(substr(sapply(str_split(Ergebnis$Player, " "), `[[`, 1), 1, 1 ), sapply(str_split(Ergebnis$Player, " "),`[[`, 2), 
                         Ergebnis$Team, Ergebnis$Position)
  
  # Ergebnis ausgeben
  return(Ergebnis)
  
}

# NFL Funktion---------------------------------------------------------------------------------------------
NFL_load <- function(){
  
  # Ergebnis Liste erstellen
  Ergebnis_List <- list()
  
  # Problem auf einer Seite sind nur 100 Spieler. Deshalb zwei HTML auslesen
  # HTML auslesen
  FF_html <- read_html("https://fantasy.nfl.com/research/rankings?leagueId=0&statType=draftStats")
  
  # Nach dem passenden Tag suchen
  Res <- FF_html %>%
    html_node("tbody") %>%
    html_text()
  
  # String als Liste splitten
  x <- str_split(Res, "   ")[[1]][-101]
  
  # Die Nummern aus den Strings entfernen
  x2 <- gsub('[0-9]+|\n|\n\n|even', "", x)
  
  # Führende Leerzeichen entfernen
  x2 <- str_trim(x2)
  
  # Spieler Listenelement splitten
  x3 <- str_split(x2, " ") 
  
  # Durch jeden Spieler iterieren
  for(i in 1:length(x3)){
    
    # Sonderfall abfragen für Spieler mit Jr. oder III
    if(!x3[[i]][3] %in% c("RB", "QB", "TE", "WR", "K")) {
      
      # DF für Sonderfall Spieler
      Ergebnis_List[[i]] <- data.frame(Rank = i,
                                       Player = paste(x3[[i]][1], x3[[i]][2], x3[[i]][3], sep = " "),
                                       Position = x3[[i]][4],
                                       Team = x3[[i]][6])
    } else {
      
      # DF für die "normalen" Spieler
      Ergebnis_List[[i]] <- data.frame(Rank = i,
                                       Player = paste(x3[[i]][1], x3[[i]][2], sep = " "),
                                       Position = x3[[i]][3],
                                       Team = x3[[i]][5])
    }
  }
  
  # Ergebnis als DF
  Ergebnis <- bind_rows(Ergebnis_List)
  
  # Zweite Seite
  FF_html <- read_html("https://fantasy.nfl.com/research/rankings?offset=101&sort=average&statType=draftStats#researchRankings=researchRankings%2C%2Fresearch%2Frankings%253Foffset%253D101%2526sort%253Daverage%2526statType%253DdraftStats%2Creplace")
  
  # Neue Ergebnis_List
  Ergebnis_List <- list()
  
  # Nach dem passenden Tag suchen
  Res <- FF_html %>%
    html_node("tbody") %>%
    html_text()
  
  # String als Liste splitten
  x <- str_split(Res, "   ")[[1]][-101]
  
  # Die Nummern aus den Strings entfernen
  x2 <- gsub('[0-9]+|\n|\n\n|even|--', "", x)
  
  # Führende Leerzeichen entfernen
  x2 <- str_trim(x2)
  
  # Spieler Listenelement splitten
  x3 <- str_split(x2, " ") 
  
  # Durch jeden Spieler iterieren
  for(i in 1:length(x3)){
    
    # Sonderfall abfragen für Spieler mit Jr. oder III
    if(!x3[[i]][3] %in% c("RB", "QB", "TE", "WR", "K")) {
      
      # DF für Sonderfall Spieler
      Ergebnis_List[[i]] <- data.frame(Rank = 98 + i,
                                       Player = paste(x3[[i]][1], x3[[i]][2], x3[[i]][3], sep = " "),
                                       Position = x3[[i]][4],
                                       Team = x3[[i]][6])
    } else {
      
      # DF für die "normalen" Spieler
      Ergebnis_List[[i]] <- data.frame(Rank = 98 + i,
                                       Player = paste(x3[[i]][1], x3[[i]][2], sep = " "),
                                       Position = x3[[i]][3],
                                       Team = x3[[i]][5])
    }
  }
  # Ergebnis als DF
  Ergebnis<- rbind(Ergebnis, bind_rows(Ergebnis_List))
  
  # Team LA zu LAR ändern
  Ergebnis <- Ergebnis %>%
    mutate(Team = as.character(Team)) %>%
    slice(1:nrow(Ergebnis) - 1) %>%
    filter(!is.na(Team))
  
  Ergebnis[Ergebnis$Team == "LA",]$Team <- "LAR"
  
  # Schlüssel bilden (Anfangsbuchstabe Vorname, Nachname, Team, Position)
  Ergebnis$Name <- paste(substr(sapply(str_split(Ergebnis$Player, " "), `[[`, 1), 1, 1 ), sapply(str_split(Ergebnis$Player, " "),`[[`, 2), 
                         Ergebnis$Team, Ergebnis$Position)
  
  
  # Ergebnis ausgeben
  return(Ergebnis)
}


Daten3 <- NFL_load()

# Fantasy Football Calculator Funktion---------------------------------------------------------------------------------------------
FFC_load <- function(){
  
  # Ergebnis Liste erstellen
  Ergebnis_List <- list()
  
  # html auslesen
  FF_html <- read_html("https://fantasyfootballcalculator.com/adp/ppr")
  
  # html tag table auslesen
  Res <- FF_html %>%
    html_node("table") %>%
    html_text()
  
  # Inhalt in einzelne Character aufteilen
  x <- as.data.frame.list(str_split(Res, "\n"))
  
  # Schleife um durch das DF zu iterieren und Daten eines Spielers in eine Zeile
  i = 17
  while (i <= nrow(x)){
    Ergebnis_List[[i / 17]] <- data.frame(Rank = str_trim(x[i,]),
                                     Player = str_trim(x[i + 2, ]),
                                     Position = str_trim(x[i + 3, ]),
                                     Team = str_trim(x[i + 4, ]))
    
    i = i + 17  
  }
  
  # Ergebnis als DF
  Ergebnis <- bind_rows(Ergebnis_List) %>%
    filter(!is.na(Player))
  
  # Schlüssel bilden (Anfangsbuchstabe Vorname, Nachname, Team, Position)
  Ergebnis$Name <- paste(substr(sapply(str_split(Ergebnis$Player, " "), `[[`, 1), 1, 1 ), sapply(str_split(Ergebnis$Player, " "),`[[`, 2), 
                         Ergebnis$Team, Ergebnis$Position)
  
  # Ergebnis ausgeben
  return(Ergebnis)
}

Daten4 <- FFC_load()
View(Daten4)

# FantasyPros Funktion ---------------------------------------------------------------------------------------------
FP_load <- function(){
  
  # Ergebnis Liste erstellen
  Ergebnis_List <- list()
  
  FF_html <- read_html("https://www.fantasypros.com/nfl/adp/half-point-ppr-overall.php")
  
  Res <- FF_html %>%
    html_node("table") %>%
    html_text()
  
  # Inhalte in Character splitten
  x <- as.data.frame(str_split(Res, "\n"))
  
  # Schleife um durch das DF zu iterieren
  # Die ersten 5 Zeilen sind nicht wichtig
  i = 5 
  while(i <= nrow(x)){
    
    # Sonderfälle wie Namen mit Jr. oder III abfragen sowie Defenses
    # Namen
    if(str_split(x[i + 1,], " ")[[1]][3] %in% c("Jr.", "III", "V", "II", "Sr.")){
      
      # Namen einfach erweitern
      Ergebnis_List[[i/5]] <- data.frame(Rank = x[i,],
                                         Player = paste(str_split(x[i + 1,], " ")[[1]][1], str_split(x[i + 1,], " ")[[1]][2],
                                                        str_split(x[i + 1,], " ")[[1]][3], sep = " "),
                                         Position = substr(x[i + 2,], 1, 2),
                                         Team = str_split(x[i + 1, ], " ")[[1]][4])
    # Defenses
    } else if(length(str_split(x[i + 1,], " ")[[1]]) >=4 && "DST" %in% 
              c(str_split(x[i + 1,], " ")[[1]][3], str_split(x[i + 1,], " ")[[1]][4])){
      
      # Bei den Defenses fehlt leider das Team
      Ergebnis_List[[i/5]] <- data.frame(Rank = x[i,],
                                         Player = ifelse(length(str_split(x[i + 1,], " ")[[1]]) == 6,
                                                         paste(str_split(x[i + 1,], " ")[[1]][1], str_split(x[i + 1,], " ")[[1]][2],
                                                         str_split(x[i + 1,], " ")[[1]][3], sep = " "),
                                                         paste(str_split(x[i + 1,], " ")[[1]][1], str_split(x[i + 1,], " ")[[1]][2],
                                                               sep = " ")),
                                         Position = substr(x[i + 2,], 1, 3),
                                         Team = NA)
      
    } else {
      
    # DF für die "normalen" Spieler erzeugen
    Ergebnis_List[[i/5]] <- data.frame(Rank = x[i,],
                                       Player = paste(str_split(x[i + 1,], " ")[[1]][1], str_split(x[i + 1,], " ")[[1]][2], sep = " "),
                                       Position = substr(x[i + 2,], 1, 2),
                                       Team = str_split(x[i + 1, ], " ")[[1]][3])
    }
    
    i = i + 7
  }
  # Ergebnis als DF
  Ergebnis <- bind_rows(Ergebnis_List)
  
  # Jaguars JAC zu JAX
  Ergebnis <- Ergebnis %>%
    mutate(Team = as.character(Team))
  
  Ergebnis[Ergebnis$Team == "JAC" & !is.na(Ergebnis$Team), ]$Team <- "JAX"
  
  # Schlüssel bilden (Anfangsbuchstabe Vorname, Nachname, Team, Position)
  Ergebnis$Name <- paste(substr(sapply(str_split(Ergebnis$Player, " "), `[[`, 1), 1, 1 ), sapply(str_split(Ergebnis$Player, " "),`[[`, 2), 
                         Ergebnis$Team, Ergebnis$Position)
  
  # Ergebnis ausgeben
  return(Ergebnis)
}

FP_load()

# Funktionen speichern
save(CBS_load, FDG_load, FFC_load, FP_load, NFL_load,
     file = "C:/Users/janni/OneDrive - EUFH GmbH/R-Uebungen/Football Stuff/Fantasy Football/Scraping_Functions.RData")

save(CBS_load, file = "C:/Users/janni/OneDrive - EUFH GmbH/R-Uebungen/Football Stuff/Fantasy Football/test.RData")

