#########################################################################################################################
# ADP der verschiedenen Quellen in ein DF zusammenfassen
# Andere Informationen hinzufügen
# Funktion die dann von der Shiny App aufgerufen wird
#########################################################################################################################
# Environment löschen
rm(list = ls())

# Start der Funktion
Fantasy_Scraper <- function() {
        
        # Packages laden
        library(dplyr)
        library(rvest)
        library(stringr)
        library(nflfastR)
        library(ffanalytics)
        # Functions laden
        load(file = "C:/Users/janni/OneDrive - EUFH GmbH/R-Uebungen/Football Stuff/Fantasy Football/Scraping_Functions.RData")
        
        # Die alte ADP laden
        ADP_Old <- load(file = "C:/Users/janni/OneDrive - EUFH GmbH/R-Uebungen/Football Stuff/Fantasy Football/Shiny_App/ADP.RData")
        
        CBS <- CBS_load()
        FDG <- FDG_load()
        FFC <- FFC_load()
        FP <- FP_load()
        NFL<- NFL_load()
        
        # Alle Quellen joinen
        ADP_New <- NFL %>% 
                left_join(CBS[, c("Name", "Rank")], by = "Name") %>%
                left_join(FDG[, c("Name", "Rank")], by = "Name") %>%
                left_join(FFC[, c("Name", "Rank")], by = "Name") %>%
                left_join(FP[, c("Name", "Rank")], by = "Name") %>%
                transmute(Player = as.character(Player),
                          Position = as.factor(Position),
                          Team = as.factor(Team),
                          Key = as.factor(Name),
                          NFL_Rank = as.numeric(Rank.x),
                          CBS_Rank = as.numeric(as.character(Rank.y)),
                          FDG_Rank = as.numeric(Rank.x.x),
                          FFC_Rank = as.numeric(as.character(Rank.y.y)),
                          FP_Rank = as.numeric(as.character(Rank)))
        
        # Umgang mit NAs also Spielern die auf der Seite nicht geranked waren
        
        # Wenn der Spieler bei mehr als zwei Seiten nicht geranked ist, dann rausfiltern also NA setzen
        ADP_New[rowSums(is.na(ADP_New)) > 2, 5:9] <- NA
        
        # Wenn der Spieler bei weniger als zwei Seiten nicht geranked ist, dann für dei beiden Seiten den schlechtesten Rank einsetzen
        ADP_New[is.na(ADP_New$NFL_Rank), ]$NFL_Rank <- max(ADP_New$NFL_Rank, na.rm = T) + 1
        ADP_New[is.na(ADP_New$CBS_Rank), ]$CBS_Rank <- max(ADP_New$CBS_Rank, na.rm = T) + 1
        ADP_New[is.na(ADP_New$FDG_Rank), ]$FDG_Rank <- max(ADP_New$FDG_Rank, na.rm = T) + 1
        ADP_New[is.na(ADP_New$FFC_Rank), ]$FFC_Rank <- max(ADP_New$FFC_Rank, na.rm = T) + 1
        ADP_New[is.na(ADP_New$FP_Rank), ]$FP_Rank <- max(ADP_New$FP_Rank, na.rm = T) + 1
        
        # Es gibt leider zwei D Moore CAR WR deshalb den zweiten Eintrag entfernen
        ADP_New <- ADP_New %>%
                filter(!Key == "D Moore CAR WR " & !row_number() == 56) %>%
                filter(!Key == "D Johnson HOU RB" & !row_number() == 92)
        
        # Spalten mean, min, max einführen
        ADP_New <- ADP_New %>% 
                mutate(Mean = rowMeans(ADP_New[, 5:9]),
                       Max = apply(ADP_New[,5:9], 1, max),
                       Min = apply(ADP_New[,5:9], 1, min),
                       Sd = round(apply(ADP_New[5:9], 1, sd), 2)) %>%
                arrange(Mean)
        
        # Altes und neues ADP joinen und die Differenz berechnen
        Diff <- ADP %>% 
                select(Key, Player, Mean) %>%
                left_join(select(ADP_New, Key, Mean), by = "Key") %>%
                mutate(Diff = round(Mean.x - Mean.y), 2)
        
        ADP_New <- ADP_New %>% left_join(select(Diff, Key, Diff), by = "Key")
        
        
        # Ergebnisse zurückgeben
        return(ADP_New)
        
        # ADP_New speichern
        ADP <- ADP_New

}

# Funktion speichern 
save(Fantasy_Scraper, file = "C:/Users/janni/OneDrive - EUFH GmbH/R-Uebungen/Football Stuff/Fantasy Football/Shiny_App/Fantasy_Scraper.RData")

ADP_App <- Fantasy_Scraper()

# Daten speichern
save(ADP_App , file = "C:/Users/janni/OneDrive - EUFH GmbH/R-Uebungen/Football Stuff/Fantasy Football/Shiny_App/ADP_App.RData")












