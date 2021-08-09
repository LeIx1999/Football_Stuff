#########################################################################################################################
# ADP der verschiedenen Quellen in ein DF zusammenfassen
# Andere Informationen hinzufügen
#########################################################################################################################
# Environment löschen
rm(list = ls())

# Packages laden
library(dplyr)
library(rvest)
library(stringr)
library(nflfastR)

# Functions laden
load(file = "C:/Users/janni/OneDrive - EUFH GmbH/R-Uebungen/Football Stuff/Fantasy Football/Scraping_Functions.RData")

CBS <- CBS_load()
FDG <- FDG_load()
FFC <- FFC_load()
FP <- FP_load()
NFL<- NFL_load()

# Alle Quellen joinen
ADP <- NFL %>% 
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
ADP[rowSums(is.na(ADP)) > 2, 5:9] <- NA

# Wenn der Spieler bei weniger als zwei Seiten nicht geranked ist, dann für dei beiden Seiten den schlechtesten Rank einsetzen
ADP[is.na(ADP$NFL_Rank), ]$NFL_Rank <- max(ADP$NFL_Rank, na.rm = T) + 1
ADP[is.na(ADP$CBS_Rank), ]$CBS_Rank <- max(ADP$CBS_Rank, na.rm = T) + 1
ADP[is.na(ADP$FDG_Rank), ]$FDG_Rank <- max(ADP$FDG_Rank, na.rm = T) + 1
ADP[is.na(ADP$FFC_Rank), ]$FFC_Rank <- max(ADP$FFC_Rank, na.rm = T) + 1
ADP[is.na(ADP$FP_Rank), ]$FP_Rank <- max(ADP$FP_Rank, na.rm = T) + 1

# Es gibt leider zwei D Moore CAR WR deshalb den zweiten Eintrag entfernen
ADP <- ADP %>%
        filter(!Key == "D Moore CAR WR " & !row_number() == 56) %>%
        filter(!Key == "D Johnson HOU RB" & !row_number() == 92)

# Spalten mean, min, max einführen
ADP <- ADP %>% 
        mutate(Mean = rowMeans(ADP[, 5:9]),
               Max = apply(ADP[,5:9], 1, max),
               Min = apply(ADP[,5:9], 1, min),
               Sd = round(apply(ADP[5:9], 1, sd), 2)) %>%
        arrange(Mean)

# Daten speichern für Nutzung in shiny App
save(ADP, file = "C:/Users/janni/OneDrive - EUFH GmbH/R-Uebungen/Football Stuff/Fantasy Football/Shiny_App/ADP.RData")

