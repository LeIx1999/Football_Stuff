#########################################################################################################################
# Algorithmus um Fantansy Football Points zu predicten
# Nutzung des ffanalytics Packages
# https://fantasyfootballanalytics.net/2016/06/ffanalytics-r-package-fantasy-football-data-analysis.html
#########################################################################################################################
# Environment löschen
rm(list = ls())

# Packages laden
library("ffanalytics")

# Daten laden
# Historische Fantasy Football Daten laden
HData <- scrape_data(src = c("CBS", "ESPN", "FantasyData", "FantasyPros", "FantasySharks", "FFToday",
                              "FleaFlicker", "NumberFire", "Yahoo", "FantasyFootballNerd", "NFL", "RTSports",
                              "Walterfootball"),
                     pos = c("QB", "RB", "WR", "TE", "K"),
                     season = 2020,
                     week = 0)

HData2 <- scrape_data(src = "CBS",
                      pos = "QB",
                      season = 2021,
                      week = 5)

Qb <- HData$QB

# Predictions für die 2021 Saison
Preds <- projections_table(HData)

# WR analysieren
Wr_Data <- Preds %>% 
  filter(pos == "WR") %>%
  add_player_info() %>%
  arrange(pos_rank) %>%
  view()


