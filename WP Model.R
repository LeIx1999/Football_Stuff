#####################################################################################################################################
# Win probability / expected points model with the xgboost algorithm
# xgboost ist ein gradient boosted trees algorithmus
# Quelle: Open Source Football: https://www.opensourcefootball.com/posts/2021-04-13-creating-a-model-from-scratch-using-xgboost-in-r/
#####################################################################################################################################
# Environment löschen
rm(list = ls())

# Packages laden
library(dplyr)
library(nflfastR)
library(splitTools)
library(dials)
library(xgboost)
library(ggplot2)
library(tidyverse)

# Seed setzen
set.seed(2013)

options(scipen = 999999)

# Anzahl der hyperparameter festlegen (fuer die Grid-Search)
grid_size <- 40

################################################################################
# Datenextraktion
################################################################################
future::plan("multisession")
pbp_data <- nflfastR::load_pbp(2001:2020) %>%
  mutate(
    # Label um zu erkennen, ob das Team mit possession gewonnen hat
    # Nas und unentschieden erstmal ignorieren
    label = case_when(
      result > 0 & posteam == home_team ~ 1,
      result < 0 & posteam == away_team ~ 1,
      TRUE ~ 0
    ),
    # home team inidcator für das team mit possession
    home = ifelse(posteam == home_team, 1, 0)
  ) %>%
  # Funktion um Diff_Time_Ratio und spread_time zu kreieren
  nflfastR:::prepare_wp_data() %>%
  filter(
    !is.na(down),
    !is.na(game_seconds_remaining),
    !is.na(yardline_100),
    !is.na(score_differential),
    # overtime ist ziemlich komplex
    qtr <= 4,
    !is.na(result),
    !is.na(posteam),
    # Unentschieden ignorieren
    result != 0
  ) %>%
  select(
    # label und identifier
    label, # Zielvariable
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
    defteam_timeouts_remaining
  )

######################################################################
# Daten in Training und Test aufteilen
######################################################################
# Saison 2019 und 2020 als Testdaten
test_date <- pbp_data %>% filter(season >= 2019)

# Den Rest als Training
train_data <- pbp_data %>% filter(season < 2019)

# ein fold object erstellen
# Liste mit 5 Vektoren, alle Zeilen werden in diese Vektoren unterteilt
# game_id gruppen werden zusammengenommen, Wichtig da die Zielvariable bei jedem Play des selben Spiels gleich ist.
# Plays aus einem Spiel dürfen nicht in unterschiedliche folds
folds <- create_folds(
  y = train_data$game_id, # Gruppen
  k = 5, # Anzahl Vektoren
  type = "grouped",
  invert = TRUE
)

# Zielvariable wegschreiben
train_labels <- train_data %>% select(label)

# label und identifier Spalten aus Training entfernen
train_data <- train_data %>% select(-season, -game_id, -label)

######################################################################
# Tuning der Hyperparameter
######################################################################
grid <- grid_latin_hypercube(
  # finalizing, da mtry (Anzahl Spalten pro Split) abhaengig von der Anzahl der Spalten der Trainingsdaten ist
  finalize(mtry(), train_data),
  min_n(),
  tree_depth(),
  # lernrate festlegen. desto kleiner, desto laenger braucht der Algorithmusv (GefahrvOverfitting)
  learn_rate(range = c(-1.5, -0.5), trans = scales::log10_trans()),
  loss_reduction(),
  sample_size = sample_prop(),
  size = grid_size
) %>%
  mutate(
    # fuer den xgboost muss die Anzahl der Spalten als Verhaeltnis angegeben werden
    mtry = mtry / length(train_data),
    
    # monotone_constraints = zusaetzliche Einschränkungen (mehr Herausfinden)
    # 1 = desto groesser die Variable, desto groesser die WP
    # 0 = egal
    # -1 =desto groesser die Variable, desto kleiner die WP
    monotone_constraints = "(0, 0, 0, 0, 0, 1, 1, -1, -1, -1, 1, -1)"
    
    # Auflistung welche constraints zu welcher Variable gehört
    # gleiche Reihenfolge wie im train_data
    
    # receive_2h_ko, 0
    # spread_time, 0
    # home, 0
    
    # half_seconds_remaining, 0
    # game_seconds_remaining, 0
    # Diff_Time_Ratio, 1
    
    # score_differential, 1
    # down, -1
    # ydstogo, -1
    
    # yardline_100, -1
    # posteam_timeouts_remaining, 1
    # defteam_timeouts_remaining, -1
    
  ) %>%
  # Namen der Hyperparameter auf xgb Vorgaben anpassen
  rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )

grid

# Funktion schreiben, um einen xbg f�r jede grid Zeile zu erstellen
get_row <- function(row){
  params <- 
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("logloss"),
      eta = row$eta,
      gamma = row$gamma,
      subsample = row$subsample,
      colsample_bytree = row$colsample_bytree,
      max_depth = row$max_depth,
      min_child_weight = row$min_child_weight,
      monotone_constraints = row$monotone_constraints
    )
  # Cross validation erstellen (xgboost erstellen)
  wp_cv_model <- xgb.cv(
    params = params,
    data = as.matrix(train_data),
    label = train_labels$label,
    # Anzahl der Durchläufe, wobei der Algorithmus eh stoppt wenn die loss_reduction zu klein ist
    nrounds = 15000,
    folds = folds,
    metrics = list("logloss"),
    early_stopping_rounds = 50,
    print_every_n = 50
  )
  # Ergebnisse zusammenfassen
  # Hyperparameter
  output <- params
  # Anzahl Durchläufe
  output$iter <- wp_cv_model$best_iteration
  # Fehlerrate
  output$logloss <- wp_cv_model$evaluation_log[output$iter]$test_logloss_mean
  
  row_result <- bind_rows(output)
  
  return(row_result)
}

# Ergebnisse ausgeben, xgb f�r jede Zeile des Hyperparameter grids
results <- map_df(1:nrow(grid), function(x){
  get_row(grid[x, ])
})

# Ergebnisse als Plot darstellen
results %>% 
  select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  # pivot_longer immer praktisch, um mehrere Spalten zusammenzufassen und zu plotten
  pivot_longer(eta: min_child_weight,
               names_to = "parameter",
               values_to = "value") %>%
  ggplot(aes(value, logloss, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = F, size = 3) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "Logloss") +
  theme_minimal()
  

# Grid aufgrund der Ergebnisse anpassen
grid2 <- grid_latin_hypercube(
  # mtry soll mindestens 1/4 der Spalten sein
  mtry(range = c(1/4*ncol(train_data), ncol(train_data))),
  min_n(),
  
  # tiefe zwischen 4 und 8 
  tree_depth(range = c(4L, 8L)),
  
  # lernrate festlegen. desto kleiner, desto länger braucht der Algorithmus
  learn_rate(range = c(-1.5, -1), trans = scales::log10_trans()),
  loss_reduction(),
  sample_size = sample_prop(),
  size = grid_size
) %>%
  mutate(
    # für den xgboost muss die Anzahl der Spalten als Verhältnis angegeben werden
    mtry = mtry / length(train_data),
    
    # leichte Anpassung bei den Constraints
    monotone_constraints = "(0, 1, 0, 0, 0, 1, 1, -1, -1, -1, 1, -1)"
    
  ) %>%
  # Namen der Hyperparameter auf xgb Vorgaben anpassen
  rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )

# xgboost mit neuen Hyperparametern nochmal durchlaufen
results2 <- map_df(1:nrow(grid2), function(x) {
    get_row(grid2[x,])
   })


# Beste Parameter auswerten 
bestparameters<- results2 %>% arrange(logloss) %>% slice(1) 

# Beste Parameter wegschreiben
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta_metric = c("logloss"),
  eta = bestparameters$eta,
  gamma = bestparameters$gamma,
  subsample = bestparameters$subsample,
  colsample_bytree = bestparameters$colsample_bytree,
  max_depth = bestparameters$max_depth,
  min_child_weight = bestparameters$min_child_weight,
  monotone_constraints = bestparameters$monotone_constraints
)
# Anzahl der Wiederholungen festlegen (die beste Iteration aus dem Tuning der Hyperparameter)
nrounds <- bestparameters$iter

######################################################################
# Modell trainieren
######################################################################
wp_model <- xgboost(
  # Hyperparameter
  params = params,
  # Trainingsdaten
  data = as.matrix(train_data),
  # Zielvariable
  label = train_labels$label,
  # Anzahl der Durchläufe
  nrounds = nrounds,
  verbose = 2
)

# Variablenwichtigkeit
importance <- xgb.importance(
  feature_names = colnames(wp_model),
  model = wp_model
)

# Plotten
xgb.ggplot.importance(importance_matrix = importance)
  
######################################################################
# Predictions mit dem erstellten Modell
######################################################################
load(file = "C:/Users/janni/OneDrive - EUFH GmbH/R-Uebungen/Football Stuff/Football/wp_model.RData")

preds <- predict(
  wp_model,
  # Testdaten
  as.matrix(test_date %>% select(-label, -game_id, -play_id, -season)),
) %>%
  as_tibble() %>%
  rename(wp = value) %>%
  bind_cols(test_date)

# Predictions ausgeben
preds

######################################################################
# Model evaluation
######################################################################
# Logloss der 2019 und 2020 Saison
MLmetrics::LogLoss(preds$wp, preds$label)

# Fehlerwert überprüfen
MLmetrics::Accuracy(
  preds %>% mutate(
    pred = ifelse(wp >0.5, 1, 0)) %>%
    pull(pred),
  preds$label
)

# Das Modell ist zu ungefähr 80 Prozent richtig

# Calibration plot erstellen
plot <- preds %>%
  # BINS f+r die wp erstellen
  mutate(bin_pred_prob = round(wp / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  # calibration Ergebnisse berechnen
  summarize(
    n_plays = n(),
    n_wins = length(which(label == 1)),
    bin_actual_prob = n_wins / n_plays
  ) %>%
  ungroup()

ann_text <- data.frame(
  x = c(.25, 0.75), y = c(0.75, 0.25),
  lab = c("More times\nthan expected", "Fewer times\nthan expected")
)

plot %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1))
  labs(size = "Number of Plays",
       x = "Estimated win probability",
       y = "Observed win probability",
       title = "Win probability calibration plot") +
  geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 2) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 90),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )
Vorhersagen <- preds
######################################################################
# Predictions aufbereiten
######################################################################
# Play-by-Play von 2019 und 2020
pbp <- load_pbp(2019:2020)

# Einige Variablen dazu joinen
Vorhersagen <- Vorhersagen %>% left_join(pbp %>% select(
                               play_id, game_id, game_seconds_remaining, home_team, away_team, season_type,
                               week, posteam, defteam, qtr, total_home_score, 
                               total_away_score), by = c("game_id", "play_id")) %>%
  mutate(wp = round(wp, 3),
         spread_time = round(spread_time, 2),
         Diff_Time_Ratio = round(Diff_Time_Ratio, 2),
         home_wp = if_else(posteam == home_team, wp, 1 - wp),
         away_wp = if_else(posteam == away_team, wp, 1 - wp)) 
  
# Das Modell und die Vorhersagen speichern
save(wp_model, file = "C:/Users/janni/OneDrive - EUFH GmbH/R-Uebungen/Football Stuff/Football/wp_model.RData")
save(Vorhersagen, file = "C:/Users/janni/OneDrive - EUFH GmbH/R-Uebungen/Football Stuff/Football/Vorhersagen.RData")



  








