#####################################################################################################################################
# Win probability / expected points model with the xgboost algorithm
# xgboost ist ein gradient boosted trees algorithmus
# In Anlehnung an Ben Baldwins Model (Open Source Football)
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
library(tidymodels)

# Seed setzen
set.seed(2013)

options(scipen = 999999)
################################################################################
# Datenextraktion
################################################################################
future::plan("multisession")
pbp_data <- nflfastR::load_pbp(2001:2021) %>%
  mutate(
    # Label um zu erkennen, ob das Team mit possession gewonnen hat
    # Nas und unentschieden erstmal ignorieren
    label = case_when(
      result > 0 & posteam == home_team ~ 1,
      result < 0 & posteam == away_team ~ 1,
      TRUE ~ 0
    ),
    label = as.factor(label),
    # home team inidcator fuer das team mit possession
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
# Daten in Train, Test und CV aufteilen
######################################################################
# Saison 2020 und 2021 als Testdaten
test_data <- pbp_data %>% filter(season >= 2020) %>% select(-label)

# Den Rest als Training
train_data <- pbp_data %>% filter(season < 2020)

# Cross Validation Datensatz
# game_id gruppen werden zusammengenommen, Wichtig da die Zielvariable bei jedem Play des selben Spiels gleich ist.
# Plays aus einem Spiel duerfen nicht in unterschiedliche folds
folds<- group_vfold_cv(data = train_data,
                         group = game_id,
                         v = 5)

######################################################################
# Recipes, Spec und Workflow erstellen
######################################################################
# identifier entfernen
train_data <- train_data %>% select(-game_id, play_id)

# recipes
wp_rec <- recipe(label ~ .,
                 data = train_data)

# Modell specs festlegen
wp_spec <- boost_tree(trees = tune(),
                           mtry = tune(),
                           min_n = tune(),
                           sample_size = tune(),
                           learn_rate = tune()) %>%
  # Validation Data Set um zu testen ob gestoppt werden kann
  set_engine("xgboost", validation = 0.2) %>%
  set_mode("classification")

# Workflow erstellen
wp_wf <- workflow() %>%
  add_model(wp_spec) %>%
  add_recipe(wp_rec)


######################################################################
# Tuning der Hyperparameter
######################################################################
# Hyperparameter festlegen
wp_grid <- grid_latin_hypercube(
  mtry(range = c(round(1/4*ncol(train_data)), round(3/4*ncol(train_data)))),
  min_n(range = c(2L, 40L)),
  trees(range = c(1L, 2500)),
  sample_prop(c(0.5, 1.0)),
  # lernrate festlegen. desto kleiner, desto laenger braucht der Algorithmusv (GefahrvOverfitting)
  learn_rate(range = c(-1.5, -0.5)),
  size = 40
) 

wp_grid

# Evaluationsmetriken festlegen
wp_metrics <- metric_set(accuracy, roc_auc, mn_log_loss)

# Hyperparamter_tunen---------------------------------------------------------------------------------------------------------------------
#library(finetune)
#doParallel::registerDoParallel()

#wp_tune <- tune_race_anova(
 # wp_wf,
  #folds,
  #grid = wp_grid,
  #metrics = metric_set(mn_log_loss),
  #control = control_race(verbose_elim = T)
#)

# Hyperparameter tunen
# wp_tune <- tune_grid(wp_wf,
                     # grid = wp_grid,
                     # folds,
                     # metrics = wp_metrics)
# Modell fitten und testen---------------------------------------------------------------------------------------------------------------
# Ergebnisse analysieren
# save(wp_tune, file = "C:/Users/janni/OneDrive/R-Uebungen/Football Stuff/Football/Shiny_WP/wp_tune.RData")
load(file = "C:/Users/janni/OneDrive/R-Uebungen/Football Stuff/Football/Shiny_WP/wp_tune.RData")

# Bestes Ergebniss anschauen
show_best(wp_tune)

# Nochmal Training und Testing
wp_split <- initial_split(pbp_data, strata = game_id)

wp_train <- training(wp_split)
wp_test <- testing(wp_split) %>% select(-label)


# Modell finalisieren (Mit Trainingsdaten fitten und mit Testdaten testen)
wp_final <-
  wp_wf %>%
  finalize_workflow(select_best(wp_tune, "mn_log_loss")) %>%
  last_fit(pbp_data %>% select(-label))


# Testing
collect_metrics(wp_final)

MLmetrics::Accuracy(collect_predictions(wp_final)$.pred_class,
                    collect_predictions(wp_final)$label)

# Fertigen Workflow erstellen
wp_final <-
  wp_wf %>%
  finalize_workflow(select_best(wp_tune, "mn_log_loss"))

# Fertiges Modell fitten
wp_fit <- fit(wp_final, pbp_data)

# Modell speichern
# save(wp_fit, file = "C:/Users/janni/OneDrive/R-Uebungen/Football Stuff/Football/Shiny_WP/wp_fit.RData")

# Predictions erstellen--------------------------------------------------------------------------------------------

# Play by Play Daten f?r die entsprechenden Spiele laden
pbp_2022 <-  nflfastR::load_pbp(2018:2022) %>%
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

# Predictions erstellen
Predictions <- predict(wp_fit, type = "prob", new_data = pbp_2021[,1:15])

# Zus?tzliche Variablen f?r den Plot joinen
Predictions <- cbind(pbp_2021, Predictions) %>%
  mutate(wp = .pred_1,
         label = ifelse(wp >= 0.5, 1, 0),
         home_wp = ifelse(posteam == home_team, .pred_1, .pred_0),
         away_wp = ifelse(posteam == home_team, .pred_0, .pred_1)) %>%
  left_join(select(teams_colors_logos, team_abbr,team_name, team_color, team_color2,
                   team_logo_wikipedia), by = c("posteam" = "team_abbr")) %>%
  select(-.pred_1, -.pred_0)

# Update_Zeitpunkt noch hinzufuegen
Predictions$Update_Zeitpunkt <- as.Date("2001-01-01")

save(Predictions, file = "C:/Users/janni/OneDrive/R-Uebungen/Football Stuff/Football/Shiny_WP/Predictions.RData")















