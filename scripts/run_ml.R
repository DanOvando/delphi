#######################################################
## Project: Unassessed Fisheries via Machine Learning
## Script purpose: Train ML models and predict unassessed status
## Date: 2018-04-03
## Author: Tyler Clavelle
######################################################

# Packages and Data -------------------------------------------------------

set.seed(43)

library(recipes)
library(rsample)
library(caret)
library(broom)
library(tidyposterior)
library(tidyverse)
library(FishLife)
library(GUM)

ram <- read_csv(here::here("processed_data", "ram_data.csv")) # Most recent ram data 
load(here::here("processed_data", "MsyData.rdata")) # Costello et al. Msy Data
rm(BiomassData) # remove BiomassData (not used)
ram2 <- filter(MsyData, Dbase == 'RAM')

# Data Processing ---------------------------------------------------------

# Extract only stocks that were used in Costello et al. 2016
stocks <- unique(ram$stockid) 
stocks2 <- unique(ram2$IdOrig)
# Lookup table of which ram stocks to extract
stocks_to_use <- lapply(stocks, function(x) {
  data_frame(stockid = x,
             IdOrig  = stocks2[grepl(x, stocks2)],
             include = any(grepl(x, stocks2)))
}) %>% bind_rows()

# Filter join the new ram data to only include stocks from Costello et al.
ram_final <- stocks_to_use %>% 
  filter(include == TRUE) %>% 
  inner_join(ram) %>% 
  left_join(ram2 %>% 
              select(IdOrig, SpeciesCatName) %>% 
              distinct())

## Create primary predictor variables
ram_df <- ram_final %>%
  filter(is.na(tcbest) == F) %>%
  mutate(b_bmsy = pmin(6, bdivbmsypref)) %>% 
  group_by(stockid) %>%
  mutate(log_b               = log(b_bmsy),
         max_catch           = max(tcbest, na.rm = T),
         years_back          = rev(1:length(tcbest)),
         scaled_catch        = tcbest / max(tcbest, na.rm = T),
         scaled_lag1         = lag(scaled_catch, n = 1),
         scaled_lag2         = lag(scaled_catch, n = 2),
         scaled_lag3         = lag(scaled_catch, n = 3),
         scaled_lag4         = lag(scaled_catch, n = 4),
         mean_scaled_catch   = mean(scaled_catch, na.rm = T),
         rolling_catch_ratio = tcbest / cummax(tcbest),
         yrs_to_max_catch    = year[match(max_catch, tcbest)] - min(year)) %>%
  ungroup()

# Calculate scaled catch window to include in regression
slope_window <- 1:6 

catch_slope <- ram_df %>%
  split(.$stockid) %>%
  map_df(~ lm(scaled_catch[1:6] ~  slope_window, na.action='na.omit', data = . )$coefficients['slope_window']) %>%
  gather() %>%
  dplyr::rename(stockid = key,
                initial_scaled_catch_slope = value)

# Join catch slope data with RAM
ram_df <- left_join(ram_df, catch_slope)

# Candidate variables to use
candidate_variables <-c("loo","k","winfinity","tmax","tm","m","lm","temperature", "max_catch","years_back",
                        "tcbest","scaled_lag1","scaled_lag2","scaled_lag3","scaled_lag4",
                        "mean_scaled_catch", "rolling_catch_ratio", "yrs_to_max_catch", "initial_scaled_catch_slope")

# Filter RAM to year >= 1950 and select variables of interest
ram_df <- ram_df %>% 
  dplyr::select(b_bmsy,
                log_b,
                year,
                region,
                scientificname,
                stockid,
                SpeciesCatName,
                candidate_variables) %>%
  na.omit()

# Model Prep ----------------------------------------------

# Create a training and testing dataset based on the best/worst status fisheries in RAM
ram_status_ranks <- ram_df %>% 
  group_by(stockid) %>% 
  summarize(mean_b = mean(b_bmsy, na.rm = T),
            mean_c = mean(tcbest, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(rank_b = percent_rank(mean_b),
         rank_c = percent_rank(mean_c))

# Join ranks with RAM data
ram_df <- left_join(ram_status_ranks, ram_df)

# Dependent variable is B/Bmsy
dep_var <- "b_bmsy"

# Model recipe B/Bmsy ~ independent variables
ram_recipe <- recipes::recipe(b_bmsy ~ ., data = ram_df %>% select(dep_var, candidate_variables))

# Center and scale all life history and year terms for both models
# Variables to adjust
vars_to_scale <- c("loo","k","winfinity","tmax","tm","m","lm","temperature","years_back")

# B/Bmsy model
ram_recipe_1 <- ram_recipe %>%
  recipes::step_center(vars_to_scale) %>%
  recipes::step_scale(vars_to_scale)

# Save recipes in dataframe
recipe_df <- data_frame(recipe_name = c('b'),
                        recipe      = list(ram_recipe))

# Build model data frame with training and testing data split based on catch percentages
# Only consider RAM stocks with mean lifetime B/Bmsy below 3
ram_df <- ram_df %>%
  filter(mean_b < 3)

# Build training/testing sets based on splitting the data by mean lifetime catch
models <- expand.grid(train_perc  = c(0.2, 0.4, 0.6, 0.8),
                      split_type  = 'status',
                      recipe_name = c('b')) %>%
  left_join(recipe_df) %>%
  group_by(recipe_name, train_perc) %>%
  mutate(split_metric = 'catch',
         training     = map(train_perc, .f = ~filter(ram_df, rank_c > .x)),
         testing      = map(train_perc, .f = ~filter(ram_df, rank_c <= .x))) %>% 
  bind_rows(expand.grid(train_perc  = c(0.2, 0.4, 0.6, 0.8),
                        split_type  = 'status',
                        recipe_name = c('b')) %>%
              left_join(recipe_df) %>%
              group_by(recipe_name, train_perc) %>%
              mutate(split_metric = 'b_bmsy',
                     training     = map(train_perc, .f = ~filter(ram_df, rank_b > .x)),
                     testing      = map(train_perc, .f = ~filter(ram_df, rank_b <= .x))))


# Create a training/testing set based on a proportional split unrelated to status
models_prop <- expand.grid(train_perc   = c(0.8),
                           split_type   = 'random',
                           split_metric = 'random',
                           recipe_name  = c('b')) %>%
  left_join(recipe_df)

ram_split <- rsample::initial_split(ram_df, 0.8)
ram_testing <- rsample::testing(ram_split)
ram_training <- rsample::training(ram_split)

models_prop$training <- list(ram_training)
models_prop$testing <- list(ram_testing)

# Bind the two training types together
models <- bind_rows(models, models_prop)

# Save training/testing data object
save(models, file = "processed_data/training_testing_data.Rdata")

# Model Training ----------------------------------------------------------

# Train control parameters
fit_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 1,
  allowParallel = TRUE
)

# Random Forest
fit_models <- models %>% 
  ungroup() %>% 
  mutate(recipe_prep = map2(recipe, training, ~prep(.x, training = .y)), # prep the recipe with the training data
         testing     = map2(recipe_prep, testing, ~bake(.x, newdata = .y)), # bake the testing data
         trained_rf  = map2(recipe, training, ~caret::train(.x, data = .y, 
                                                            method = "ranger",
                                                            trControl = fit_control))) # train the various models

# Gradient Boosted 
fit_models_gbm <- models %>% 
  ungroup() %>% 
  mutate(recipe_prep  = map2(recipe, training, ~prep(.x, training = .y)), # prep the recipe with the training data
         testing      = map2(recipe_prep, testing, ~bake(.x, newdata = .y)), # bake the testing data
         trained_gbm  = map2(recipe, training, ~caret::train(.x, 
                                                             data = .y, 
                                                             method = "gbm",
                                                             trControl = fit_control,
                                                             verbose = FALSE))) # train the various models

# Multivariate Adaptive Regression Splines (MARS)  
fit_models_mars <- models %>% 
  ungroup() %>% 
  mutate(recipe_prep   = map2(recipe, training, ~prep(.x, training = .y)), # prep the recipe with the training data
         testing       = map2(recipe_prep, testing, ~bake(.x, newdata = .y)), # bake the testing data
         mars_grid     = map(training, .f = ~expand.grid(degree = 1:2, nprune = seq(2, ncol(.x), by = 2))),
         trained_mars  = pmap(list(recipe, training, mars_grid), ~caret::train(..1, 
                                                             data = ..2, 
                                                             method = "earth",
                                                             trControl = fit_control,
                                                             tuneGrid = ..3,
                                                             trace = 1))) # train the various models

# Model Predictions -------------------------------------------------------

# Random Forest
rf_preds <- fit_models %>% 
  mutate(testing = map2(trained_rf, testing, ~.y %>% 
                          mutate(pred = predict(.x$finalModel, data = .y %>% 
                                                  select(candidate_variables))$predictions)),
         training = map2(recipe_prep, training, ~bake(.x, newdata = .y)), # bake the training data
         training = map2(trained_rf, training, ~.y %>% 
                           mutate(pred = predict(.x$finalModel, data = .y %>% 
                                                   select(candidate_variables))$predictions)))

# GBM
gbm_preds <- fit_models_gbm %>% 
  mutate(testing = map2(trained_gbm, testing, ~.y %>% 
                          mutate(pred = predict(.x$finalModel, newdata = .y %>% 
                                                  select(candidate_variables),
                                                n.trees = .x$bestTune$n.trees))),
         training = map2(recipe_prep, training, ~bake(.x, newdata = .y)), # bake the training data
         training = map2(trained_rf, training, ~.y %>% 
                           mutate(pred = predict(.x$finalModel, newdata = .y %>% 
                                                   select(candidate_variables),
                                                 n.trees = .x$bestTune$n.trees))))

# MARS predictions
mars_preds <- fit_models_mars %>% 
  mutate(testing = map2(trained_mars, testing, ~.y %>% 
                          mutate(pred = predict(.x$finalModel, newdata = .y %>% 
                                                  select(candidate_variables)))),
         training = map2(recipe_prep, training, ~bake(.x, newdata = .y)), # bake the training data
         training = map2(trained_mars, training, ~.y %>% 
                           mutate(pred = predict(.x$finalModel, newdata = .y %>% 
                                                   select(candidate_variables)))))

# Join prediction datasets
all_preds <- rf_preds %>% 
  select(train_perc, split_type, split_metric, recipe_name, testing) %>%
  unnest() %>% 
  mutate(model = 'RF') %>%
  bind_rows(gbm_preds %>%
            select(train_perc, split_type, split_metric, recipe_name, testing) %>%
            unnest() %>%
            mutate(model = 'GBM')) %>% 
  bind_rows(mars_preds %>%
              select(train_perc, split_type, split_metric, recipe_name, testing) %>%
              unnest() %>%
              mutate(model = 'MARS'))


# Save Predictions --------------------------------------------------------

write_csv(all_preds, path = "processed_data/ml_predictions.csv")
