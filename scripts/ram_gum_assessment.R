##################################################
## Project: Upside vs Random Forest
## Script purpose: compare upside results with RF
## Date: 03/28/2018
## Author: Tyler Clavelle
##################################################

# Data & Libraries --------------------------------------------------------

library(broom)
library(tidyverse)
library(FishLife)
library(GUM)

# Data
ram <- read_csv(here::here("processed_data", "ram_data.csv")) # Most recent ram data 
load(here::here("processed_data", "MsyData.rdata")) # Costello et al. Msy Data
ram2 <- filter(MsyData, Dbase == 'RAM')


# RAM Formatting ----------------------------------------------------------

# Extract only stocks that were used in Costello et al. 2016
stocks <- unique(ram$stockid) 
stocks2 <- unique(ram2$IdOrig)
# Lookup table of which ram stocks to extract
stocks_to_use <- lapply(stocks, function(x) {
  data_frame(stockid = x,
             IdOrig  = stocks2[grepl(x, stocks2)],
             include = any(grepl(x, stocks2)))
}) %>% bind_rows() %>% 
  left_join(MsyData %>% 
              select(IdOrig, MSY) %>% 
              distinct())

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
  rename(stockid = key,
         initial_scaled_catch_slope = value)

# Join catch slope data with RAM
ram_df <- left_join(ram_df, catch_slope)

# Candidate variables to use
candidate_variables <-c("loo","k","winfinity","tmax","tm","m","lm","temperature", "max_catch","years_back",
                        "tcbest","scaled_lag1","scaled_lag2","scaled_lag3","scaled_lag4",
                        "mean_scaled_catch", "rolling_catch_ratio", "yrs_to_max_catch", "initial_scaled_catch_slope")

# Filter RAM to year >= 1950 and select variables of interest
ram_df <- ram_df %>% 
  select(b_bmsy,
         log_b,
         year,
         region,
         scientificname,
         stockid,
         SpeciesCatName,
         candidate_variables) %>%
  na.omit()

# Rank RAM fisheries based on the best/worst status 
ram_status_ranks <- ram_df %>% 
  group_by(stockid) %>% 
  summarize(mean_b = mean(b_bmsy, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(rank = percent_rank(mean_b))

# Join ranks with RAM data
ram_df <- left_join(ram_status_ranks, ram_df)

# Run GUM Assessment ------------------------------------------------------

# Prep data for GUM
ram_prm_df <- ram_df %>% 
  select(stockid, scientificname, SpeciesCatName, year, tcbest, loo, k, tm, temperature) %>% 
  rename(IdOrig = stockid,
         Year = year,
         SciName = scientificname,
         Catch = tcbest,
         MaxLength = loo,
         AgeMat    = tm,
         VonBertK  = k,
         Temp      = temperature) %>% 
  mutate(b_to_k_ratio = 0.4)

# Run GUM 
ram_gum <- run_gum_assessment(as.data.frame(ram_prm_df))

# Pull out results and join with observed data
ram_gum_results <- ram_gum %>% 
  select(IdOrig, year, BvBmsy, MSY) %>% 
  rename(stockid = IdOrig,
         MSY_pred = MSY) %>% 
  left_join(ram) %>% 
  select(stockid, year, bdivbmsypref, BvBmsy, MSY_pred) %>% 
  left_join(stocks_to_use %>% 
              select(stockid, MSY))

ram_gum_results %>% 
  filter(year > 1950) %>% 
  ggplot(aes(x = MSY, y = MSY_pred)) +
  geom_point(alpha = 0.4) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(intercept = 0, slope = 1)


# Save results
write_csv(ram_gum_results, path = 'processed_data/ram_gum_results.csv')
