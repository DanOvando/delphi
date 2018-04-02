##################################################
## Function purpose: Prep data from PRM regression
## Date: 08-07-2017
## Author: Tyler Clavelle
##################################################

format_for_prm <- function(df, slopeWindow) {
  
  ## Section: Add derived predictors for Panel Regression Model
  ##############################################################
  
  ## Create primary predictor variables
  df_prm <- df %>%
    filter(is.na(catch) == F) %>%
    group_by(stockid) %>%
    mutate(log_b_bmsy = log(b_bmsy),
           max_catch = max(catch, na.rm = T),
           years_back     = rev(1:length(catch)),
           scaled_catch = catch / max(catch, na.rm = T),
           scaled_catch_1 = lag(scaled_catch, n = 1),
           scaled_catch_2 = lag(scaled_catch, n = 2),
           scaled_catch_3 = lag(scaled_catch, n = 3),
           scaled_catch_4 = lag(scaled_catch, n = 4),
           mean_scaled_catch = mean(scaled_catch, na.rm = T),
           rolling_catch_ratio = catch / cummax(catch),
           yrs_to_max_catch    = year[match(max_catch, catch)] - min(year)) %>%
    ungroup()
  
  ## Calculate catch slope
  slope_window <- slopeWindow # scaled catch window to include in regression
  
  catch_slope <- df_prm %>%
    split(.$stockid) %>%
    map_df(~ lm(scaled_catch[1:6] ~  slope_window, na.action='na.omit', data = . )$coefficients['slope_window']) %>%
    gather() %>%
    rename(stockid = key,
           initial_scaled_catch_slope = value)
  
  ## join with RAM
  df_prm <- left_join(df_prm, catch_slope)
  
  # Convert necessary variables to numeric
  df_prm <- df_prm %>%
    filter(is.na(isscaap)==F) %>%
    mutate(age_50_mat = as.numeric(age_50_mat),
           max_length = as.numeric(max_length),
           vbK        = as.numeric(vbK))
  
  return(df_prm)
}