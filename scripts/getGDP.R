##################################################
## Project: Upside Machine Learning
## Script purpose: Get national GDP data 
## Date: 2018-04-13
## Author: Tyler Clavelle
##################################################

# Libraries
library(tidyverse)
library(wbstats)
library(countrycode)

# Load cost data to add GDP to
cost <- read_csv(file = "max_cost_mt_df.csv")

# Add country codes to cost data
cost <- cost %>% 
  mutate(iso3c = countrycode(country,
                                  origin = "country.name",
                                  destination = "iso3c"))

# Examine the cached list about available World Bank data in the wbstats package
indicator_df <- wb_cachelist$indicators %>% 
  tbl_df() %>% 
  select(indicatorID, indicator) %>% 
  distinct()

# Filter indicator data frame for GDP related variables
gdp_vars <- wbsearch(pattern = "GDP") %>% 
  tbl_df()

# Download data for indicator NY.GDP.MKTP.PP.CD, which is for GDP, PPP (current international $)
gdp_df <- wb(indicator = "NY.GDP.MKTP.PP.CD", startdate = 2012, enddate = 2012)

# Join GDP data with cost data
gdp_df <- gdp_df %>% 
  select(iso3c, value) %>% 
  rename(gdp_2012 = value) %>% 
  right_join(cost)

# Save results
write_csv(gdp_df, path = "../../max_cost_mt_df_gdp.csv")
  