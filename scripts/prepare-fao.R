#####################################################
## Project: Are we really overfishing coastal stocks
## Script purpose: Prepare FAO data for PRM regression 
## Date: 2018-03-26
## Author: Tyler Clavelle
####################################################

# Load MSY Data from Costello et al.
load(file = '../processed_data/MsyData.rdata')

# Subset the results to include unassessed FAO fisheries and process data to include required predictors:
# Pull out historical data and select candidate vars
fao <- MsyData %>% 
  filter(Year <= 2012 & Dbase == 'FAO' & IdLevel == 'Species') %>% 
  select(IdOrig,SciName,Year,BvBmsy,Catch,RegionFAO) %>% 
  rename(stockid = IdOrig,
         year    = Year,
         scientificname = SciName,
         b = BvBmsy,
         region = RegionFAO,
         tcbest = Catch
  ) %>% 
  group_by(stockid) %>%
  arrange(year) %>% 
  ungroup()

# Get lifehistory variables for FAO data: ---------------------------------
  
# Function to get lifehistory data
get_fish_life <- function(genus, species) {
  
  Predict = Plot_taxa(
    Search_species(Genus = genus, Species = species)$match_taxonomy,
    mfrow = c(2, 2),
    partial_match = T,
    verbose = F
  )
  out <- Predict[[1]]$Mean_pred %>%
    as.matrix() %>%
    t() %>%
    as.data.frame()
  
  out[colnames(out) != 'Temperature'] <-
    exp(out[colnames(out) != 'Temperature'])
  
  return(out)
}

# Pull out genus and species from FAO data
genus_species <-
  stringr::str_split(fao$scientificname, " ", simplify = T) %>%
  as_data_frame() %>%
  select(1:2) %>%
  set_names(c("genus", "species")) %>%
  unique() %>% 
  filter(species!='')

# Get life history for FAO stocks
fish_life <- genus_species %>%
  mutate(life_traits = map2(genus, species, safely(get_fish_life)))

fish_life <- fish_life %>%
  mutate(fish_life_worked = map(life_traits, 'error') %>% map_lgl(is.null)) %>%
  filter(fish_life_worked) %>%
  mutate(life_traits = map(life_traits, 'result')) %>%
  unnest() %>%
  mutate(taxa = glue::glue('{genus} {species}')) %>%
  set_names(tolower) %>%
  filter(fish_life_worked == TRUE) %>%
  select(-fish_life_worked)

fao_data <- fao %>%
  left_join(fish_life %>% mutate(
    scientificname = glue::glue("{genus} {species}"),
    by = "scientificname"
  ))

# Save prepared FAO data
write_csv(fao_data, path = "../processed_data/fao_data.csv")