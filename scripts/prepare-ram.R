library(tidyverse)

sheets <- c("area", "stock", "timeseries_values_views")


ram <-
  map(sheets,
      ~ readxl::read_xlsx(
        here::here("data", "RLSADB v4.25 (model fits included).xlsx"),
        sheet = .x
      ) %>%
        set_names(tolower))

names(ram) <- sheets

ram_data <- ram$timeseries_values_views %>%
  select(stockid,
         stocklong,
         year,
         tcbest,
         bdivbmsypref,
         udivumsypref,
         effort) %>%
  left_join(ram$stock, by = "stockid") %>%
  left_join(ram$area, by = "areaid")


genus_species <-
  stringr::str_split(ram_data$scientificname, " ", simplify = T) %>%
  as_data_frame() %>%
  set_names(c("genus", "species")) %>%
  unique()

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

ram_data <- ram_data %>%
  left_join(fish_life %>% mutate(
    scientificname = glue::glue("{genus} {species}"),
    by = "scientificname"
  ))

write_csv(ram_data, path = here::here("processed_data","ram_data.csv"))
