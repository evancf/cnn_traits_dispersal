##### this R-script was used to preprocess the raw data from TRY database
##### the result of this script is a dataset containing the means, standard deviations and growth forms 
##### of all valid observations for the six traits leaf area, growth height, specific leaf area, 
##### leaf nitrogen concentration, seed mass and stem specific density in their respective standard unit
##### further details: https://www.try-db.org/TryWeb/TRY_Data_Release_Notes.pdf and https://www.try-db.org/de/TabDetails.php

### portion trait data (each column)

### load libraries
library(dplyr)
library(tidyverse)
library(RCurl)
library(TNRS)
library(data.table)

# Get dispersal mode info
mode_dat <- getURL("https://raw.githubusercontent.com/evancf/classifying-dispersal-mode/main/data/mode_dat.csv")
mode_dat <- read.csv(text = mode_dat, row.names = 1) %>%
  tibble()

table(mode_dat$mode)

table(list(mode_dat$mode, mode_dat$biotic))

# Want to clean this up with TNRS
mode_sp <- tibble(sp = unique(mode_dat$sp),
                  gen = word(unique(mode_dat$sp), 1))

# This takes a few minutes
mode_tnrs <- TNRS(mode_sp$sp)

# Put this on the mode_sp dataframe
mode_sp <- mode_sp %>%
  mutate(tnrs_sp = mode_tnrs$Accepted_species,
         tnrs_fam = mode_tnrs$Accepted_family) %>%
  mutate(tnrs_sp = ifelse(tnrs_sp == "",
                          NA,
                          tnrs_sp))


# Then join this back to mode_dat
mode_dat <- mode_dat %>%
  left_join(mode_sp)


# Want to get the mode averages for the records that are actually specific
# modes (rather than just being biotic/not biotic or unspecified animals)
specific_modes <- c("ant", "attach", "ballistic", "cache", "endo", "water", "wind")
mode_specific_summary <- mode_dat %>% 
  filter(mode %in% specific_modes) %>% 
  filter(!is.na(tnrs_sp)) %>% 
  group_by(tnrs_sp, mode) %>% 
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>% 
  dplyr::select(-n) %>% 
  ungroup() %>% 
  pivot_wider(values_from = prop,
              names_from = mode,
              values_fill = 0)

# Similarly want to get a percent biotic estimate
biotic_summary <- mode_dat %>% 
  filter(!is.na(tnrs_sp)) %>% 
  group_by(tnrs_sp, biotic) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(values_from = n,
              names_from = biotic,
              values_fill = 0) %>% 
  rename("bio" = '1',
         "abio" = '0') %>% 
  mutate(biotic = bio/(bio + abio)) %>% 
  dplyr::select(-bio,
                -abio)
  
biotic_summary %>% filter(biotic < 1)

# Then will full join and call it what the original repo did

agg_wide_full <- full_join(mode_specific_summary,
                          biotic_summary)
# Note there will be some NAs at the bottom of this dataset

# Will also match the use of "AccSpeciesName" to mean the accepted species name
# according to TNRS
agg_wide_full <- agg_wide_full %>% 
  rename(AccSpeciesName = tnrs_sp)

# write table, will be reused in script "3_join_GBIF_TRY.R"
fwrite(agg_wide_full, file = "./data/TRY_final.txt", col.names = TRUE)



