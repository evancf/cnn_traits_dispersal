##### this R-script was used to preprocess the raw data from TRY database
##### the result of this script is a dataset containing the means, standard deviations and growth forms 
##### of all valid observations for the six traits leaf area, growth height, specific leaf area, 
##### leaf nitrogen concentration, seed mass and stem specific density in their respective standard unit
##### further details: https://www.try-db.org/TryWeb/TRY_Data_Release_Notes.pdf and https://www.try-db.org/de/TabDetails.php


### load libraries
library(dplyr)
library(tidyverse)
require(data.table)


### set paths to data files
workdir <- getwd()
path_try <- "./data/13238.txt"


### read data
setwd(workdir)

## plant functional trait data
trydat <- fread(path_try, header = T, sep = "\t", dec = ".", quote = "", 
                data.table = T, select = c("DataID", "AccSpeciesName", "StdValue", "UnitName", "TraitID", "TraitName", "ErrorRisk"))
head(trydat)
dim(trydat)

### preprocessing

## remove those entries with high uncertainty (as explained in release notes)
trydat <- trydat[!(trydat$ErrorRisk >= 4), ]

## remove TraitID = NA indicating missing observations
trydat <- subset(trydat, !is.na(trydat$TraitID))

# In section below, need to convert to something that applies for categorical data
# Maybe columns should be portion of records (for the species) that is each dispersal mode
# Under the column of wind dispersal = 0.75, under the column for attachment = 0.25 (if there are 4 total records)
# ## group by AccSpeciesName (species name) and Trait ID (plant functional trait), compute mean and SD
# agg <- trydat %>%
#   group_by(AccSpeciesName, TraitID) %>%
#   summarize(mean = mean(StdValue, na.rm = TRUE),
#             stddev = sd(StdValue, na.rm = TRUE))
# 
# ## convert to dataframe
# agg <- as.data.frame(agg)
# 
# ## convert from long to wide format to create new columns for each value-trait combination
# agg_wide <- pivot_wider(agg, names_from = TraitID, values_from = c("mean", "stddev"))
# 
# ## convert to dataframe
# agg_wide <- as.data.frame(agg_wide)
# 
# ### join with table containing growth form by "AccSpeciesName"
# agg_wide_full <- left_join(agg_wide, gf, by = "AccSpeciesName")
# 
# # write table, will be reused in script "3_join_GBIF_TRY.R"
# fwrite(agg_wide_full, file = "TRY_final.txt", col.names = TRUE)



