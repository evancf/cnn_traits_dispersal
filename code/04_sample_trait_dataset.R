##### in this script, the plant functional trait dataset is being sampled and made ready for download
##### input is the resulting dataframe from "3_join_GBIF_TRY.R", output is a dataframe containing the sample

### load libraries
require(data.table)
library(dplyr)
set.seed(12345)  


### set path to files
workdir <- "~/data/GBIF_data/"


### set working directory
setwd(workdir)

### load helper function "stratified"
# function retrieved from https://gist.github.com/mrdwab/6424112
# credits to Ananda Mahto
require(devtools)
source_url("https://gist.githubusercontent.com/mrdwab/6424112/raw/1960180850d9631cb0ac64816c72b6272bc26205/stratified.R")


### read data
dat <- fread("Dat_full_ready_for_sampling.txt", header = TRUE, sep = ",", dec = ".", quote = "", data.table = T)

# some values of MODE are empty strings; assign NA to them
## dat$mode[(dat$mode == "")] <- NA

# remove duplicate download links
dat <- dat %>% distinct(identifier, .keep_all = TRUE)

# filter out species with null trait data


### sample data
# as an example, leaf area is used here; it can be replaced by any of the other five traits
# traitID's are:
# 4 = SSD = stem specific density, 
# 14 = N = leaf nitrogen content, 
# 26 = SM = seed dry mass,
# 3106 = H = plant height, 
# 3110 = LA = leaf area, 
# 3117 = SLA = specific leaf area = 1/LMA

# subset to all non-missing values of mean_3110
## leaf_area <- dat_full[!is.na(dat_full$mean_3110), ]

# exclude observations without information on growth form
## leaf_area <- leaf_area[!is.na(leaf_area$PlantGrowthForm), ]

# remove unnecessary columns
## leaf_area <- subset(leaf_area, select = c("species", "identifier", "mean_3110", "stddev_3110", "uniqID", "PlantGrowthForm", "decimalLongitude", "decimalLatitude", "bio1", "bio4", "bio7", "bio12", "bio13", "bio14", "bio15"))

## Filter out species without mode info
dat <- dat[(!is.na(dat$endo)), ]
dat <- dat[(!is.na(dat$wind)), ]
dat <- dat[(!is.na(dat$cache)), ]
dat <- dat[(!is.na(dat$water)), ]
dat <- dat[(!is.na(dat$ant)), ]
dat <- dat[(!is.na(dat$attach)), ]
dat <- dat[(!is.na(dat$ballistic)), ]
dat <- dat[(!is.na(dat$biotic)), ]


### random sampling of up to "size" (here: 8) images per stratum (= species)
# in case less than "size" images exist for a stratum, all observations are returned
dat <- as.data.frame(dat)
dispersal_mode_sample <- stratified(dat, "species", size = 8)


# save sample dataframe to disk
fwrite(dispersal_mode_sample, file = "seed_dispersal_mode_rdy_for_download.txt", col.names = TRUE)
# continue with "5_download_images.R"














