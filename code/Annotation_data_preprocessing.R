library(data.table)
library(dplyr)

setwd("~/MIT/Thesis/Code/Github/cnn_traits_dispersal")
# import data from data folder
dat <- read.csv('data/annotation_batch_1.csv', header = T, sep = ",")
sp <- read.table('data/metadata_updated.txt', row.names=NULL, sep = ",", header = TRUE)

# if contains_fruits_seeds is 'contains', change to 1
dat$contains_fruits_seeds[dat$contains_fruits_seeds == 'contains'] <- 1
dat$contains_fruits_seeds[dat$contains_fruits_seeds == 'does_not_contain'] <- 0

# merge together
sp <- merge(sp, dat, by = 'pic_name', all.x = TRUE)
head(sp)
# drop column X
sp = subset(sp, select = -c(X))

# save as csv
write.csv(sp, file = 'data/feature_extraction_batch_1.csv', row.names = FALSE)
