# install.packages("Rphylopars")
library("Rphylopars")
library("tidyverse")
library("data.table")

# set random seed
set.seed(123)

setwd("~/project/cnn_traits_dispersal/")

load(file = "./data/sp_tree.RData")
length(sp_tree)
sp_dat <- fread("./data/TRY_final.txt", header = T, sep = "auto", dec = ".", quote = "", data.table = T)
sp_dat <- sp_dat %>% rename(species = AccSpeciesName)


# merge sp_dat with sp_tree based on species and select only biotic seed dispersal mode
sp_dat <- left_join(sp_tree$sp.list, sp_dat, by = c("species" = "species")) %>% 
  dplyr::select(-genus, -family, -family.in.genus.list, -species.relative, -genus.relative, -output.note, -endo, -wind, -cache, -water, -ant, -attach, -ballistic)

# replace space with underscore
sp_dat$species <- gsub(" ", "_", sp_dat$species)

# split up data into train and test
trainIDX <- sample(c(TRUE, FALSE), nrow(sp_dat), replace=TRUE, prob=c(0.8,0.2))
train_sp_dat <- sp_dat[trainIDX,]
test_sp_dat <- sp_dat[!trainIDX,]

# store test data
target_test_sp_dat <- test_sp_dat
# create missing value 
test_sp_dat$biotic <- NA

# merge back together
sp_dat_train <- rbind(train_sp_dat, test_sp_dat)
sp_dat_train <- sp_dat_train[order(as.numeric(row.names(sp_dat_train))), ]

# simulate missing data
# sim_data <- simtraits(v = sp_dat_train, tree = sp_tree$phylo, nmissing = as.numeric(nrow(sp_dat_train)*0.2))

# record time
start_time <- Sys.time()
# impute missing data
pred_sp_dat <- phylopars(trait_data = sp_dat_train,tree = sp_tree$phylo)
# record time
end_time <- Sys.time()
# calculate time
end_time - start_time

# check type of the result
sapply(pred_sp_dat, class)

# save the result
save(pred_sp_dat, file = "./data/pred_sp_dat.RData")
save(sp_dat_test, file = "./data/sp_dat_test.RData")