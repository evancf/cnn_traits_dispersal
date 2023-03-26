library("U.PhyloMaker")
library("tidyverse")
library("data.table")

# setwd("~/project/cnn_traits_dispersal/")

sp_dat <- fread("./data/TRY_final.txt", header = T, sep = "auto", dec = ".", quote = "", data.table = T)
megatree <- read.tree("./data/plant_megatree.tre")
families <- read.csv("./data/plant_genus_list.csv")
head(sp_dat)
head(families)

# Add in higher taxonomy (Genus)
# First remove a couple genera (that aren't genera) that cause problems
sp_dat <- sp_dat %>% filter(!AccSpeciesName == "")
sp_dat <- sp_dat %>% filter(!AccSpeciesName == "X")
# Rename AccSpeciesName to species
sp_dat <- sp_dat %>% rename(species = AccSpeciesName)


# Second, make a tibble for each species, join to higher taxonomy from lookup_table()
genera_dat <- tibble(species = unique(sp_dat$species))
genera_dat <- taxonlookup::lookup_table(unique(sp_dat$species), by_species = TRUE)
# Move index to a column
genera_dat <- genera_dat %>% mutate(species = rownames(genera_dat)) %>% as.data.frame()


# Third, add this to sp_dat (remove mold species)
sp_dat <- left_join(sp_dat, genera_dat) %>% 
  filter(group %in% c("Angiosperms", "Gymnosperms")) %>% 
  dplyr::select(-group, -order, -family)

# Don't want duplicates for the purpose of this
sp_dat <- sp_dat %>% unique()


# # Figure out which families aren't represented
# filter(sp_dat, !family %in% families$family)$family %>% unique()
filter(sp_dat, !genus %in% families$genus)$genus %>% unique()

# fam_changes <- c("Rhipogonaceae" = "Ripogonaceae",
#                      "Greyiaceae" = "Francoaceae",
#                      "Batidaceae" = "Bataceae",
#                      "Vivianiaceae" = "Geraniaceae")

# sp_dat$family <- plyr::revalue(sp_dat$family, fam_changes)
sp_dat <- left_join(sp_dat, families, by = c("genus" = "genus"))

# drop nan rows
nrow(sp_dat)
sp_dat <- sp_dat %>% filter(!is.na(genus))
nrow(sp_dat)
sp_dat <- sp_dat %>% filter(!is.na(family))
nrow(sp_dat)

# Lastly, format this for U.PhyloMaker
sp_dat <- tibble(species = sp_dat$species,
                  genus	= sp_dat$genus, 
                  family = sp_dat$family) %>% as.data.frame()

#  genus	= sp_dat$genus, family = sp_dat$family, species.relative = NA, genus.relative = NA

# Save sp_dat
save(sp_dat, file = "./data/sp_dat.RData")


# # subsample highly represented genera??
# 
# set.seed(4)
# sp_dat <- sp_dat[1:length(sp_dat$species),] # randomize order
# 
# sp_dat <- sp_dat %>% group_by(genus) %>% mutate(count = sequence(n()))
# 
# sp_dat$has_mode <- sp_dat$species %in% mode_dat$sp
# 
# sp_dat <- sp_dat %>% filter(count < 5 | has_mode)