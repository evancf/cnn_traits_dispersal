library("U.PhyloMaker")
library("tidyverse")
library("data.table")

setwd("~/project/cnn_traits_dispersal/")

# import sp_dat for species data
load("./data/sp_dat.RData")
sp.list <- sp_dat
megatree <- read.tree("./data/plant_megatree.tre")
gen.list <- read.csv("./data/plant_genus_list.csv")


dim(sp.list)
time.phylo.start <- Sys.time()
sp_tree <- phylo.maker(sp.list, megatree, gen.list, nodes.type = 1, scenario = 3) # Started around 5:00 PM Wednesday
time.phylo.end <- Sys.time()
time.phylo.end - time.phylo.start # Took about 16 hours


save(sp_tree, file = "./data/sp_tree.RData")

# Save trees
write.tree(sp_tree$phylo, file = "~/data/Phylogeny/seed_plant_phylo.tre")

# Write the phylogeny out for RevBayes analysis
write.csv(sp_tree$sp.list,
           file = "~/data/Phylogeny/seed_plant_phylo.txt")