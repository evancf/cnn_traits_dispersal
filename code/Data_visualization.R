# Visualize the data
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggsci)
library(ks)
library(ape)
library(ggtree)
library(pROC)
library(ggtreeExtra)
# set the working directory
setwd("~/MIT/Thesis/Code/Github/cnn_traits_dispersal")
# set random seed
set.seed(123)

#### Calculate R squrared of CNN output based on species average ####
# read data
feature_cnn <- read.csv("data/Feature_CNN_Test_results_31.csv", header = T, sep = ",")
direct_cnn <- read.csv("data/Direct_CNN_test_results_44.csv", header = T, sep = ",")
# calculate average of predictions within species
feature_cnn_avg <- feature_cnn %>% group_by(species) %>% summarise(mean_pred = mean(test_pred_df), target = mean(test_ref))
direct_cnn_avg <- direct_cnn %>% group_by(species) %>% summarise(mean_pred = mean(test_pred_df), target = mean(test_ref))
# calculate R squared
feature_cnn_avg_R2 <- cor(feature_cnn_avg$mean_pred, feature_cnn_avg$target)^2
direct_cnn_avg_R2 <- cor(direct_cnn_avg$mean_pred, direct_cnn_avg$target)^2
feature_cnn_avg_R2
direct_cnn_avg_R2
# calculate R squared from original data
feature_cnn_R2 <- cor(feature_cnn$test_pred_df, feature_cnn$test_ref)^2
direct_cnn_R2 <- cor(direct_cnn$test_pred_df, direct_cnn$test_ref)^2
feature_cnn_R2
direct_cnn_R2

#### plot the relationship between feature extraction and direct cnn ####
feature_pred  <- read.csv("data/Feature_Pred_results.csv", header = T, sep = ",")
# add pic_name based on path
direct_cnn$pic_name <- basename(direct_cnn$as.character.test_img.)
feature_cnn$pic_name <- basename(feature_cnn$as.character.test_img.)

# calculate r squared in direct cnn for images with feature extraction 0 - 0.2 
# select file names with feature extraction <= 0.2
feature_pred_0_2 <- feature_pred[feature_pred$target_pred_df <= 0.2,]
# merge with direct cnn
direct_cnn_0_2 <- merge(direct_cnn, feature_pred_0_2, by = "pic_name")
# calculate r squared
direct_cnn_0_2_R2 <- cor(direct_cnn_0_2$test_pred_df, direct_cnn_0_2$test_ref)^2
direct_cnn_0_2_R2

# calculate r squared in direct cnn for images with feature extraction 0.2 - 0.4
# select file names with feature extraction <= 0.4 and > 0.2
feature_pred_0_4 <- feature_pred[feature_pred$target_pred_df <= 0.4 & feature_pred$target_pred_df > 0.2,]
# merge with direct cnn
direct_cnn_0_4 <- merge(direct_cnn, feature_pred_0_4, by = "pic_name")
# calculate r squared
direct_cnn_0_4_R2 <- cor(direct_cnn_0_4$test_pred_df, direct_cnn_0_4$test_ref)^2
direct_cnn_0_4_R2

# calculate r squared in direct cnn for images with feature extraction 0.4 - 0.6
# select file names with feature extraction <= 0.6 and > 0.4
feature_pred_0_6 <- feature_pred[feature_pred$target_pred_df <= 0.6 & feature_pred$target_pred_df > 0.4,]
# merge with direct cnn
direct_cnn_0_6 <- merge(direct_cnn, feature_pred_0_6, by = "pic_name")
# calculate r squared
direct_cnn_0_6_R2 <- cor(direct_cnn_0_6$test_pred_df, direct_cnn_0_6$test_ref)^2
direct_cnn_0_6_R2

# calculate r squared in direct cnn for images with feature extraction 0.6 - 0.8
# select file names with feature extraction <= 0.8 and > 0.6
feature_pred_0_8 <- feature_pred[feature_pred$target_pred_df <= 0.8 & feature_pred$target_pred_df > 0.6,]
# merge with direct cnn
direct_cnn_0_8 <- merge(direct_cnn, feature_pred_0_8, by = "pic_name")
# calculate r squared
direct_cnn_0_8_R2 <- cor(direct_cnn_0_8$test_pred_df, direct_cnn_0_8$test_ref)^2
direct_cnn_0_8_R2

# calculate r squared in direct cnn for images with feature extraction 0.8 - 1
# select file names with feature extraction > 0.8
feature_pred_0_8 <- feature_pred[feature_pred$target_pred_df > 0.8,]
# merge with direct cnn
direct_cnn_0_8 <- merge(direct_cnn, feature_pred_0_8, by = "pic_name")
# calculate r squared
direct_cnn_0_8_R2 <- cor(direct_cnn_0_8$test_pred_df, direct_cnn_0_8$test_ref)^2
direct_cnn_0_8_R2

# plot the relationship between feature extraction and direct cnn in a scatter plot with x axis as "0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1", and y axis as R squared
# create a data frame
data <- data.frame(x = c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1"), y = c(direct_cnn_0_2_R2, direct_cnn_0_4_R2, direct_cnn_0_6_R2, direct_cnn_0_8_R2, direct_cnn_0_8_R2), name=c("R2", "R2", "R2", "R2", "R2"))
# plot
ggplot(data, aes(x, y)) + geom_point(size=6, color="cyan4") + geom_line(lwd=1, color="cyan4", aes(group = name), linetype='dashed') + 
    labs(x = "\nPossibility ", y = "\nR squared") + 
    theme_classic() + theme(text = element_text(size = 20)) + theme(axis.text = element_text(size = 15, colour = "black")) 

# save the plot
ggsave("plot/feature_extraction_direct_cnn.png", width = 10, height = 10)



#### Scatter plot comparing the prediction and the target with contour levels ####

## feature extraction cnn ##
# Choose some random point if target is 0 and 1, otherwise, choose all the points
feature_cnn_rdm_0 <- feature_cnn[feature_cnn$test_ref == 0,]
feature_cnn_rdm_0 <- feature_cnn_rdm_0[sample(nrow(feature_cnn_rdm_0), 100),]
feature_cnn_rdm_1 <- feature_cnn[feature_cnn$test_ref == 1,]
feature_cnn_rdm_1 <- feature_cnn_rdm_1[sample(nrow(feature_cnn_rdm_1), 100),]
feature_cnn_rdm <- rbind(feature_cnn_rdm_0, feature_cnn_rdm_1)
feature_cnn_keep <- feature_cnn[feature_cnn$test_ref != 0 & feature_cnn$test_ref != 1,]
feature_cnn_keep <- rbind(feature_cnn_rdm, feature_cnn_keep)
# kernel density estimation
H <- Hpi(x=feature_cnn_keep[, c('test_ref', 'test_pred_df')])
est <- kde(x=feature_cnn_keep [, c('test_ref', 'test_pred_df')], H=H, compute.cont=TRUE)
# plot the contour levels
cl<-contourLevels(est, prob=c(0.5, 0.05), approx=TRUE)
png("plot/feature_extraction_scatter.png", width = 10, height = 10, units = "in", res = 300)
plot(jitter(feature_cnn_keep $test_ref, 50), feature_cnn_keep $test_pred_df, asp = 1, cex.axis = 1.2, las = 1,
 ylim=c(-0.01, 1.01), xlim=c(-0.01, 1.01), col="#005285", xlab=" ", ylab=" ", pch=16)
mtext("Targets", cex=1.5, side=1, line=2.5)
mtext("Predictions", cex=1.5, side=2, line=2.5)
par(new=TRUE)
text(0.5, 1.1, labels = "Predictions with Feature Extraction", font = 2, cex = 2, xpd = NA)
rug(feature_cnn_keep $test_ref, lwd = 0.2, ticksize = 0.015)
rug(feature_cnn_keep $test_pred_df, side = 2, lwd = 0.2, ticksize = 0.015)
plot(est,abs.cont=cl[1], labels=c(0.5),xlim=c(-0.01, 1.01), axes = FALSE, labcex=1, add=TRUE, lwd=1, col="grey10", xlab = '', ylab = '')
plot(est,abs.cont=cl[2], labels=c(0.95),xlim=c(-0.01, 1.01), axes = FALSE, labcex=1, add=TRUE, lwd=1, col="#adadad", xlab = '', ylab = '')
# nmae <- mean(abs(feature_cnn$test_ref - feature_cnn$test_pred_df))/ (max(feature_cnn$test_ref) - min(feature_cnn$test_ref)) * 100
fit <- lm(feature_cnn$test_pred_df ~ feature_cnn$test_ref)
cf <- round(coef(fit), 4) 
# mtext(paste0("NMAE = ", round(nmae, 2), " %"), 3, line=-37, cex = 2, adj = 1, at = 1)
mtext(paste0("R² = ", round(summary(fit)$r.squared, 4)), 3, line=-39, cex = 2, adj = 1, at = 1)
abline(a = 0, b = 1, col = "gray50", lty = 2)
dev.off()

## direct cnn ##
# Choose some random point if target is 0 and 1, otherwise, choose all the points
direct_cnn_rdm_0 <- direct_cnn[direct_cnn$test_ref == 0,]
direct_cnn_rdm_0 <- direct_cnn_rdm_0[sample(nrow(direct_cnn_rdm_0), 100),]
direct_cnn_rdm_1 <- direct_cnn[direct_cnn$test_ref == 1,]
direct_cnn_rdm_1 <- direct_cnn_rdm_1[sample(nrow(direct_cnn_rdm_1), 100),]
direct_cnn_rdm <- rbind(direct_cnn_rdm_0, direct_cnn_rdm_1)
direct_cnn_keep <- direct_cnn[direct_cnn$test_ref != 0 & direct_cnn$test_ref != 1,]
direct_cnn_keep <- rbind(direct_cnn_rdm, direct_cnn_keep)
# kernel density estimation
H <- Hpi(x=direct_cnn_keep[, c('test_ref', 'test_pred_df')])
est <- kde(x=direct_cnn_keep [, c('test_ref', 'test_pred_df')], H=H, compute.cont=TRUE)
# plot the contour levels
cl<-contourLevels(est, prob=c(0.5, 0.05), approx=TRUE)
png("plot/direct_scatter.png", width = 10, height = 10, units = "in", res = 300)
plot(jitter(direct_cnn_keep $test_ref, 50), direct_cnn_keep $test_pred_df, asp = 1, cex.axis = 1.2, las = 1,
 ylim=c(-0.01, 1.01), xlim=c(-0.01, 1.01), col="#00736d", xlab=" ", ylab=" ", pch=16)
mtext("Targets", cex=1.5, side=1, line=2.5)
mtext("Predictions", cex=1.5, side=2, line=2.5)
par(new=TRUE)
text(0.5, 1.1, labels = "Predictions with Direct CNN", font = 2, cex = 2, xpd = NA)
rug(direct_cnn_keep $test_ref, lwd = 0.2, ticksize = 0.015)
rug(direct_cnn_keep $test_pred_df, side = 2, lwd = 0.2, ticksize = 0.015)
plot(est,abs.cont=cl[1], labels=c(0.5),xlim=c(-0.01, 1.01), axes = FALSE, labcex=1, add=TRUE, lwd=1, col="grey10", xlab = '', ylab = '')
plot(est,abs.cont=cl[2], labels=c(0.95),xlim=c(-0.01, 1.01), axes = FALSE, labcex=1, add=TRUE, lwd=1, col="#adadad", xlab = '', ylab = '')
# nmae <- mean(abs(direct_cnn$test_ref - direct_cnn$test_pred_df))/ (max(direct_cnn$test_ref) - min(direct_cnn$test_ref)) * 100
fit <- lm(direct_cnn$test_pred_df ~ direct_cnn$test_ref)
cf <- round(coef(fit), 4)
# mtext(paste0("NMAE = ", round(nmae, 2), " %"), 3, line=-37, cex = 2, adj = 1, at = 1)
mtext(paste0("R² = ", round(summary(fit)$r.squared, 4)), 3, line=-39, cex = 2, adj = 1, at = 1)
abline(a = 0, b = 1, col = "gray50", lty = 2)
dev.off()

## phylogenetic imputation ##
# input data
phylo_imputation <- read.csv("data/target_test_phylo.csv", header = TRUE, sep = ",")
# drop na
phylo_imputation <- phylo_imputation[complete.cases(phylo_imputation),]
# Choose some random point if target is 0 and 1, otherwise, choose all the points
phylo_imputation_rdm_0 <- phylo_imputation[phylo_imputation$biotic == 0,]
phylo_imputation_rdm_0 <- phylo_imputation_rdm_0[sample(nrow(phylo_imputation_rdm_0), 100),]
phylo_imputation_rdm_1 <- phylo_imputation[phylo_imputation$biotic == 1,]
phylo_imputation_rdm_1 <- phylo_imputation_rdm_1[sample(nrow(phylo_imputation_rdm_1), 100),]
phylo_imputation_rdm <- rbind(phylo_imputation_rdm_0, phylo_imputation_rdm_1)
phylo_imputation_keep <- phylo_imputation[phylo_imputation$biotic != 0 & phylo_imputation$biotic != 1,]
phylo_imputation_keep <- rbind(phylo_imputation_rdm, phylo_imputation_keep)
# kernel density estimation
H <- Hpi(x=phylo_imputation_keep[, c('biotic', 'biotic_pred')])
est <- kde(x=phylo_imputation_keep [, c('biotic', 'biotic_pred')], H=H, compute.cont=TRUE)
# plot the contour levels
cl<-contourLevels(est, prob=c(0.5, 0.05), approx=TRUE)
png("plot/phylo_scatter.png", width = 10, height = 10, units = "in", res = 300)
plot(jitter(phylo_imputation_keep$biotic, 50), phylo_imputation_keep $biotic_pred, asp = 1, cex.axis = 1.2, las = 1,
 ylim=c(-0.01, 1.01), xlim=c(-0.01, 1.01), col="#226306", xlab=" ", ylab=" ", pch=16)
mtext("Targets", cex=1.5, side=1, line=2.5)
mtext("Predictions", cex=1.5, side=2, line=2.5)
par(new=TRUE)
text(0.5, 1.1, labels = "Predictions with Phylogenetic Information", font = 2, cex = 2, xpd = NA)
rug(phylo_imputation_keep$biotic, lwd = 0.2, ticksize = 0.015)
rug(phylo_imputation_keep$biotic_pred, side = 2, lwd = 0.2, ticksize = 0.015)
plot(est,abs.cont=cl[1], labels=c(0.5),xlim=c(-0.01, 1.01), axes = FALSE, labcex=1, add=TRUE, lwd=1, col="grey10", xlab = '', ylab = '')
plot(est,abs.cont=cl[2], labels=c(0.95),xlim=c(-0.01, 1.01), axes = FALSE, labcex=1, add=TRUE, lwd=1, col="#adadad", xlab = '', ylab = '')
# nmae <- mean(abs(phylo_imputation$biotic - phylo_imputation$biotic_pred))/ (max(phylo_imputation$biotic) - min(phylo_imputation$biotic)) * 100
fit <- lm(phylo_imputation$biotic_pred ~ phylo_imputation$biotic)
cf <- round(coef(fit), 4)
# mtext(paste0("NMAE = ", round(nmae, 2), " %"), 3, line=-37, cex = 2, adj = 1, at = 1)
mtext(paste0("R² = ", round(summary(fit)$r.squared, 4)), 3, line=-39, cex = 2, adj = 1, at = 1)
abline(a = 0, b = 1, col = "gray50", lty = 2)
dev.off()


## Feature Extraction ##
# input data
feature_extraction <- read.csv("data/Test_results_Freeze_152.csv", header = TRUE, sep = ",")
# calculate auc
auc <- roc(feature_extraction$test_ref, feature_extraction$test_pred_df)
auc <- auc$auc
# plot as two violin plots, one for test_ref=0 and one for test_ref=1
ggplot(feature_extraction, aes(x=factor(test_ref), y=test_pred_df)) + geom_violin(fill="cyan4") + 
    labs(x = "\nTargets ", y = "\nPredictions") + 
    theme_classic() + theme(text = element_text(size = 20)) + theme(axis.text = element_text(size = 15, colour = "black")) + 
    geom_text(data=data.frame(), aes(label = paste0("AUC = ", round(auc, 4))), x = Inf, y = Inf, size=10,
            hjust = 1, vjust = 1)

ggsave("plot/feature_extraction.png", width = 10, height = 10, units = "in", dpi = 300)

#### Plot phylogenetic tree ####
# https://epirhandbook.com/en/phylogenetic-trees-1.html#:~:text=ggtree%28tree%2C%20layout%20%3D%20%22circular%22%2C%20branch.length%20%3D%20%27none%27%29%20%25%3C%2B%25,%28color%20%3D%20Continent%29%2C%20%23%20tip%20color%20by%20continent.
# read in the tree
seed_dispersal_mode <- fread("data/TRY_final.txt", header = T, sep = ",", dec = ".", quote = "\"", data.table = T)
# add dashline between names in species column
seed_dispersal_mode$species <- gsub(" ", "_", seed_dispersal_mode$AccSpeciesName)
seed_dispersal_mode <- seed_dispersal_mode[, c("species", "biotic")]
tree <- read.tree("data/Phylogeny/seed_plant_phylo.tre")
# plot the tree without species names
# color the branches by biotic dispersal
# ggtree(tree, layout='circular', branch.length = 'none') %<+% seed_dispersal_mode +
#  aes(color = biotic) + 
#  scale_color_continuous(name = 'Biotic Disperse',
#                           limits=c(0, 1),
#                          low = "cyan", high = "#0a4d4d") 
#  theme(legend.position = c(0.9, 1),
#         legend.justification = c(0,1),
#         legend.title=element_text(size=0.00001),legend.text=element_text(size=0.00001))

# color the nodes by biotic dispersal
ggtree(tree, layout='circular', branch.length = 'none', color='grey') %<+% seed_dispersal_mode +
# geom_tippoint(aes(color = biotic), size = 0.6, stroke = 0, shape = 16) +
geom_fruit(geom=geom_tile, mapping=aes(color = biotic), height=1, offset=0.2) +
 scale_color_continuous(name = 'Biotic Disperse',
                        limits=c(0, 1),
                        low = "cyan", high = "#0a4747") + 
                        theme(legend.text = element_text(size = 10), legend.title = element_text(size = 15))
# save the plot
ggsave("plot/phylogenetic_tree.png", width = 10, height = 10, units = "in", dpi = 300)

# import csv from url
try <- read.csv("https://raw.githubusercontent.com/evancf/classifying-dispersal-mode/main/data/mode_dat.csv", header = T, sep = ",", dec = ".", quote = "\"")
# count unique species which datasource is not TRY or bien
length(unique(try$sp[try$datasource != "TRY" & try$datasource != "bien" & try$datasource != "fricke_svenning_2020"]))
# loop through all datasources
for (i in unique(try$datasource)){
  # count unique species which datasource is i
  print(i)
  print(length(unique(try$sp[try$datasource == i])))
}

length(unique(try$sp[try$datasource == "./data/mode_data/Vittoz and Engler 2007.csv"]))


#### Comparison inside a family ####
# connect species name to genus and family
families <- read.csv("./data/plant_genus_list.csv")
observe_data <- fread("./data/TRY_final.txt", header = T, sep = ",", dec = ".", quote = "\"", data.table = T)
## cnn with features ##
genera_dat_cnn <- tribble(specise = unique(feature_cnn$species))
genera_dat_cnn <- taxonlookup::lookup_table(unique(feature_cnn$species), by_species = TRUE)
# Move index to a column
genera_dat_cnn <- genera_dat_cnn %>% mutate(species = rownames(genera_dat_cnn)) %>% as.data.frame()
# merge with cnn data
genera_dat_cnn <- merge(genera_dat_cnn, feature_cnn, by = "species")

## Phylogenetic Imputation ##
genera_dat_phylo <- tribble(specise = unique(phylo_imputation$species))
genera_dat_phylo <- taxonlookup::lookup_table(unique(phylo_imputation$species), by_species = TRUE)
# Move index to a column
genera_dat_phylo <- genera_dat_phylo %>% mutate(species = rownames(genera_dat_phylo)) %>% as.data.frame()
# merge with phylo data
genera_dat_phylo <- merge(genera_dat_phylo, phylo_imputation, by = "species")

## Observation ##
# change name of species column
colnames(observe_data)[colnames(observe_data) == "AccSpeciesName"] <- "species"
genera_dat_obs <- tribble(specise = unique(observe_data$species))
genera_dat_obs <- taxonlookup::lookup_table(unique(observe_data$species), by_species = TRUE)
# Move index to a column
genera_dat_obs <- genera_dat_obs %>% mutate(species = rownames(genera_dat_obs)) %>% as.data.frame()
# merge with phylo data
genera_dat_obs <- merge(genera_dat_obs, observe_data, by = "species")

# select top 6 families from cnn 
top_families <- genera_dat_cnn %>% group_by(family) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(6) %>% pull(family)
# store cnn and phylo data for top 6 families
cnn_top_families <- genera_dat_cnn %>% filter(family %in% top_families) %>% select(species, family, test_pred_df)
phylo_top_families <- genera_dat_phylo %>% filter(family %in% top_families) %>% select(species, family, biotic_pred)
obs_top_families <- genera_dat_obs %>% filter(family %in% top_families) %>% select(species, family, biotic)
# randomly choose 661 species from phylo data to match the number of species in cnn data
phylo_top_families <- phylo_top_families[sample(nrow(phylo_top_families), nrow(cnn_top_families)),]
# plot prediction for top 6 families from both cnn and phylo on the same plot with violin plot
ggplot() +
  geom_violin(data = cnn_top_families, aes(x = family, y = test_pred_df, color = "CNN"), size = 0.5, position = position_dodge(width = 5)) +
  geom_violin(data = phylo_top_families, aes(x = family, y = biotic_pred, color = "Phylogenetic"), size = 0.5, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("CNN" = "#0a4d4d", "Phylogenetic" = "#0b9678")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Family", y = "Biotic Dispersal Probability")




ggplot() +
  geom_point(data = cnn_top_families, aes(x = family, y = test_pred_df, color = "CNN"), size = 3, position= position_nudge(x = -0.1, y = 0)) +
  geom_point(data = phylo_top_families, aes(x = family, y = biotic_pred, color = "Phylogenetic"), size = 3, position= position_nudge(x = 0.1, y = 0)) +
  geom_point(data = obs_top_families, aes(x = family, y = biotic, color = "Observation"), size = 3, position= position_nudge(x = 0, y = 0)) +
  scale_color_manual(values = c("CNN" = "#0a4d4d", "Phylogenetic" = "#10ECBD", "Observation" = "#0077ff")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Family", y = "Biotic Dispersal Probability", color = "Model")
ggsave("plot/compare_top_6_families.png", width = 10, height = 10, units = "in", dpi = 300)
