library(ggplot2)
library(pROC)
# set working directory
setwd("~/MIT/Thesis/Code/Github/cnn_traits_dispersal/")
# read csv file
phylo <- read.csv("./data/target_test_phylo.csv", header = T, sep = ",")
direct_cnn <- read.csv("./data/Test_results_Unfreeze_152.csv", header = T, sep = ",")

# calculate r squared for phylo
# drop NA values
phylo <- phylo[complete.cases(phylo),]
phylo_r2 <- auc(phylo$biotic, phylo$biotic_pred)^2
phylo_r2
# calculate auc
direct_cnn <- auc(direct_cnn$test_ref, direct_cnn$test_pred_df)
direct_cnn

# plot predicted vs observed values
ggplot(data = phylo) + 
  geom_point(aes(x = biotic_pred, y = biotic)) + 
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Predicted", y = "Observed", title = paste0("Phylogenetic Imputation (R^2 = ", phylo_r2, ")")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = direct_cnn) +
  geom_point(aes(x = test_pred_df, y = test_ref)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Predicted", y = "Observed", title = paste0("Direct CNN (R^2 = ", direct_cnn_r2, ")")) +
  theme(plot.title = element_text(hjust = 0.5))
