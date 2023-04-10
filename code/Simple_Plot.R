library(ggplot2)
# set working directory
setwd("~/MIT/Thesis/Code/Github/cnn_traits_dispersal/")
# read csv file
phylo <- read.csv("./data/target_test_phylo.csv", header = T, sep = ",")
direct_cnn <- read.csv("./data/direct_CNN_test_results.csv", header = T, sep = ",")

# calculate r squared for phylo
# drop NA values
phylo <- phylo[complete.cases(phylo),]
phylo_r2 <- cor(phylo$biotic, phylo$biotic_pred)^2
phylo_r2
# calculate r squared for direct cnn
direct_cnn_r2 <- cor(direct_cnn$test_ref, direct_cnn$test_pred_df)^2
direct_cnn_r2

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
