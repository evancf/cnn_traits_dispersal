# Combine mannual annotation and prediction results
library(data.table)
library(dplyr)

# set the working directory
setwd("F:/Seed_Dispersal_Mode_Dict/")

# read data
manual_annotation <- read.csv("feature_extraction_batch_1.csv", header = T, sep = ",")
predict <- read.csv("Feature_Pred_results.csv", header = T, sep = ",")

# merge two dataframes by pic_name
merge_feature <- left_join(manual_annotation, predict[c('pic_name', 'target_pred_df')], by = "pic_name")
head(merge_feature, 20)

# change NA in col contains_fruits_seeds in manual_annotation with col target_pred_df
merge_feature$contains_fruits_seeds[is.na(merge_feature$contains_fruits_seeds)] <- merge_feature$target_pred_df[is.na(merge_feature$contains_fruits_seeds)]

# save the result
write.csv(merge_feature, "merge_feature.csv", row.names = F)

# read csv
merge_feature <- read.csv("merge_feature.csv", header = T, sep = ",")
