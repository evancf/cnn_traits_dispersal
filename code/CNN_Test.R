### libraries
library(NCmisc)
library(keras)
library(tensorflow)
library(tfdatasets)
library(tidyverse)
library(tibble)
library(rsample)
library(data.table)
library(reticulate)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(tfaddons)
set.seed(123)

# create virtual environment
# virtualenv_create("r-reticulate")
# use_virtualenv("r-reticulate")

tf_config()

# model building
tf$keras$backend$clear_session()  # For easy reset of notebook state.

### helper function
# normalisation
range01_v2 <- function(x, min, max){(x - min)/(max - min)}

# function to adjust a variable randomly within a certain specified range, making use of the truncated normal distribution
truncatedsd <- function(mn, dev)
{
  rtruncnorm(1, a = (mn - dev), 
             b = (mn + dev), 
             mean = mn, sd = dev)
}

### set up tensorflow
tf$compat$v1$set_random_seed(as.integer(28))

# --------------- Set up TF_CONFIG --------------- #
# Python
# load the index and node info from the command line
# py_run_file("/home/hardyxu/Seed_Dispersal/CNN_Code/cnn_traits_dispersal/code/CNN_Config.py")
# print('Trying to resolve cluster')
## slurm_resolver <- tf$distribute$cluster_resolver$SlurmClusterResolver()
# print('Resolved cluster')

# set memory growth policy
#gpu1 <- tf$config$experimental$get_visible_devices('GPU')[[1]]
#gpu2 <- tf$config$experimental$get_visible_devices('GPU')[[2]]
#tf$config$experimental$set_memory_growth(device = gpu1, enable = TRUE)
#tf$config$experimental$set_memory_growth(device = gpu2, enable = TRUE)
# Define communication options
communication_options <- tf$distribute$experimental$CommunicationOptions(
  implementation=tf$distribute$experimental$CommunicationImplementation$AUTO)
# Assign strategy
# strategy <- tf$distribute$MultiWorkerMirroredStrategy(communication_options=communication_options)
strategy <- tf$distribute$MultiWorkerMirroredStrategy(communication_options=communication_options)
# cluster_resolver=slurm_resolver
strategy$num_replicas_in_sync
# list physical devices
tf$config$list_physical_devices()

### set paths (to corresponding trait dataset) and parameters
workdir = "~/image"
path_img = "/SSD_test"
## path_img = ""
path_ref = "/metadata_updated.txt"
outdir =  "~/image/direct_cnn_output/"

xres = 512
yres = 512
no_bands = 3

### set working directory and output directory
setwd(workdir)
dir.create(paste0(outdir), recursive = FALSE)

### read data

## reference
ref = read.table(paste0(workdir, path_ref), row.names=NULL, sep = ",", header = TRUE)

## list all img  data
path_img = list.files(paste0(workdir, path_img), full.names = T, pattern = "jpg")

# combine image data with target value data
dat <- cbind(path_img, ref)

## tfdatasets input pipeline 
create_dataset <- function(data,
                           train, # logical. TRUE for augmentation of training data
                           batch, # numeric. multiplied by number of available gpus since batches will be split between gpus
                           epochs,
                           useDSM = FALSE, 
                           shuffle = TRUE, # logical. default TRUE, set FALSE for test data
                           dataset_size){ # numeric. number of samples per epoch the model will be trained on
  if(useDSM) chnl = 4L else chnl = 3L
  
  if(shuffle){
    dataset = data %>%
      tensor_slices_dataset() %>%
      dataset_shuffle(buffer_size = length(data$img), reshuffle_each_iteration = TRUE)
  } else {
    dataset = data %>%
      tensor_slices_dataset() 
  } 
  
  dataset = dataset %>%
    dataset_map(~.x %>% list_modify( # read files and decode jpg
      img = tf$image$decode_jpeg(tf$io$read_file(.x$img), channels = chnl)
    )) %>%
    dataset_map(~.x %>% list_modify(
      img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32) %>% # convert datatype
        tf$squeeze() # removes dimensions of size 1 from the shape of a tensor
    )) %>%
    dataset_map(~.x %>% list_modify( # set shape to avoid error at fitting stage "tensor of unknown rank"
      img = tf$reshape(.x$img, shape = c(512L, 512L, chnl))
    ))
  
  if(train) {
    dataset = dataset %>%
      dataset_map(~.x %>% list_modify( # randomly flip up/down
        img = tf$image$random_flip_up_down(.x$img, seed = 1L)
        #msk = tf$image$random_flip_up_down(.x$msk, seed = 1L)
      )) %>%
      dataset_map(~.x %>% list_modify( # randomly flip left/right
        img = tf$image$random_flip_left_right(.x$img, seed = 1L)
        #msk = tf$image$random_flip_left_right(.x$msk, seed = 1L)
      )) %>%
      dataset_map(~.x %>% list_modify( # randomly assign brightness, contrast and saturation to images
        img = tf$image$random_brightness(.x$img, max_delta = 0.1, seed = 1L) %>%
          tf$image$random_contrast(lower = 0.9, upper = 1.1, seed = 2L) %>%
          tf$image$random_saturation(lower = 0.9, upper = 1.1, seed = 3L) %>% # requires 3 chnl -> with useDSM chnl = 4
          tf$clip_by_value(0, 1) # clip the values into [0,1] range.
      )) %>%
      # dataset_repeat(count = ceiling(epochs * (dataset_size/length(train_data$img))) )
      dataset_repeat()
  }
  
  
  dataset = dataset %>%
    dataset_batch(batch, drop_remainder = TRUE) %>%
    dataset_map(unname) %>%
    dataset_prefetch(buffer_size = tf$data$experimental$AUTOTUNE)
}



#### Test ####
## Evaluation
checkpoint_dir <- paste0(outdir, "checkpoints/")
load(paste0(outdir, "test_img.RData"))
load(paste0(outdir, "test_ref.RData"))

# convert to tibble
test_data = tibble(img = test_img, ref = test_ref)

# prepare dataset that can be input to the CNN
test_dataset <- create_dataset(test_data, train = FALSE, batch = 1, shuffle = FALSE, useDSM = FALSE)

# load model (use meaningful "modelname.hdf5" file)
model = load_model_hdf5(paste0(checkpoint_dir, "weights.49-1.25418.hdf5"), compile = TRUE) 

# evaluate test dataset
eval <- evaluate(object = model, x = test_dataset)
eval

# make predictions
test_pred = predict(model, test_dataset)

# save predictions
test_pred_df <- as.data.frame(test_pred)
test_pred_df <- test_pred_df$V1

# round predictions
round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}
test_pred_df_round <- round2(test_pred_df, 0)

# include species name in the output
test_img_2 <- as.data.frame(test_img)
colnames(test_img_2) <- "path_img"
joined_img <- left_join(test_img_2, dat, by = "path_img")

# allocate image names and reference data to predictions of test dataset for succeeding analysis
data_full <- cbind(as.character(test_img), joined_img['species'], test_ref, test_pred_df, test_pred_df_round)
write.csv(data_full, paste0(outdir, "Test_results.csv"))


# calculate average of predictions within species
data_full_avg <- data_full %>% group_by(species) %>% summarise(avg_pred = mean(test_pred_df_round))

# calculate r squared
r2 <- cor(data_full_avg$avg_pred, data_full_avg$ref)^2
r2