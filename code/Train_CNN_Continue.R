##### in this script, the CNN models are trained 
##### input is the metadata from script "6_image_processing.R" and the corresponding image data
##### all setups are considered in this modular script
##### the code for each setup is clearly marked, the TA and Bioclim setups can be added if desired


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
library(tfaddons)
set.seed(123)

# model building
tf$keras$backend$clear_session()  # For easy reset of notebo

### set paths (to corresponding trait dataset) and parameters
workdir = "/pool001/hardyxu"
path_img = "/SSD_test"
## path_img = ""
path_ref = "/metadata_updated.txt"
outdir = "/pool001/hardyxu/output/"

# Define communication options
communication_options <- tf$distribute$experimental$CommunicationOptions(
  implementation=tf$distribute$experimental$CommunicationImplementation$AUTO)
# Assign strategy
# strategy <- tf$distribute$MultiWorkerMirroredStrategy(communication_options=communication_options)
strategy <- tf$distribute$MultiWorkerMirroredStrategy(communication_options=communication_options)
# cluster_resolver=slurm_resolver
strategy$num_replicas_in_sync

### Import saved data
training_dataset <- load(paste0(outdir, "training_dataset.RData"))
validation_dataset <- load(paste0(outdir, "validation_dataset"))




### Continue training the model
with(strategy$scope(), {model <- load_model_tf(paste0(outdir, "saved_model/my_model"))})

### Setup checkpoint
checkpoint_dir <- paste0(outdir, "checkpoints")
unlink(checkpoint_dir, recursive = TRUE)
dir.create(checkpoint_dir, recursive = TRUE)
filepath = file.path(checkpoint_dir, "weights.{epoch:02d}-{val_loss:.5f}.hdf5")

cp_callback <- callback_model_checkpoint(filepath = filepath,
                                         monitor = "val_loss",
                                         save_weights_only = FALSE,
                                         save_best_only = FALSE,
                                         verbose = 1,
                                         mode = "auto",
                                         save_freq = "epoch")


### Start training the model
model %>% fit(x = training_dataset,
              epochs = epochs,
              steps_per_epoch = floor(length(train_data$img)/batch_size), 
              callbacks = list(cp_callback, callback_terminate_on_naan(), callback_time_stopping(seconds = 39600)),
              validation_data = validation_dataset)

# save model
save_model_tf(model, paste0(outdir, "saved_model/my_model"))




## Evaluation
checkpoint_dir <- paste0(outdir, "checkpoints/")
load(paste0(outdir, "test_img.RData"))
load(paste0(outdir, "test_ref.RData"))

# convert to tibble
test_data = tibble(img = test_img, ref = test_ref)

# prepare dataset that can be input to the CNN
test_dataset <- create_dataset(test_data, train = FALSE, batch = 1, shuffle = FALSE, useDSM = FALSE)

# load model (use meaningful "modelname.hdf5" file)
model = load_model_hdf5(paste0(checkpoint_dir, "modelname.hdf5"), compile = TRUE) 

# evaluate test dataset
eval <- evaluate(object = model, x = test_dataset)
eval

# make predictions
test_pred = predict(model, test_dataset)

# save predictions
test_pred_df <- as.data.frame(test_pred)
test_pred_df <- test_pred_df$V1

# allocate image names and reference data to predictions of test dataset for succeeding analysis
data_full <- cbind(as.character(test_img), test_ref, test_pred_df)
write.csv(data_full, paste0(outdir, "Test_results.csv"))