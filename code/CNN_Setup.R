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
outdir =  "~/image/output/"

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

### remove outliers
# dat$ref <- log10(dat$ref)
# outl <- which.outlier(dat$ref, thr = 3, method = "sd")
# dat <- dat[-outl,]

### split test dataset from training/validation dataset
testIdx <- sample(x = 1:nrow(dat), size = floor(nrow(dat)/10), replace = F)
test_dat <- dat[testIdx, ]
test_img <- dat$path_img[testIdx]
test_ref <- dat$biotic[testIdx]
dat <- dat[-testIdx, ]

### split training and validation data
valIdx <- sample(x = 1:nrow(dat), size = floor(nrow(dat)/5), replace = F)
val_dat <- dat[valIdx, ]
val_img <- dat$path_img[valIdx]
val_ref <- dat$biotic[valIdx]
train_dat <- dat[-valIdx, ]
# train_dat contains the remaining training dataset

### prepare training datasets
train_img = train_dat$path_img


### prepare training reference values
train_ref = train_dat$biotic
# range01_v2(train_dat$ref, log10(min_ref), log10(max_ref))

### prepare tibble
train_data = tibble(img = train_img, ref = train_ref)
val_data = tibble(img = val_img, ref = val_ref)


# save test data to disk
save(test_img, file = paste0(outdir, "test_img.RData"), overwrite = T)
save(test_ref, file = paste0(outdir, "test_ref.RData"), overwrite = T)




### tfdatasets input pipeline 
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



### set parameters 
batch_size <- 20 
epochs <- 50
dataset_size <- length(train_data$img)

### prepare dataset that can be input to CNN
training_dataset <- create_dataset(train_data, train = TRUE, batch = batch_size, epochs = epochs, dataset_size = dataset_size)
validation_dataset <- create_dataset(val_data, train = FALSE, batch = batch_size, epochs = epochs)

# save dataset for future training
save(training_dataset, file = paste0(outdir, "training_dataset.RData"), overwrite = T)
save(validation_dataset, file = paste0(outdir, "validation_dataset"), overwrite = T)

### check dataset loading
dataset_iter = reticulate::as_iterator(training_dataset)
train_example = dataset_iter %>% reticulate::iter_next()
train_example[[1]]
train_example[[2]]

dataset_iter = reticulate::as_iterator(validation_dataset)
val_example = dataset_iter %>% reticulate::iter_next()
val_example


### choose one of three pre-defined base model architectures: Inception-Resnet-v2, Xception, MobileNet-v2
### Inception-Resnet-v2 was used for Baseline and TA in the study
with(strategy$scope(), {
  
  # import base mnodel from downloaded inception_resnet_v2
  weights_path <- 'inception_resnet_v2_weights_tf_dim_ordering_tf_kernels_notop.h5'
  base_model <- application_inception_resnet_v2(weights = weights_path, include_top = FALSE, input_shape = c(xres, yres, no_bands) )
  # base CNN model definition, initial weights should be downloaded automatically from www.image-net.org upon compiling
  #base_model <- application_inception_resnet_v2(weights = 'imagenet',
                                                # include_top = FALSE, input_shape = c(xres, yres, no_bands) )
  # base_model <- application_xception(weights = 'imagenet',
  #                               include_top = FALSE, input_shape = c(xres, yres, no_bands))
  # base_model <- application_mobilenet_v2(weights = 'imagenet', alpha = 0.5,
  #                          include_top = FALSE, input_shape = c(xres, yres, no_bands))
  
  # add custom layers as regressor
  predictions <- base_model$output %>%
    layer_global_average_pooling_2d() %>%
    layer_dense(units = 512, activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear') 
  
  # set up the model
  model <- keras_model(inputs = base_model$input, outputs = predictions)
  
  # set up learning rate schedule
  #lr_schedule = keras.optimizers.schedules.ExponentialDecay(
  #initial_learning_rate=0.001,
  # decay_steps= 10000,
  #decay_rate= 0.0001)
  
  # compile model
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = c("mean_absolute_error")
  )
})



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

model %>% fit(x = training_dataset,
              epochs = epochs,
              steps_per_epoch = floor(length(train_data$img)/batch_size), 
              callbacks = list(cp_callback, callback_terminate_on_naan()),
              # callback_time_stopping(seconds = 39600)
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

