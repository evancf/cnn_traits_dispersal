##### in this script, the CNN models are trained
##### input is the metadata manually created to classify the photos we want 
##### train the model to classify all the images with fruits and seeds recorded
##### the output metadata will serve as the input to the CNN models in predicting seed dispersal mode 



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
path_ref = "/feature_extraction_batch_1.csv"
outdir =  "~/image/feature_output/"
logdir = "logs/"

xres = 512
# convert to integer
xres = as.integer(xres)
yres = 512
no_bands = 3

### set working directory and output directory
setwd(workdir)
dir.create(paste0(outdir), recursive = FALSE)
dir.create(paste0(outdir, logdir), recursive = FALSE)

### read data

## reference from csv
ref <- read.csv(paste0(workdir, path_ref), header = T, sep = ",")

## list all img  data
path_img = list.files(paste0(workdir, path_img), full.names = T, pattern = "jpg")

# combine image data with target value data
dat <- cbind(path_img, ref)

# # choose image data with annotation information
# dat <- dat[complete.cases(dat),]

### remove outliers
# dat$ref <- log10(dat$ref)
# outl <- which.outlier(dat$ref, thr = 3, method = "sd")
# dat <- dat[-outl,]

# drop NA in contains_fruits_seeds
dat <- dat[!is.na(dat$contains_fruits_seeds),]

### split test dataset from training/validation dataset
species_test <- sample(unique(dat$species), size = floor(length(unique(dat$species))/10), replace = F)
test_dat <- dat[dat$species %in% species_test, ]
test_img <- dat$path_img[dat$species %in% species_test]
test_ref <- dat$biotic[dat$species %in% species_test]
dat <- dat[!(dat$species %in% species_test), ]

# split training and validation data
species_val <- sample(unique(dat$species), size = floor(length(unique(dat$species))/5), replace = F)
val_dat <- dat[dat$species %in% species_val, ]
val_img <- dat$path_img[dat$species %in% species_val]
val_ref <- dat$biotic[dat$species %in% species_val]
train_dat <- dat[!(dat$species %in% species_val), ]
# train_dat contains the remaining training dataset

### prepare training datasets
train_img = train_dat$path_img


### prepare training reference values
train_ref = train_dat$contains_fruits_seeds
# range01_v2(train_dat$ref, log10(min_ref), log10(max_ref))

### prepare tibble
train_data = tibble(img = train_img, ref = train_ref)
val_data = tibble(img = val_img, ref = val_ref)

# save training data to disk
save(train_ref, file = paste0(outdir, "train_ref.RData"), overwrite = T)
save(train_img, file = paste0(outdir, "train_img.RData"), overwrite = T)
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
batch_size <- 64
epochs <- 50
dataset_size <- length(train_data$img)

### prepare dataset that can be input to CNN
training_dataset <- create_dataset(train_data, train = TRUE, batch = batch_size, epochs = epochs, dataset_size = dataset_size)
validation_dataset <- create_dataset(val_data, train = FALSE, batch = batch_size, epochs = epochs)

# save dataset for future training
save(training_dataset, file = paste0(outdir, "training_dataset.RData"), overwrite = T)
save(validation_dataset, file = paste0(outdir, "validation_dataset.RData"), overwrite = T)

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
  
  # import base model from downloaded inception_resnet_v2
  # weights_path <- 'inception_resnet_v2_weights_tf_dim_ordering_tf_kernels_notop.h5'
  # base_model <- application_inception_resnet_v2(weights = weights_path, include_top = FALSE, input_shape = c(xres, yres, no_bands) )
  # import base model from downloaded ResNet V2 152
  # base_model <- application_resnet152_v2(weights = 'imagenet', include_top = FALSE, input_shape = c(xres, yres, no_bands) )
  weights_path <- 'resnet152v2_weights_tf_dim_ordering_tf_kernels_notop.h5'
  base_model <- tf$keras$applications$resnet_v2$ResNet152V2(weights = weights_path, include_top = FALSE, input_shape = c(as.integer(xres), as.integer(yres), as.integer(no_bands))) 

  # base CNN model definition, initial weights should be downloaded automatically from www.image-net.org upon compiling
  #base_model <- application_inception_resnet_v2(weights = 'imagenet',
  # include_top = FALSE, input_shape = c(xres, yres, no_bands) )
  # base_model <- application_xception(weights = 'imagenet',
  #                               include_top = FALSE, input_shape = c(xres, yres, no_bands))
  # base_model <- application_mobilenet_v2(weights = 'imagenet', alpha = 0.5,
  #                          include_top = FALSE, input_shape = c(xres, yres, no_bands))

  # freeze base model
  base_model$trainable <- FALSE

  # summarize base model
  summary(base_model)

  
  # add custom layers as regressor
  predictions <- base_model$output %>%
    layer_average_pooling_2d(pool_size = c(16, 16)) %>% 
    layer_flatten() %>%
    layer_dense(units = 512, activation = 'relu') %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1, activation = 'sigmoid') 
  
  # set up the model
  model <- keras_model(inputs = base_model$input, outputs = predictions)
  
  # set up learning rate schedule
  #lr_schedule = keras.optimizers.schedules.ExponentialDecay(
  #initial_learning_rate=0.001,
  # decay_steps= 10000,
  #decay_rate= 0.0001)
  
  # tensorboard setup
  tensorboard(paste0(outdir, logdir, "fit_features"))

  # compile model
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = tf$keras$optimizers$RMSprop(learning_rate=0.0001),
    # optimizer = optimizer_rmsprop(),
    metrics = c("accuracy")
  )
})

# summarize model
summary(model)



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
              callbacks = list(cp_callback, callback_terminate_on_naan(), callback_tensorboard(paste0(outdir, logdir, "fit_features"))),
              # callback_time_stopping(seconds = 39600)
              validation_data = validation_dataset)

# save model
save_model_tf(model, paste0(outdir, "saved_model/my_model"))