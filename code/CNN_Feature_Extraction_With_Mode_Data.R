##### in this script, the CNN models are trained
##### input is the metadata manually created to classify the photos we want , along with the seed dispersal mode data
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

# drop NA in contains_fruits_seeds
dat <- dat[!is.na(dat$contains_fruits_seeds),]

# Include aux data as input

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

### prepare training datasets
train_img = train_dat$path_img

### prepare training reference values
train_ref = train_dat$biotic
# range01_v2(train_dat$ref, log10(min_ref), log10(max_ref))

# include auxiliary data
train_aux <- subset(train_dat, select = c('endo','wind','cache','water','ant','attach','ballistic','biotic'))
test_aux <- subset(test_dat, select = c('endo','wind','cache','water','ant','attach','ballistic','biotic'))
val_aux <- subset(val_dat, select = c('endo','wind','cache','water','ant','attach','ballistic','biotic'))

### prepare tibble
train_data = tibble(img = train_img, ref = train_ref, aux = train_aux)
val_data = tibble(img = val_img, ref = val_ref, aux = val_aux)


# save test data to disk
save(test_img, file = paste0(outdir, "test_img.RData"), overwrite = T)
save(test_ref, file = paste0(outdir, "test_ref.RData"), overwrite = T)
save(test_aux, file = paste0(outdir, "test_aux.RData"), overwrite = T)

####################################################################################
##### use the following code as input pipeline of Bioclim setup ###################
####################################################################################




# inspired by: https://blogs.rstudio.com/ai/posts/2019-08-23-unet/
# and: https://blogs.rstudio.com/ai/posts/2019-10-08-tf2-whatchanges/
# and: https://tensorflow.rstudio.com/tutorials/beginners/load/load_image/
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
  
  
  # this is just the transformation from image paths to actual images that will be fed to the model
  dataset = dataset %>%
    dataset_map(~.x %>% list_modify( # read files and decode jpeg
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
      )) %>%
      dataset_map(~.x %>% list_modify( # randomly flip left/right
        img = tf$image$random_flip_left_right(.x$img, seed = 1L)
      )) %>%
      dataset_map(~.x %>% list_modify( # randomly assign brightness, contrast and saturation to images
        img = tf$image$random_brightness(.x$img, max_delta = 0.1, seed = 1L) %>%
          tf$image$random_contrast(lower = 0.9, upper = 1.1, seed = 2L) %>%
          tf$image$random_saturation(lower = 0.9, upper = 1.1, seed = 3L) %>% # requires 3 chnl -> with useDSM chnl = 4
          tf$clip_by_value(0, 1) # clip the values into [0,1] range.
      ))#  %>%
  }

  dataset = dataset %>%
    dataset_prepare(x = c(img, aux), y = ref, named_features = TRUE, batch_size = batch, drop_remainder = TRUE) %>% 
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
# save(training_dataset, file = paste0(outdir, "training_dataset.RData"), overwrite = T)
# save(validation_dataset, file = paste0(outdir, "validation_dataset.RData"), overwrite = T)

### check dataset loading
dataset_iter = reticulate::as_iterator(training_dataset)
train_example = dataset_iter %>% reticulate::iter_next()
train_example[[1]]
train_example[[2]]

dataset_iter = reticulate::as_iterator(validation_dataset)
val_example = dataset_iter %>% reticulate::iter_next()
val_example

#################################################
##### define model for including Seed Dispersal Mode ##########
#################################################

### choose one of three pre-defined base model architectures: ResNet152V2
### Set up model with the Functional API

with(strategy$scope(), {
  
  ### input branch A:
  input_cnn <- layer_input(shape = c(xres, yres, no_bands))

  weights_path <- 'inception_resnet_v2_weights_tf_dim_ordering_tf_kernels_notop.h5'
  base_cnn <- input_cnn %>%
    application_inception_resnet_v2(weights = weights_path, include_top = FALSE, input_shape = c(xres, yres, no_bands) )

  #weights_path <- 'resnet152v2_weights_tf_dim_ordering_tf_kernels_notop.h5'
  ### output branch A:
  #base_cnn <- input_cnn %>%
  #  tf$keras$applications$ResNet152V2(weights = weights_path, input_shape = c(as.integer(xres), as.integer(yres), as.integer(no_bands)))
  # base_cnn <- tf$keras$applications$resnet_v2$ResNet152V2(weights = weights_path, include_top = FALSE, input_shape = c(as.integer(xres), as.integer(yres), as.integer(no_bands))) 
    # application_inception_resnet_v2(weights = 'imagenet',
    #                         include_top = FALSE, input_shape = c(xres, yres, no_bands))
    # application_xception(weights = 'imagenet',
    #                               include_top = FALSE, input_shape = c(xres, yres, no_bands))
    # application_mobilenet_v2(weights = 'imagenet', alpha = 0.5,
    #                          include_top = FALSE, input_shape = c(xres, yres, no_bands))
  
  # base_cnn$trainable <- FALSE

  summary(base_cnn)

  output_cnn <- base_cnn$output %>% 
    layer_average_pooling_2d(pool_size = c(14, 14)) %>% 
    layer_flatten() %>%
    layer_dense(units = 512, activation = 'relu') %>%
    layer_dense(units = 4, activation = 'linear') 
  
  ### input branch B:
  input_dense <- layer_input(shape = c(NA), name = "aux")

  # output of branch B
  output_dense <- input_dense %>%
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dense(units = 32, activation = 'relu') %>%
    layer_dense(units = 4, activation = 'linear') 
  
  
  # concatenated branch
  main_output <- layer_concatenate(c(output_cnn, output_dense)) %>%
    layer_dense(units = 8, activation = 'relu') %>%
    layer_dense(units = 8, activation = 'relu') %>%
    layer_dense(units = 4, activation = 'relu') %>%
    layer_dense(units = 1, activation = 'sigmoid') 
  
  # set up complete model
  model <- keras_model(
    inputs = list(input_cnn, input_dense),
    outputs = list(main_output)
  )
  
  
  # compile model
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = tf$keras$optimizers$RMSprop(learning_rate=0.0001),
    # optimizer = optimizer_rmsprop(),
    metrics = c("accuracy")
  )

  # tensorboard setup
  tensorboard <- tf$keras$callbacks$TensorBoard(log_dir = paste0(outdir, logdir, "fit_features"), histogram_freq = 0, 
                                                write_graph = TRUE, write_grads = TRUE, batch_size = batch_size)
  tensorboard$set_model(model)

})

train_steps <- floor(length(train_data$img) / batch_size) # number of batches the model is trained upon per epoch
val_steps <- floor(length(val_data$img) / batch_size) # number of batches fed to model for validation

# initialize empty list to populate in succeeding loop:
summary_list.names <- c('train_loss', 'train_acc', 'val_loss', 'val_acc')
summary_list <- vector("list", length(summary_list.names))
names(summary_list) <- summary_list.names

###############
## fit model ##
###############
for (t in 1:epochs){ # for-loop that loops through all training and validation steps
  
  # initiate new set of batches
  iter_train <- make_iterator_one_shot(training_dataset)
  next_batch_train <- iterator_get_next(iter_train)
  iter_val <- make_iterator_one_shot(validation_dataset)
  next_batch_val <- iterator_get_next(iter_val)
  
  mean_train = list()
  for (i in 1:train_steps) { # loop is gone through 'steps' times
    
    # training
    history <- model %>% fit(x = list(next_batch_train$x$img, next_batch_train$x$aux), 
                                        y = next_batch_train$y)
    
    if (i %% 50 == 0){ # every 50 steps, print some information
      print('training step:')
      print(i)
      print('train_loss, train_accuracy')
      print(history)
    }

    mean_train$loss[[i]] <- history[1]
    mean_train$acc[[i]] <- history[2]
    
    # if iterator_get_next is called in last step, error will be thrown
    # this is avoided by this if-condition
    if (i < train_steps)
    {
    # create new batch of training data:
    next_batch_train <- iterator_get_next(iter_train)
    } 
    
  }
  
  print(paste('average train_loss and train_accuracy of epoch', t))
  print(mean(unlist(mean_train$loss)))
  print(mean(unlist(mean_train$acc)))
  
  mean_val = list() 
  for (k in 1:val_steps){
    # check validation loss:
    history <- model %>% test_on_batch(x = list(next_batch_val$x$img, next_batch_val$x$aux), 
                                       y = next_batch_val$y)
    
    if (k %% 20 == 0){ # every 30 steps, print some information
      print('validation step:')
      print(k)
      print('val_loss (mse), val_mean_absolute_error')
      print(history)
    }
    
    mean_val$loss[[k]] <- history[1]
    mean_val$acc[[k]] <- history[2]
    
    # if iterator_get_next is called in last step, error will be thrown
    # this is avoided by this if-condition
    if (k < val_steps)
    {
    # create new batch of validation data:
    next_batch_val <- iterator_get_next(iter_val)
    }
  }
  
  print(paste('average val_loss and val_mean_accuracy of epoch', t))
  print(mean(unlist(mean_val$loss)))
  print(mean(unlist(mean_val$acc)))

  
  # save history
  summary_list$train_loss[[t]] <- mean(unlist(mean_train$loss))
  summary_list$train_acc[[t]] <- mean(unlist(mean_train$acc))
  summary_list$val_loss[[t]] <- mean(unlist(mean_val$loss))
  summary_list$val_acc[[t]] <- mean(unlist(mean_val$acc))
  
  if (t == 1){
    # save model of first epoch
    save_model_hdf5(model,
                    paste0(workdir,outdir, "checkpoints/", "weights.epoch_", t, "-val_loss_", round(summary_list$val_loss[[t]], 4), ".hdf5"),
                    overwrite = TRUE)
    print(paste('Model of epoch', t, 'saved!'))
  }
  else{
    if (summary_list$val_loss[[t]] < summary_list$val_loss[[t-1]]){
      # save model of epoch in case the validation loss has been improved compared to preceding epoch
      save_model_hdf5(model,
                      paste0(workdir,outdir, "checkpoints/", "weights.epoch_", t, "-val_loss_", round(summary_list$val_loss[[t]], 4), ".hdf5"),
                      overwrite = TRUE) # , custom_objects=list("loss_function" = RMSE)
      print(paste('Model of epoch', t, 'saved!', 'val_acc is', round(summary_list$val_acc[[t]], 4)))
    }
  }

  # log to tensorboard
  tensorboard$on_epoch_end(epoch = t, logs = list(train_loss = summary_list$train_loss[[t]], 
                                                 train_acc = summary_list$train_acc[[t]], 
                                                 val_loss = summary_list$val_loss[[t]], 
                                                 val_acc = summary_list$val_acc[[t]]))
  
  # end of loop that denotes epochs
}

tensorboard$on_train_end(logs = NULL)


### plot results
# summary_df <- as.data.frame(summary_list)
# min(summary_df$val_acc)
# summary_df

write.csv(summary_df, paste0(workdir, outdir, "History.csv"))
