##### in this script, the images in a folder a processed
##### that is, images are clipped to quadratic shape by removing spare margins of the longer edge, 
##### and resampled to a resolution of 512 x 512 pixels
##### input are the images produced by script "5_download_images.R" and their metadata

### load libraries
library(sp)
library(raster)
library(rgdal)
library(doParallel)
library(foreach)
library(abind)
library(berryFunctions)
library(jpeg)
library(data.table)



### set paths
workdir <- "/pool001/hardyxu"
path_meta <- "/Species_Pics"

### read data
setwd(workdir)

## read all images
all_pics <- list.files(path = paste0(workdir, path_meta), pattern = ".jpg")


### set parameters
pixel_size = 512 # x-y pixel size used for creating tiles
no_cores = 16 # how many cores for multicore processing?
dummy_rast <- raster(matrix(data = NA, ncol = pixel_size, nrow = pixel_size)) # used for resampling (see below)
outputfolder = "/SSD_test/" # folder to which the resulting images will be saved
unlink(substr(outputfolder, 0, nchar(outputfolder)-1), recursive = TRUE)
dir.create(paste(workdir, outputfolder, sep =""))

metadata <- fread(paste0(workdir, "/", "metadata.txt"), header = TRUE, sep = ",", dec = ".", quote = "", data.table = T)
head(metadata)

# change working directory
## setwd(paste0(workdir, path_meta))


# start parallel processing
cl <- makeCluster(no_cores)
registerDoParallel(cl)



convert_output = list()
convert_output <- foreach(i = 1:length(all_pics), .errorhandling='pass') %dopar% {
  
  library(raster)
  library(rgdal)
  library(berryFunctions)
  
  # Define function quadradize
  quadradize = function(image){ # make image frames quadratic
    if(nrow(image) < ncol(image)){ # width is greater than height
      
      min_col = floor((ncol(image)-nrow(image))/2)
      max_col = ncol(image) - floor((ncol(image)-nrow(image))/2)
      image = crop(image, extent(image, 1, nrow(image) - 1, min_col + 1, max_col - 1))
      
    }else{ # height is greater than width
      
      min_row = floor((nrow(image)-ncol(image))/2)
      max_row = nrow(image) - floor((nrow(image)-ncol(image))/2)
      image = crop(image, extent(image, min_row + 1 , max_row - 1, 1, ncol(rast) - 1))
    }
    return(image)
  }
  
  
  rast = NA # set rast to NA, because if file is corrupt previous raster will not be overwritten
  
  # Error handling
  result <- tryCatch({
    # load image
    suppressWarnings(rast <- stack(paste0(workdir, path_meta, "/", all_pics[i])))
    # remove alpha channel (if available) and ignore images with number of bands != 3 (RGB)
    if(dim(rast)[3] == 4){
      rast = rast[[1:3]]
    }
    if(dim(rast)[3] != 3){
      rast = NA
    }
    
    # rescaling: resulting scale of pixel values is 0-255
    if (!(max(suppressWarnings(getValues(rast))) <= 1 || max(suppressWarnings(getValues(rast))) > 255)) { # if TRUE: max value is either lower than/equal to 1 or higher than 255
      # no action required
    } else {
      # max value is higher than 255
      if (max(suppressWarnings(getValues(rast))) > 255)
      {
        # adjust to 0-255 scale
        rast <- suppressWarnings(rast / max(suppressWarnings(getValues(rast))))
        rast <- suppressWarnings(rast * 255)
      } else { # max value is lower than 1 (implies 0-1 pixel range)
        # adjust to 255 scale
        rast <- suppressWarnings(rast * 255)
      }
    }
  }, warning = function(war) {
    return(paste(i, war))}, 
  error = function(err) {
    return(paste(i, err))}
  ) # END tryCatch
 

  if(length(rast) > 1){ # continue only if rast is not NA
    if(min(dim(rast)[c(1,2)])>= pixel_size){ # continue only if the image has the minimum required pixel size
      # quadradize, clip and resample image:
      rast = suppressWarnings(quadradize(rast))
      extent(rast) = suppressWarnings(extent(dummy_rast))
      rast = suppressWarnings(resample(rast, dummy_rast))

      # save jpeg image
      jpeg(filename=paste0(workdir, outputfolder, all_pics[i]), quality = 100, width = pixel_size, height = pixel_size)
      suppressWarnings(plotRGB(rast))
      dev.off()
    }
  }
  
  return(result)
}
stopCluster(cl)


### remove those entries in metadata table that have been filtered out in the image processing step
### (e.g. due to insufficient resolution)
subfiles <- list.files(path = paste0(workdir, outputfolder), pattern = ".jpg")
metadata <- subset(metadata, pic_name %in% subfiles)
# sort by image name
metadata <- metadata[order(metadata$pic_name), ]

# save metadata file to disk
fwrite(metadata, file = paste0(workdir,"/", "metadata_updated.txt"), col.names = TRUE)

# save error-handling file
## fwrite(convert_output, file = paste0(workdir,"/", "06_error.txt"), col.names = TRUE)
# continue with script "7_train_CNN.R"









