##### in this script, images referenced by url's are downloaded for the trait dataset
##### input is a dataframe (produced in script "4_sample_trait_dataset.R"), 
##### output is a folder with all the images as .jpg files and corresponding dataframe containing the metadata


### load libraries
require(data.table)
require(stringr)

### set paths and parameters
workdir <- "~/data/GBIF_data/"
outputfolder <- "~/image/Species_Pics/"

### read data
setwd(workdir)

dir.create(outputfolder)

# replace with any other trait sample
dispersal_mode <- fread("seed_dispersal_mode_rdy_for_download.txt", header = TRUE, sep = ",", dec = ".", quote = "", data.table = T)
dispersal_mode <- as.data.frame(dispersal_mode)

# initialise column "pic_name" to reference the name of the .jpg-file in the metadata
dispersal_mode$pic_name <- matrix(NA, nrow(dispersal_mode), 1)

### Suppress extraneous warning messages
oldw <- getOption("warn")
options(warn = -1)
options(timeout=100)

### Close the connection when done
### download the data     nrow(dispersal_mode)
for (i in (1:nrow(dispersal_mode)))
{
  ## IF function
  # assign unique picture name
  skip_to_next <- FALSE
  download_url <- NA
  thePage <- NA
  closeAllConnections()
  dispersal_mode$pic_name[i] <- paste("SDM_", sprintf("%06d", i), ".jpg", sep="")
  # check if image already exists
  skip_to_next <<- file.exists(paste(outputfolder, "/SDM_", sprintf("%06d", i), ".jpg", sep=""))
  if(skip_to_next) {
    print(paste(i, 'Image already exists'))
    next
  }
  # access inaturalist web page
  page_url <- paste("https://www.inaturalist.org/observations/", as.character(dispersal_mode$identifier[i]), sep="")
  # processing url
  ## function(e){
  ## download_url_improved <- “whatever”
  ## print(“inat link did not work”)
  ## }
  tryCatch({
    page_url <- url(page_url, "rb")
    thePage <- readLines(page_url)
    close(page_url)
    }, error = function(x) {
    print(paste(i, x))
    skip_to_next <<- TRUE
    })
  if(skip_to_next) next
  tryCatch({
    tempRow <- grep('large.jp',thePage)
    tempRow <- tempRow[1]
    download_url <- sapply(strsplit(thePage[tempRow], "\""), "[", 2)
  }, error = function(y){
    print(paste(i, y))
    skip_to_next <<- TRUE
  })
  if(skip_to_next) next
  print(paste("[", i, "]", download_url, sep=""))
  tryCatch({
    download.file(download_url, destfile = paste(outputfolder, "/SDM_", sprintf("%06d", i), ".jpg", sep=""), mode = "wb")
    while (!file.exists(paste(outputfolder, "/SDM_", sprintf("%06d", i), ".jpg", sep=""))) {
      Sys.sleep(1)
    }
    closeAllConnections()
    }, 
           error = function(e) print(paste(i, e)))
}

### Restore settings
options(warn = oldw)

# remove observations with failed downloads (= missing image in the image folder)
fls <- list.files(outputfolder, pattern = ".jpg")
dispersal_mode <- dispersal_mode[as.character(dispersal_mode$pic_name) %in% fls,]

# write to disk
fwrite(dispersal_mode, file = paste0("~/image/","metadata.txt"), col.names = TRUE)
# continue with script "6_image_processing.R"


## Search Webpage
## webpage <- xml2::read_html(download_url)
## ExOffndrsRaw <- rvest::html_table(webpage)[[1]] %>% 
## tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
## ExOffndrsRaw %>% dplyr::glimpse(45)

