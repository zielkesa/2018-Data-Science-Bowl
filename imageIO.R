#functions for reading images and information

library(magrittr)
library(png)

#reads an image given the image id 
#Takes: character
#Returns: array
readImage <- function(image.id){
  img <- readPNG(paste("raw/train/", 
                       image.id, 
                       "/images/", 
                       image.id, 
                       ".png", 
                       sep = ""))
  img
}#readImage

#reads a test image given the image id 
#Takes: character
#Returns: array
readTestImage <- function(image.id){
  img <- readPNG(paste("raw/test/", 
                       image.id, 
                       "/images/", 
                       image.id, 
                       ".png", 
                       sep = ""))
  img
}#readImage

#Reads in all the masks for an image
#Takes: character, numeric, numeric
#Returns: array
readMasks <- function(image.id, x, y){
  path <- paste("raw/train/", image.id, "/masks/", 
                "/", sep = "")
  mask.id <- list.files(path)
  masks <- array(0, dim = c(x, y, length(mask.id)))
  for (i in 1:length(mask.id)){
    img <- readPNG(paste(path, mask.id[i], sep = ""))
    masks[,,i] <- img
  }#for
  masks
}#readMasks

#reads the training labels
#Returns: data.frame
getTrainingLabels <- function(){
  label.train <- read.csv("raw/stage1_train_labels.csv",
                          stringsAsFactors = FALSE)
  label.train
}#getTrainingLables