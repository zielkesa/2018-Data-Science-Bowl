#A collection of functions that I will be using a lot in various places

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

#returns the encoded pixels given the list of them
#Takes: character
#Returns: numeric
getEncodedPixels <- function(pixels){
  pixels <- pixels %>% strsplit(" ") %>% unlist %>% as.numeric
  pixels.df <- data.frame(pixel = pixels[seq(1, length(pixels), 2)], 
                        number = pixels[seq(2,length(pixels),2)])
  #turning data.frame into a vector of all the pixels
  pixels.vec <- numeric()
  for (i in 1:nrow(pixels.df)){
    for (j in 0:(pixels.df$number[i]-1)){
      pixels.vec <- c(pixels.vec, (pixels.df$pixel[i] + j))
    }#for j
  }#for i
  pixels.vec
}#getEncodedPixels

#Creates a mask based on a list of pixels
#Takes: vector
#Returns: matrix
createMask <- function(pixels){
  mask <- array(0, dim = c(256,256,3))
  for (i in 1:length(pixels)){
    mask[pixels[i] %% 256, pixels[i] %/% 256,2] <- 1
  }#for
  mask
}#createMask

#plots the image in the viewer
#Takes: array
plotArray <- function(img){
  xy <- max(dim(img))
  x <- dim(img)[1]
  y <- dim(img)[2]
  plot(1:xy,1:xy, type = "n")
  rasterImage(img, 1, 1, x, y)
}#plotArray

#checks if an image is a colour image or black and white
#Takes: array
#Return: logical
isColour <- function(img){
  if (img[1,1,1] == img[1,1,2] &&
      img[25,25,2] == img[25,25,3] &&
      img[50,50,1] == img[50,50,3]){
    return(FALSE)
  }#if
  TRUE
}#isColour

#makes a colour picture black and white
#Takes: array
#Returns: array
makeBW <- function(img){
  img.bw <- array(0, dim(img)[1:2])
  for (i in 1:dim(img)[1]){
    for (j in 1:dim(img)[2]){
      img.bw[i,j] <- 1 - mean(img[i,j,1:3])
    }#for j
  }#for i
  img.bw
}#makeBW


