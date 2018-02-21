#functions for handeling images. Things I use a lot


#plots the image in the viewer
#Takes: array
plotArray <- function(img){
  xy <- max(dim(img))
  x <- dim(img)[1]
  y <- dim(img)[2]
  plot(1:xy,1:xy, type = "n")
  rasterImage(img, 1, 1, x, y)
}#plotArray

#Converts an image to a data.frame
#columns: value, x, y, prediction
#Takes: array
#Returns: data.frame
imageToDataFrame <- function(img){
  x.dim <- dim(img)[1]
  y.dim <- dim(img)[2]
  value <- numeric()
  x <- numeric()
  y <- numeric()
  for (i in 1:dim(img)[1]){
    value <- c(value, img[i, 1:y.dim])
    x <- c(x, seq(from = i, to = i, length.out = y.dim))
    y <- c(y, 1:y.dim)
  }#for i
  img.df <- data.frame(value, x, y, prediction = 0)
  img.df
}#imageToDataFrame

#Converts an image to a data.frame
#columns: r, g, b, x, y, prediction
#Takes: array
#Returns: data.frame
imageToDataFrameColour <- function(img){
  x.dim <- dim(img)[1]
  y.dim <- dim(img)[2]
  r <- numeric()
  g <- numeric()
  b <- numeric()
  x <- numeric()
  y <- numeric()
  current.row <- 1
  for (i in 1:dim(img)[1]){
    r <- c(r, img[i, 1:y.dim, 1])
    g <- c(g, img[i, 1:y.dim, 2])
    b <- c(b, img[i, 1:y.dim, 3])
    x <- c(x, seq(from = i, to = i, length.out = y.dim))
    y <- c(y, 1:y.dim)
  }#for i
  img.df <- data.frame(r, g, b, x, y, prediction = 0)
  img.df
}#imageToDataFrameColour

#Turns a data frame into an array
#Takes: data.frame
#Returns: array
dataFrameToImage <- function(img.df){
  img <- array(0, dim = c(max(img.df$x), max(img.dif$y)))
  for (i in 1:nrow(img.df)){
    img[img.df$x[i], img.df$y[i]] <- img.df$value[i]
  }#for
  img
}#dataFrameToImage

#Turns a data frame, with predictions, into an array
#Takes: data.frame
#Returns: array
dfPredictionToImage <- function(img.df){
  img <- array(0, dim = c(max(img.df$x), max(img.df$y)))
  for (i in 1:nrow(img.df)){
    if (img.df[i, ncol(img.df)] != 0){
      img[img.df$x[i], img.df$y[i]] <- img.df[i, ncol(img.df)]
    }#if
  }#for
  img <- img / max(img.df[, ncol(img.df)])
  img
}#dataFrameToImage