#Submission 1
#simple kmeans (2 level) with dbscan separation
#Score: 0.000, placed 1500th at the time
library(dbscan)
library(dplyr)

source("imageHandling.R")
source("imageIO.R")

submission.name <- "submission1"
dir.create("processing/submission1")

#converts the dbscan data to rle format
#Takes: numeric
#Returns: List
convertToRLE <- function(clusters){
  rle.string <- character()
  for (i in 1:max(clusters)){
    rle.temp <- character()
    start.i <- 0
    length.i <- 0
    for (j in 1:max(which(clusters == i))){
      if (start.i == 0 && clusters[j] == i){
        start.i <- j
        length.i <- 1
      } else if (start.i > 0 && clusters[j] == i){
        length.i <- length.i + 1
      } else if (start.i > 0 && clusters[j] != i){
        rle.temp <- paste(rle.temp, start.i, length.i, sep = " ")
        start.i <- 0
        length.i <- 0
      }#else if
    }#for j
    rle.string <- c(rle.string, trimws(rle.temp))
  }#for i
  rle.string
}#convertToRLE


#MAIN LOOP
sub.df <- data.frame()

image.ids <- list.dirs("raw/test/", full.names = FALSE, recursive = FALSE)

for (i in 1:length(image.ids)){
  img <- readTestImage(image.ids[i]) #%>% makeGreyscale
  print(image.ids[i])
  plotArray(img)  
  
  #do kmeans by type of image
  img.kmean <- NULL
  if (sum(img[,,1] == img[,,2]) < dim(img)[1] * dim(img)[2]){
    img.df <- imageToDataFrameColour(img)
    img.kmean <- kmeans(img.df[,1:3], centers = 2)
  } else {
    img <- img[,,1]
    img.df <- imageToDataFrame(img)
    img.kmean <- kmeans(img.df$value, centers = 2)
  }#else

  #Process kmeans
  if (img.kmean$size[2] < img.kmean$size[1]){
    img.df$prediction <- img.kmean$cluster - 1
  } else {
    img.df$prediction <- (img.kmean$cluster - 2) * -1
  }#else
  
  #img2 <- dfPredictionToImage(img.df)
  #Do DBSCAN and add results to img.df
  img.dbscan <- cbind(img.df[img.df$prediction == 1, c("x", "y")],
                      cluster = dbscan(img.df[img.df$prediction == 1, c("x", "y")], 
                                       eps = 2, minPts = 3)$cluster)
  img.df <- left_join(img.df, img.dbscan, by = c("x", "y"))
  img.df$cluster[is.na(img.df$cluster)] <- 0 
  
  #write out intermediate data
  write.csv(img.df, file = paste("processing/", submission.name, 
                                 "/", image.ids[i], ".csv", sep = ""), 
            row.names = FALSE, quote = FALSE)
  img2 <- dfPredictionToImage(img.df)
  writePNG(img2, target = paste("processing/", submission.name, 
                              "/", image.ids[i], ".png", sep = ""))
  
  #Convect to rle format
  rle.strings <- convertToRLE(img.df$cluster)
  temp <- data.frame(ImageId = image.ids[i],
                     EncodedPixels = rle.strings)
  sub.df <- rbind(sub.df, temp)
  print(paste(i, "/", length(image.ids), sep = ""))
}#for

write.csv(sub.df, file = paste("submissions/", submission.name, 
                               ".csv", sep = ""), row.names = FALSE,
                               quote = FALSE)

new.sub <- data.frame()
#re-running rle converstion due to overlaps
for (i in 1:length(image.ids)){
  print(paste(i, "/", length(image.ids), sep = ""))
  img.df <- read.csv(paste("processing/submission1/", image.ids[i],
                           ".csv", sep = ""), stringsAsFactors = FALSE)
  new.sub <- rbind(new.sub,
                   data.frame(ImageId = image.ids[i],
                              EncodedPixels = convertToRLE(img.df$cluster)))
}#for

write.csv(new.sub, file = paste("submissions/", submission.name, 
                               ".csv", sep = ""), row.names = FALSE,
          quote = FALSE)



