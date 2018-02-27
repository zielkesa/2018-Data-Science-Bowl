#Submission 3
#Similar algorithm to sub1, but will be using the classes
#created in the classifyTestImages.R file.
#Submitted score: 0.231

library(dbscan)
library(dplyr)

source("imageHandling.R")
source("imageIO.R")

submission.name <- "submission3"

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

#writes out intermediate data
#Takes: character, data.frame
writeIntermediate <- function(image.id, img.df){
  write.csv(img.df, file = paste("processing/", submission.name, 
                                 "/", image.id, ".csv", sep = ""), 
            row.names = FALSE, quote = FALSE)
  img2 <- dfPredictionToImage(img.df)
  writePNG(img2, target = paste("processing/", submission.name, 
                                "/", image.id, ".png", sep = ""))
}#writeIntermediate

#removes clusters smaller than 75 pixels
#Takes: numeric
#Returns: numeric
rmSmallClusters <- function(clusters){
  new.index <- 1
  new.clusters <- rep(0, length(clusters))
  for (i in 1:max(clusters)){
    if (sum(clusters == i) > 75) {
      new.clusters[clusters == i] <- new.index
      new.index <- new.index + 1
    }#if
  }#for
  new.clusters
}#rmSmallClusters

#runs the dbscan and returns a full data frame
#Takes: data.frame, numeric
#Returns: data.frame
runDBSCAN <- function(img.df, kindex){
  dbscan.df <- cbind(img.df[img.df$kmeans == kindex,c("x", "y")],
                     cluster = dbscan(img.df[img.df$kmeans == kindex,], 
                                      eps = 2, minPts = 3)$cluster)
  img.df <- left_join(img.df, dbscan.df, by = c("x", "y"))
  img.df$cluster[is.na(img.df$cluster)] <- 0
  img.df
}#runDBSCAN

#running class c1
#Takes: character
#returns: data.frame
runC1 <- function(image.id){
  img <- readTestImage(image.id) %>% makeBW()
  plotArray(img)
  img.df <- imageToDataFrame(img)
  img.df$kmeans <- kmeans(img.df$value, 3)$cluster
  m.values <- data.frame(k = 1:3,
                         kmean = c(mean1 <- mean(img.df$value[img.df$kmeans == 1]),
                                  mean2 <- mean(img.df$value[img.df$kmeans == 2]),
                                  mean3 <- mean(img.df$value[img.df$kmeans == 3]))) %>%
    arrange(-kmean)
  img.df <- runDBSCAN(img.df[,1:5], m.values$k[1])
  img.df$cluster <- rmSmallClusters(img.df$cluster)
  writeIntermediate(image.id, img.df)
  img.df
}#runC1

#running the C2 class
#Takes: character
#Returns: data.frame
runC2 <- function(image.id){
  img <- readTestImage(image.id) %>% makeBW()
  plotArray(img)
  img.df <- imageToDataFrame(img)
  img.df$kmeans <- kmeans(img.df$value, 2)$cluster
  if (mean(img.df$value[img.df$kmeans == 1]) > mean(img.df$value[img.df$kmeans == 2])){
    img.df <- runDBSCAN(img.df, 1)
  } else {img.df <- runDBSCAN(img.df, 2)}
  img.df$cluster <- rmSmallClusters(img.df$cluster)
  writeIntermediate(image.id, img.df)
  img.df
}#runOther

#running the BW class
#Takes: character
#Returns: data.frame
runBW <- function(image.id){
  img <- readTestImage(image.id)
  plotArray(img)
  img.df <- imageToDataFrame(img[,,1])
  img.df$kmeans <- kmeans(img.df$value, 2)$cluster
  if (mean(img.df$value[img.df$kmeans == 1]) > mean(img.df$value[img.df$kmeans == 2])){
    img.df <- runDBSCAN(img.df, 1)
  } else {img.df <- runDBSCAN(img.df, 2)}
  img.df$cluster <- rmSmallClusters(img.df$cluster)
  writeIntermediate(image.id, img.df)
  img.df
}#runOther




sub.df <- data.frame()
#image.ids <- list.dirs("raw/test/", full.names = FALSE, recursive = FALSE)
image.ids <- read.csv("processing/classifyImages/testClasses1.csv", stringsAsFactors = FALSE)
#image.ids <- "259b35151d4a7a5ffdd7ab7f171b142db8cfe40beeee67277fac6adca4d042c4" #change

#MAIN LOOP

for (i in 58:nrow(image.ids)){
  img.df <- data.frame()
  if (image.ids$type[i] == "c1") {img.df <- runC1(image.ids$ImageId[i])
  } else if (image.ids$type[i] == "c2") {img.df <- runC2(image.ids$ImageId[i])
  } else {img.df <- runBW(image.ids$ImageId[i])}
  
  rle.strings <- convertToRLE(img.df$cluster)
  temp <- data.frame(ImageId = image.ids$ImageId[i],
                     EncodedPixels = rle.strings)
  sub.df <- rbind(sub.df, temp)
  print(paste(i, "/", length(image.ids), sep = ""))
  write.csv(sub.df, file = paste("submissions/", submission.name, 
                               ".csv", sep = ""), row.names = FALSE,
          quote = FALSE, append = TRUE)
}#for





