#Submission 2
#This is a rework of submission 1. I may have entered the data in the wrong way.
#Score: 0.215

submission.name <- "submission2"

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

sub.df <- data.frame()
#MAIN LOOP
for (i in 1:length(image.ids)){
  print(paste(i, "/", length(image.ids), sep = ""))
  img.df <- read.csv(paste("processing/submission1/", image.ids[i],
                           ".csv", sep = ""), stringsAsFactors = FALSE) %>%
    arrange(y, x)
  sub.df <- rbind(sub.df,
                   data.frame(ImageId = image.ids[i],
                              EncodedPixels = convertToRLE(img.df$cluster)))
}#for

write.csv(sub.df, file = paste("submissions/", submission.name, 
                                ".csv", sep = ""), row.names = FALSE,
          quote = FALSE)
