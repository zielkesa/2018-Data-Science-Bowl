#classifying the training images into 3 groups

source("imageHandling.R")
source("imageIO.R")

#reads and procesess colour test images
#Takes: array, character
#Returns: vector
procTestCOL <- function(img, imageid){
  val <- c()
  x.dim <- dim(img)[1]
  y.dim <- dim(img)[2]
  for (i in 1:200){
    x <- round(runif(1, 1, x.dim))
    y <- round(runif(1, 1, y.dim))
    val <- c(val, img[x, y, 1:3])
  }#while
  c(imageid, val)
}#proctestCOL

#reads and procesess grayscale test images
#Takes: array, character
#Returns: vector
procTestBW <- function(img, imageid){
  val <- c()
  x.dim <- dim(img)[1]
  y.dim <- dim(img)[2]
  for (i in 1:600){
    x <- round(runif(1, 1, x.dim))
    y <- round(runif(1, 1, y.dim))
    val <- c(val, img[x, y, 1])
  }#while
  val <- sort(val)
  c(imageid, val)
}#proctestCOL

image.ids <- list.dirs("raw/test/", recursive = FALSE, full.names = FALSE)
test.df <- data.frame()
test.df.names <- "ImageId"
for (i in 1:200) {
  test.df.names <- c(test.df.names, 
                     paste("pixelR", i, sep = ""),
                     paste("pixelG", i, sep = ""),
                     paste("pixelB", i, sep = ""))
}

test.bw <- data.frame()
test.bw.names <- "ImageId"
for (i in 1:600) {
  test.bw.names <- c(test.bw.names, 
                     paste("pixel", i, sep = ""))
}

test.col <- data.frame()
test.col.names <- "ImageId"
for (i in 1:200) {
  test.col.names <- c(test.col.names, 
                     paste("pixelR", i, sep = ""),
                     paste("pixelG", i, sep = ""),
                     paste("pixelB", i, sep = ""))
}

#MAIN LOOP
for (i in image.ids){
  img <- readTestImage(i)
  if (!identical(img[,,1], img[,,2])) {
    #temp <- procTestCOL(img, i) %>% t() %>% as.data.frame(stringsAsFactors = FALSE)
    #names(temp) <- test.col.names
    #test.col <- rbind(test.col, temp)
  } else {
    temp <- procTestBW(img, i) %>% t() %>% as.data.frame(stringsAsFactors = FALSE)
    names(temp) <- test.bw.names
    test.bw <- rbind(test.bw, temp)
  }
}#for

write.csv(test.bw, "processing/classifyImages/testBW.csv", row.names = FALSE, quote = FALSE)
write.csv(test.col, "processing/classifyImages/testCOL.csv", row.names = FALSE, quote = FALSE)
test.bw <- read.csv("processing/classifyImages/testBW.csv", stringsAsFactors = FALSE)
test.col <- read.csv("processing/classifyImages/testCOL.csv", stringsAsFactors = FALSE)

kmeans.bw <- kmeans(test.bw[,2:601], 2)
bw1 <- test.bw$ImageId[kmeans.bw$cluster == 1]
bw2 <- test.bw$ImageId[kmeans.bw$cluster == 2]

kmeans.col<- kmeans(test.col[,2:601], 2)
c1 <- test.col$ImageId[kmeans.col$cluster == 1]
c2 <- test.col$ImageId[kmeans.col$cluster == 2]

#making a classification file.
train.type <- data.frame(ImageId = c(bw1, bw2),
                         type = "bw") %>%
  rbind(data.frame(ImageId = c1, type = "c1")) %>%
  rbind(data.frame(ImageId = c2, type = "c2"))
write.csv(train.type, "processing/classifyImages/testClasses1.csv", 
          row.names = FALSE, quote = FALSE)

#Conclusion: I was not able to classify the images as I wanted.
#What I am ending up with is 3 classes. A black and white clas
#(bw) and two colour classes (c1 & c2). This should be good enough 
#for the next submission because I think c1 was causing me a lot
#of problems. In the future I would like to classify the bw class
#into two as well.

#Trying to get good centres for c1
image.id <- "259b35151d4a7a5ffdd7ab7f171b142db8cfe40beeee67277fac6adca4d042c4"
img <- readTestImage(image.id)
img.df <- imageToDataFrameColour(img)
img.df$kmeans <- kmeans(img.df[,1:3], 3)$cluster
img.pred <- dfPredictionToImage(img.df)

plotArray(img.pred)
par(mfrow = c(2,2))
hist(img.df$r)
hist(img.df$r[img.df$kmeans == 1])
hist(img.df$r[img.df$kmeans == 2])
hist(img.df$r[img.df$kmeans == 3])

hist(img.df$g)
hist(img.df$g[img.df$kmeans == 1])
hist(img.df$g[img.df$kmeans == 2])
hist(img.df$g[img.df$kmeans == 3])

hist(img.df$b)
hist(img.df$b[img.df$kmeans == 1])
hist(img.df$b[img.df$kmeans == 2])
hist(img.df$b[img.df$kmeans == 3])

#Conclustion: the darkest ones in the c1 class are the nuclei