#General proccesing script


#making a list of all the image dimensions
label.train <- read.csv("raw/stage1_train_labels.csv",
                        stringsAsFactors = FALSE)

image.id <- unique(label.train$ImageId)
img.sizes <- c()
for (i in 1:length(image.id)){
  img.info <- file.info(paste("raw/train/", 
                              image.id[i], 
                              "/images/", 
                              image.id[i], 
                              ".png", 
                              sep = ""))
  img.sizes <- c(img.sizes, img.info$size)
}#for

img.sizes.df <- data.frame(image.id, sizes = img.sizes)
