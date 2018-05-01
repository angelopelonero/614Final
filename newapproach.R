### read in data, gets rid of NAs (in this data, an NA is noted by "?")
setwd("~/Documents/USF/HS614/DiabData")

data1 <- read.csv("diabetic_data.csv", na.strings = "?")
data2 <- data1
colnames(data2)

set.seed(300)

summary(data2)

colnames(data2)

### Drop low-variance columns:

data2 <- data2[ ,-nearZeroVar(data2)]
colnames(data2)

summary(data2)

### Should this be where the train/test split is?

### Split quant and qualitatve here

quant_data <- dplyr::select_if(data2, is.numeric)
  qual_data <- dplyr::select_if(data2, is.factor)

### Use PCA to generate features to merge with those qual features selected via random forests 

prin_comp <- prcomp(na.omit(quant_data), center = TRUE, scale = TRUE)
names(prin_comp)

### Clean categorical data here, then run random forest to figure out which features are most important

###