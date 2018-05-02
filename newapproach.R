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

library(DataCombine)
data2$weight <-NULL
data2$payer_code <- NULL
data2$medical_specialty <- NULL
data2 <- na.omit(data2)
summary(data2)

### Split quant and qualitatve here

quant_data <- dplyr::select_if(data2, is.numeric)
qual_data <- dplyr::select_if(data2, is.factor)

### Clean categorical data here, then run random forest to figure out which features are most important

library(randomForest)
forestation <- randomForest(formula = readmitted ~ ., data = qual_data, importance = TRUE)

### Use PCA to generate features to merge with those qual features selected via random forests 

prin_comp <- prcomp(na.omit(quant_data), center = TRUE, scale = TRUE)
names(prin_comp)

library(devtools); library(ggfortify); library(ggplot2)
biplot(prin_comp)

### This plot shows which principal components are actually worth their salt
plot(prin_comp)

### The first two components seem to account for most of the data's variability

custom_data <- cbind(qual_data,prin_comp$x[,1:2])
str(custom_data)

### Train/test split

library(caret)
data3 <- createDataPartition(data2$readmitted, p = .8, list = FALSE, times = 1)
train <- data2[data3, ]
test  <- data2[-data3, ]