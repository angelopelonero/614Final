### read in data, gets rid of NAs (in this data, an NA is noted by "?")
setwd("~/Documents/USF/HS614/DiabData")
library(caret)
library(DataCombine)
library(onehot)
library(devtools); library(ggfortify); library(ggplot2)
library(randomForest)
library(qdapTools)
### lightweight deep learning library, give it a shot:
library(monmlp)

data1 <- read.csv("diabetic_data.csv", na.strings = "?")
data2 <- data1
colnames(data2)

set.seed(300)

summary(data2)

colnames(data2)

### Drop low-variance columns:

data2 <- data2[ ,-nearZeroVar(data2)]
colnames(data2)

# Drop columns with >50% NA values:

data2$weight <-NULL
data2$payer_code <- NULL
data2$medical_specialty <- NULL

# Drop rows with NA values for any remaining column (this sees a loss of ~4.7% of rows, not bad)
data2 <- na.omit(data2)
summary(data2)

### Split quant and qualitatve here

quant_data <- dplyr::select_if(data2, is.numeric)
qual_data <- dplyr::select_if(data2, is.factor)
dmyframe <- qual_data

dmyframe$diag_1 <- NULL

dmyframe$diag_2 <- NULL

dmyframe$diag_3 <- NULL

### Clean categorical data here, then run random forest to figure out which features are most important
### Creation of dummy variables from nominal vars - is this the right spot to do this?

dmy <- dummyVars(" ~ .", data = qual_data)
dmyframe <- data.frame(predict(dmy, newdata = qual_data))
summary(dmyframe)

#qual_data <- onehot(qual_data)

### Random forest feature selection here:

forestation <- randomForest(formula = readmitted ~ ., data = dmyframe, importance = TRUE, ntree=500)
getTree(forestation)
yep <- predict(forestation, dmyframe)
plot(yep)

### Use PCA to generate features to merge with those qual features selected via random forests 

prin_comp <- prcomp(na.omit(quant_data), center = TRUE, scale = TRUE)
names(prin_comp)
biplot(prin_comp)

### This plot shows which principal components are actually worth their salt

plot(prin_comp)

### The first two components seem to account for most of the data's variability
### Therefore we selet the first two PCs wheb binding with qual features for use in ML

custom_data <- cbind(qual_data,prin_comp$x[,1:2])

### now let's try to predict stuff
### Train/test split

data3 <- createDataPartition(data2$readmitted, p = .8, list = FALSE, times = 1)
train <- data2[data3, ]
test  <- data2[-data3, ]

colnames(train)

### Clustering should go here:
### Give the tsne package a go here 
### Try K-means for 2 and 3 clusters (k = 2 = readmit no or >30 vs. <30, k = 3 = predict all values)
### https://www.r-bloggers.com/k-means-clustering-in-r/

ggplot(train, aes(num_medications, number_inpatient, color = readmitted)) + geom_point()

### And don't forget agglomerative clustering:
### Do I need to scale this data? probably not, but know how to do it for the final

### Plot historgrams for each of the features you selected, hue by class:

### Running a linear model:

### QC for linear model

### Running an SVM

### QC for SVM (ROC Curve)

### Show sensitivity,specificity,accuracy,F1,AUC, etc.

### Decide which model is best: