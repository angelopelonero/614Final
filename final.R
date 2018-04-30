#read in data, gets rid of NAs (in this data, an NA is noted by "?")
setwd("~/Documents/USF/HS614/DiabData")

help("read.csv")
data1 <- read.csv("diabetic_data.csv", na.strings = "?")
data2 <- data1
colnames(data2)

# set a seed to fix the random number generator and make your split reproducable (300 is a random choice)
set.seed(300)

# convert nominal values to numerical values
# should this be done with hot encoding or something?
# source: https://stackoverflow.com/questions/47922184/convert-categorical-variables-to-numeric-in-r
to_convert <- sapply(data2,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
converted <-sapply(data2[,to_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
data3 <- cbind(data2[,!to_convert],converted)        # complete data.frame with all variables put together
colnames(data3)

# find and drop zero-variance columns

data3 <- data3[ ,-nearZeroVar(data3)] # finds and removes near-zero variance columns from data
colnames(data3)

# find and drop NA-containing rows - this doesn't drop any rows according to nrow()
#require(DataCombine)
#DropNA(data3, message = TRUE)

#the following drops a TON of rows - going to try using it only in the PCA call and leave it for the rest
#data3 <- na.omit(data3)

nrow(data2)
nrow(data3)

# the variable "data3" now has entirely numerical values, no zero-cvariance colums, and no NA values (recall: removed NA rows)
# this data is now ready for PCA, but first we split into train/test sets
# currently using an 80/20 split

library(caret)
data4 <- createDataPartition(data3$readmitted, p = .8, list = FALSE, times = 1)
train <- data3[data4, ]
test  <- data3[-data4, ]
colnames(train)

# There's a large number of features in this data
# Therefore PCA will be used to determine important ML features
# Pair-plotting and guesswork may be too cumbersome in this instance

prin_comp <- prcomp(na.omit(train), center = TRUE, scale = TRUE) # throws error if NAs in data: Error in svd(x, nu = 0, nv = k) : infinite or missing values in 'x'
library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify); library(ggplot2)
autoplot(prcomp(na.omit(train), center = TRUE, scale = TRUE), colour = 'readmitted')

colnames(train)

names(prin_comp)
biplot(prin_comp)
plot(prin_comp)

#svm on whole dataset to predict "readmit"
library("e1071")
svm_model <- svm(readmitted ~ ., data=train)
summary(svm_model)

#svm_tune <- tune(svm, train.x=x, train.y=y, 
#                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
#print(svm_tune)


"""
#This is the old split approach I took, saving here for now.

require(caTools)
sample(data2)
data2$spl <- sample.split(data, SplitRatio = 0.8)
head(data2)
train <- subset(data1, data2$spl == TRUE)
test <- subset(data1, data2$spl == FALSE)
test
train


"""


