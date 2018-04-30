### read in data, gets rid of NAs (in this data, an NA is noted by "?")
setwd("~/Documents/USF/HS614/DiabData")

data1 <- read.csv("diabetic_data.csv", na.strings = "?")
data2 <- data1
colnames(data2)

### set a seed to fix the random number generator and make your split reproducable (300 is a random choice)
set.seed(300)

### the following would drop a TON of rows if we drop NAs, so we're removing it now
data2$weight <- NULL

### NOTE: TRY BOOTSTRAPPING payer_code and medical_specialty

### onehot encoding -what it do?

#require(onehot)
#onehot(data2)

### convert nominal values to numerical values
### replaceable with onehot encoding or something?
### source: https://stackoverflow.com/questions/47922184/convert-categorical-variables-to-numeric-in-r

to_convert <- sapply(data2,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
converted <-sapply(data2[,to_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
data3 <- cbind(data2[,!to_convert],converted)        # complete data.frame with all variables put together
colnames(data3)

### find and drop zero-variance columns
### this oblates the entire dataframe if it's not converted to numeric values

data3 <- data3[ ,-nearZeroVar(data3)]
colnames(data3)

### payer_code, and medical_specialty would drop a lot of data if we drop NAs
### Therefore we're currently only using this command in the PCA call to check the importance of payer_code and medical_specialty before dropping

#data3 <- na.omit(data3)
#nrow(data2)
#nrow(data3)

### the variable "data3" now has entirely numerical values, no zero-cvariance colums, and no NA values (recall: removed NA rows)
### this data is now ready for PCA, but first we split into train/test sets
### currently using an 80/20 split

library(caret)
data4 <- createDataPartition(data3$readmitted, p = .8, list = FALSE, times = 1)
train <- data3[data4, ]
test  <- data3[-data4, ]

### There's a large number of features in this data
### Therefore PCA will be used to determine important ML features
### Pair-plotting and guesswork may be too cumbersome in this instance

prin_comp <- prcomp(na.omit(train), center = TRUE, scale = TRUE)
names(prin_comp)

### plotting PCA:

library(devtools); library(ggfortify); library(ggplot2)
autoplot(prcomp(na.omit(train), center = TRUE, scale = TRUE), colour = 'readmitted')
autoplot(prcomp(na.omit(train)), colour = 'readmitted', loadings = TRUE)
biplot(prin_comp)
plot(prin_comp)

### from the documentation, we know that weight, payer_code, and medical_specialty have large amounts of missing data
### those columns may be removed here if they were shown to be unimportant via PCA

### svm on whole dataset to predict "readmit"
library("e1071")
svm_model <- svm(readmitted ~ ., data=train)
summary(svm_model)

#svm_tune <- tune(svm, train.x=x, train.y=y, 
#                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
#print(svm_tune)

# glm model run here:

help(glm)

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


