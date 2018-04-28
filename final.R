#read in data, gets rid of NAs (in this data, an NA is noted by "?")
setwd("~/Documents/USF/HS614/DiabData")

help("read.csv")
data1 <- read.csv("diabetic_data.csv", na.strings = "?")
data2 <- data1
data2

# set a seed to fix the random number generator and make your split reproducable (300 is a random choice)
set.seed(300)

# convert nominal values to numerical values
# source: https://stackoverflow.com/questions/47922184/convert-categorical-variables-to-numeric-in-r


to_convert <- sapply(data2,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
converted <-sapply(data2[,to_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
data3 <- cbind(data2[,!to_convert],converted)        # complete data.frame with all variables put together
data3

#the following is trying to use the library "caTools" 
# note: convert this to use caret() as per everyone's suggestion
require(caTools)
sample(data2)
data2$spl <- sample.split(data, SplitRatio = 0.8)
head(data2)
train <- subset(data1, data2$spl == TRUE)
test <- subset(data1, data2$spl == FALSE)
test
train


# let's plot some data to run regression on:
plot(data2$number_diagnoses, data2$diabetesMed)


