### read in data, gets rid of NAs (in this data, an NA is noted by "?")
setwd("~/Documents/USF/HS614/DiabData")

data1 <- read.csv("diabetic_data.csv", na.strings = "?")
data2 <- data1
colnames(data2)

### Split quant and qualitatve here

### Clean quant data here, then run PCA to create component features

### Clean categorical data here, then run random forest to figure out which features are most important

###