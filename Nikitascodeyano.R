setwd("~/Documents/USF/HS614/DiabData")
data <- read.csv("diabetic_data.csv", na.strings = "?")
head(data[, 1:6])
library(caret)
set.seed(300)
############################ Potential Questions to Explore ############################
#Pattern btwn medical_specialty and the different types of medication
#Readmittance rate based only on medication
#Predict what range you fall into for # of medications --med_range column 

data['med_range'] <- NA
populate <- function(data){
  for (n in 1:nrow(data)){
    x<- data$num_medications[n]
    if (1<=x && x<=10){
      data$med_range[n]<<- 0
    }else if (11<=x && x<=20){
      data$med_range[n]<<- 1
    }else if (21<=x && x<=30){
      data$med_range[n]<<- 2
    }else if (31<=x && x<=40){
      data$med_range[n]<<- 3
    }else if (41<=x && x<=50){
      data$med_range[n]<<- 4
    }else if (51<=x && x<=60){
      data$med_range[n]<<- 5
    }else if (61<=x && x<=70){
      data$med_range[n]<<- 6
    }else if (71<=x && x<=80){
      data$med_range[n]<<- 7
    }else if (81<=x && x<=90){
      data$med_range[n]<<- 8
    }else if (x>=91){
      data$med_range[n]<<- 9
    }
  }
}
populate(data)

to_convert <- sapply(data,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
converted <-sapply(data[,to_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
data <- cbind(data[,!to_convert],converted)        # complete data.frame with all variables put together
data <- na.omit(data)

trainIndex <- createDataPartition(data$med_range, p = .8, list = FALSE, times = 1)
train <- data[trainIndex, ]
test  <- data[-trainIndex, ]

##Bootstrapping the weight column 
##Cross validation 
##SVM 

train$med_range <- as.factor(train$med_range)
summary(train)

model <- glm(med_range ~ . ,family=binomial(link='logit'),data=train)
model2 <- glm(med_range ~ admission_type_id + time_in_hospital + num_procedures + number_diagnoses + race + gender + age + medical_specialty + readmitted ,family=binomial(link='logit'),data=train)
#Warning messages:
#  1: glm.fit: algorithm did not converge 
#  2: glm.fit: fitted probabilities numerically 0 or 1 occurred
summary(model) #numbers look weird 
summary(model2) #numbers look better

train$med_range <- droplevels(train$med_range)

library(randomForest)
RF1 <- randomForest(formula = med_range ~ admission_type_id + time_in_hospital + num_procedures + number_diagnoses + race + gender + age + medical_specialty + readmitted, data = train, importance = TRUE)
RF2 <- randomForest(formula = med_range ~ ., data = train, importance = TRUE)
RF2 

RF2_fix <- randomForest(formula = med_range ~ ., data = train, mtry = 12, importance = TRUE)
RF2_fix

predTrain <- predict(RF2, train, type = "class")
table(predTrain, train$med_range)

predTest <- predict(RF2, test, type = "class")
table(predTest, test$med_range) #11 misclassifications I think.... why are there 4 columns though

importance(RF2)
varImpPlot(RF2)

# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:15) {
  model3 <- randomForest(med_range ~ ., data = train, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, test, type = "class")
  a[i-2] = mean(predValid == test$med_range)
}

a
#[1] 0.7307692 0.8942308 0.9326923 0.9471154 0.9519231 0.9519231 0.9567308
#[8] 0.9519231 0.9567308 0.9711538 0.9759615 0.9759615 0.9903846
plot(3:15,a)

