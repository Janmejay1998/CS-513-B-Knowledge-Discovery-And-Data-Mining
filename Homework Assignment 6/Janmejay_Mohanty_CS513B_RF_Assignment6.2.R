#____________ Knowledge Discovery and Data Mining (CS 513 B) __________
#_____________________________ Homework 6.2 _____________________________

# Course: CS 513 B
# First Name: JANMEJAY
# Last Name: MOHANTY
# ID: 20009315
# Purpose: Random Forest

# Clearing objects of R-environment
rm(list=ls())

# Importing breast-cancer-wisconsin.csv to r
BCW <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = "?")

summary(BCW)

# Remove Missing Values
BCW <- na.omit(BCW)

# Changing labels into factor class
BCW$Class <- factor(BCW$Class, levels = c(2,4), labels = c("benign","malignment"))
View(BCW$Class)
is.factor(BCW$Class)

#get same data
set.seed(123)

# Generating the train and test dataset in the ratio of 70% to 30%
size <- sort(sample(nrow(BCW),as.integer(.70*nrow(BCW))))

# Train set
training <- BCW[size,]
nrow(training)

# Test set
testing <- BCW[-size,]
nrow(testing)

library(randomForest)

fit <- randomForest( Class~., data=training, importance=TRUE, ntree=1000)

importance(fit)
varImpPlot(fit)
while (!is.null(dev.list()))  dev.off()

predicted <- predict(fit, testing)

#Predicting class for test set
str(predicted)
print(length(predicted))
print(length(testing$Class))

#Confusion Matrix
conf_matrix <- table(RF=predicted,Actual=testing$Class)
print(conf_matrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

#error rate
wrong<- sum(testing$Class!=predicted)
c50_rate<-wrong/length(testing$Class!=predicted)
c50_rate


