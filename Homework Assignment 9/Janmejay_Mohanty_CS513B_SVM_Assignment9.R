#____________ Knowledge Discovery and Data Mining (CS 513 B) __________
#_____________________________ Homework 9 _____________________________

# Course: CS 513 B
# First Name: JANMEJAY
# Last Name: MOHANTY
# ID: 20009315
# Purpose: SVM

# Clearing objects of R-environment
rm(list=ls())

# Importing wisc_bc_ContinuousVar.csv to r
WBC <- read.csv("wisc_bc_ContinuousVar.csv", header = TRUE, sep = ",", na.strings = "?")
View(WBC)

# Removing missing values

WBC <- na.omit(WBC)
View(WBC)

WBC$diagnosis <- factor(WBC$diagnosis)
is.factor(WBC$diagnosis)

# Generating the train and test dataset in the ratio of 70% to 30%
size <- sort(sample(nrow(WBC),as.integer(.70*nrow(WBC))))

training <- WBC[size,]
testing <- WBC[-size,]

library(e1071)

svm.model <-svm(diagnosis~., data=training)
svm.pred <- predict(svm.model, testing)

t <- table(actual = testing$diagnosis, svm.pred)
t

# Error Rate
svm_wrong <- (testing$diagnosis!=svm.pred)
error_rate <- sum(svm_wrong)/length(svm_wrong)
print(error_rate)

# Accuracy
Accuracy <- 1 - error_rate 
print(Accuracy)
