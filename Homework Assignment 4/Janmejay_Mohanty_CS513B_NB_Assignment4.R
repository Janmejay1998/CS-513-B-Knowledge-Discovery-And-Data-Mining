#____________ Knowledge Discovery and Data Mining (CS 513 B) __________
#_____________________________ Homework 4 _____________________________

# Course: CS 513 B
# First Name: JANMEJAY
# Last Name: MOHANTY
# ID: 20009315
# Purpose: Naïve Bayes (NB)

# Clearing objects of R-environment
rm(list=ls())

# Loading necessary libraries
library(e1071)
library(class) 

# Importing breast-cancer-wisconsin.csv to r
BCW <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = "?")

# Remove Missing Values
BCW_NM <- na.omit(BCW)

# Changing labels into factor class
BCW_NM$Class <- factor(BCW_NM$Class, levels = c(2,4), labels = c("benign","malignment"))
View(BCW_NM$Class)
is.factor(BCW_NM$Class)

# Generating the train and test dataset in the ratio of 70% to 30%
size <- sort(sample(nrow(BCW_NM),as.integer(.70*nrow(BCW_NM))))

# Train set
training <- BCW_NM[size,-1]
nrow(training)

# Test set
test <- BCW_NM[-size,-1]
nrow(test)

# Running Naive Bayes classification
NB_class <- naiveBayes(formula = Class~., data =training)
category_class<-predict(NB_class,test)

head(cbind(category_class, test))

conf_mat <- table(NBayes=category_class,Class=test$Class)
print(conf_mat)

NB_wrong <- sum(category_class!=test$Class)

# Computing the Accuracy
accuracy <- {sum(diag(conf_mat)/(sum(rowSums(conf_mat)))) * 100}
print(accuracy)

# Calculating error rate
NB_error_rate <- {NB_wrong/length(category_class)*100}
print(NB_error_rate)

