#____________ Knowledge Discovery and Data Mining (CS 513 B) __________
#_____________________________ Homework 3 _____________________________

# Course: CS 513 B
# First Name: JANMEJAY
# Last Name: MOHANTY
# ID: 20009315
# Purpose: K-Nearest Neighbors (KNN)

# Clearing objects of R-environment.
rm(list=ls())

# Loading Breast cancer Wisconsin csv data file.
bcw <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep =",", na.strings ="?")

# Display Breast cancer Wisconsin csv data file.
View(bcw)

# Removing any row with a missing value in any of the columns.
bcw <- na.omit(bcw) 

# Changing labels into factor class
bcw$Class<- factor(bcw$Class , levels = c("2","4") , labels = c("Benign","Malignant"))

# Using KNN Algorithm

# Generating the train and test dataset in the ratio of 70% to 30%
size <- sample(1:nrow(bcw), 0.7 * nrow(bcw)) 
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

# Running normalization on first 4 columns of dataset because they are predictors
nrml <- as.data.frame(lapply(bcw[,c(2,3,4,5,6,7,8,9,10)], nor))

bcw2 = bcw['Class']

# Train set
train <- nrml[size,] 
cls_train <- bcw2[size,]

# Test set
test <- nrml[-size,] 
cls_test <- bcw2[-size,]

# Loading the package class
library(class)

# Running knn function for k = 3
clf <- knn(train,test,cl=cls_train,k=3)

# Creating confusion matrix
conf_mat <- table(clf, cls_test)
print(conf_mat)

# Computing the Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_mat)

# Running knn function for k = 5
clf <- knn(train,test,cl=cls_train,k=5)

# Creating confusion matrix
conf_mat <- table(clf, cls_test)
print(conf_mat)

# Computing the Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_mat)

# Running knn function for k = 10
clf <- knn(train,test,cl=cls_train,k=10)

# Creating confusion matrix
conf_mat <- table(clf, cls_test)
print(conf_mat)

# Computing the Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_mat)

