#____________ Knowledge Discovery and Data Mining (CS 513 B) __________
#_____________________________ Midterm _____________________________

# Course: CS 513 B
# First Name: JANMEJAY
# Last Name: MOHANTY
# ID: 20009315
# Purpose: K-Nearest Neighbors (KNN)

# Clearing objects of R-environment.
rm(list=ls())

# Loading IBM_Attrition_v2B csv data file.
IBM2 <- read.csv("IBM_Attrition_v2B.csv", header = TRUE, sep =",")

View(IBM2)

# replacing the missing values with mean
MI_mean <-round(mean(IBM2[,"MonthlyIncome"], na.rm=T))
IBM2[is.na(IBM2$MonthlyIncome), "MonthlyIncome"] <- MI_mean

# Changing Attrition into "1","0".
IBM2$Attrition<-ifelse(IBM2$Attrition=="Yes",1,0)

# Generating the train and test dataset in the ratio of 70% to 30%
size <- sample(1:nrow(IBM2), 0.7 * nrow(IBM2)) 
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

# Running normalization on first 3 columns of dataset because they are predictors
nrml <- as.data.frame(lapply(IBM2[,c(1,2,3)], nor))

IBM2_A = IBM2['Attrition']

# Train set
train <- nrml[size,] 
cls_train <- IBM2_A[size,]

# Test set
test <- nrml[-size,] 
cls_test <- IBM2_A[-size,]

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

# Computing the Precision: tp/(tp+fp):
precision <- conf_mat[1,1]/sum(conf_mat[1,1:2])
print(precision*100)

# Computing the Recall: tp/(tp+fn):
recall <- conf_mat[1,1]/sum(conf_mat[1:2,1])
print(recall*100)

# Computing the F1-score: 2*Precision*Recall/(Precision+Recall):
f1_score <- 2*precision*recall/(precision+recall)
print(f1_score*100)

