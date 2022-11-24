#____________ Knowledge Discovery and Data Mining (CS 513 B) __________
#_____________________________ Homework 5.2 _____________________________

# Course: CS 513 B
# First Name: JANMEJAY
# Last Name: MOHANTY
# ID: 20009315
# Purpose: Dtree

# Clearing objects of R-environment
rm(list=ls())

library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

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

#Growing the tree
#dev.off()
while (!is.null(dev.list()))  dev.off()

#plot the tree
CART_class<-rpart(Class~.,data=training[,-1], method = "class")
rpart.plot(CART_class)
summary(CART_class)

#Predicting class for test set
predicted <- predict(CART_class, testing, type = "class")
print(length(predicted))
print(length(testing$Class))

#Confusion Matrix
conf_matrix <- table(CART=predicted,Actual=testing$Class)
print(conf_matrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

#error rate
wrong<- sum(testing$Class!=predicted)
dtree_error_rate<-wrong/length(testing$Class!=predicted)
dtree_error_rate

prp(CART_class)
fancyRpartPlot(CART_class)


