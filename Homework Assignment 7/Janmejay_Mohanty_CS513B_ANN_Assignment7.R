#____________ Knowledge Discovery and Data Mining (CS 513 B) __________
#_____________________________ Homework 7 _____________________________

# Course: CS 513 B
# First Name: JANMEJAY
# Last Name: MOHANTY
# ID: 20009315
# Purpose: ANN

# Clearing objects of R-environment
rm(list=ls())

library('neuralnet')

# Importing wisc_bc_ContinuousVar.csv to r
WBC <- read.csv("wisc_bc_ContinuousVar.csv", header = TRUE, sep = ",", na.strings = "?")

View(WBC)

# Changing labels into factor class
WBC$diagnosis <- factor(WBC$diagnosis, labels = c("M","B"))
View(WBC$diagnosis)
is.factor(WBC$diagnosis)

table(WBC$diagnosis)
WBC<-data.frame(lapply(na.omit(WBC),as.numeric))
View(WBC)

# Generating the train and test dataset in the ratio of 70% to 30%
size <- sort(sample(nrow(WBC),as.integer(.70*nrow(WBC))))

# Train set
training <- WBC[size,]
nrow(training)

# Test set
testing <- WBC[-size,]
nrow(testing)

?neuralnet()

# Implementing ANN

Model <- neuralnet(diagnosis~.,training[-1], hidden=5, threshold=0.01)

# Plotting Neural Network
plot(Model)

# Testing should only have input column
ANN <- compute(Model,testing)
ANN$net.result

ANN_cat <- ifelse(ANN$net.result < 1.5, 1 ,2)
length(ANN_cat)
length(testing$diagnosis)
table(ANN_cat,testing$diagnosis)

# Error Rate
wrong <- (testing$diagnosis!=ANN_cat)
error_rate <- sum(wrong)/length(wrong)
print(error_rate)

# Accuracy
Accuracy <- 1 - error_rate
print(Accuracy)
