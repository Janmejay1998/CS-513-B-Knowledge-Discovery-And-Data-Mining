#____________ Knowledge Discovery and Data Mining (CS 513 B) __________
#_____________________________ Homework 8.1 _____________________________

# Course: CS 513 B
# First Name: JANMEJAY
# Last Name: MOHANTY
# ID: 20009315
# Purpose: hclust

# Clearing objects of R-environment
rm(list=ls())

library(fpc)
library(cluster)

# Importing wisc_bc_ContinuousVar.csv to r
WBC <- read.csv("wisc_bc_ContinuousVar.csv", header = TRUE, sep = ",", na.strings = "?")
View(WBC)
nrow(WBC)

table(WBC$diagnosis)

# Removing missing values

WBC <- na.omit(WBC)
View(WBC)
nrow(WBC)

WBC <- WBC[-1]
WBC_dist <- dist(WBC[,-1])
View(WBC_dist)

# Implementing Hcluster

WBC_hclust <- hclust(WBC_dist)

# Plotting Hcluster
plot(WBC_hclust)

dev.off()

WBC_hclust2 <- cutree(WBC_hclust,2)
WBC_hclust2
test <- table(WBC_hclust2,WBC[,1])

print(test)

# Accuracy
Accuracy <- (sum(diag(test))/(sum(rowSums(test)))*100)
print(Accuracy)
