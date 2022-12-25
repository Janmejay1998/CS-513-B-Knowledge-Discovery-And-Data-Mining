#____________ Knowledge Discovery and Data Mining (CS 513 B) __________
#_____________________________ Homework 8.2 _____________________________

# Course: CS 513 B
# First Name: JANMEJAY
# Last Name: MOHANTY
# ID: 20009315
# Purpose: k-means

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

# Implementing kmeans

WBC_kmeans <- kmeans(WBC[,-1],2,nstart = 10)
WBC_kmeans$cluster

# Plotting cluster

plotcluster(WBC[,-1],WBC_kmeans$cluster)
WBC_kmeans$centers

table(WBC_kmeans$cluster, WBC[,1])
