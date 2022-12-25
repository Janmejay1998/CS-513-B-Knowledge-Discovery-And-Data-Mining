#____________ Knowledge Discovery and Data Mining (CS 513 B) __________
#_____________________________ Homework 10 _____________________________

# Course: CS 513 B
# First Name: JANMEJAY
# Last Name: MOHANTY
# ID: 20009315
# Purpose: SOM

# Clearing objects of R-environment
rm(list=ls())

# Importing wisc_bc_ContinuousVar.csv to r
WBC <- read.csv("wisc_bc_ContinuousVar.csv", header = TRUE, sep = ",", na.strings = "?")
View(WBC)

# Removing missing values

WBC <- na.omit(WBC)
View(WBC)

rdata <- WBC[-1]
diagnosis <- as.factor(WBC$diagnosis)
diagnosis <- data.frame(diagnosis)
rdata <- cbind(data.frame(diagnosis), rdata[-1])

data <- rdata[,2:13]

grp <- class::somgrid(topo = "hexagonal")
rdata.som <- class::SOM(data, grp)
plot(rdata.som)

# 2nd phase of training

rdata.som2 <- class::SOM(data,grp,alpha = list(seq(0.05,0,len=1e4), seq(0.02,0,len=1e5)), 
                                  radii = list(seq(8,1,len=1e4), seq(4,1,len=1e5)))
plot(rdata.som2)
