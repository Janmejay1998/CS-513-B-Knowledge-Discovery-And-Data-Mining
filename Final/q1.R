#____________ Knowledge Discovery and Data Mining (CS 513 B) __________
#_____________________________ Final _____________________________

# Course: CS 513 B
# First Name: JANMEJAY
# Last Name: MOHANTY
# ID: 20009315
# Purpose: Kmeans and Hclust


# Clear the memory
rm(list=ls())

# Loading AL_NJ_Income_PCT data
AN <- read.csv("AL_NJ_Income_PCT.csv",na.strings = "?")

# Viewing and summarizing the data
View(AN)
summary(AN)
table(AN$State)

# Implementing kmeans algorithm
AN<-AN[,c(-1)]
View(AN)
kmeans_AN<- kmeans(AN[,c(3,4,5,6,7,8)],2,nstart = 10)
kmeans_AN$cluster

# Cross Table
table(kmeans_AN$cluster,AN[,1])

# Implementing hcluster algorithm
AN_dist<-dist(AN[,c(3,4,5,6,7,8)])
hclust_data<-hclust(AN_dist)
plot(hclust_data)
hclust_2<-cutree(hclust_data,4)

# Cross Table
table(hclust_2,AN[,1])  

# Identifying outlier
boxplot(hclust_2,
        main = "Boxplot on hclust")
outl <- boxplot.stats(hclust_2)$out
mtext(paste("Outliers: ", paste(outl, collapse = ", ")))
# No outliers found
