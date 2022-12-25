#Clear the memory
rm(list=ls())

# Import data
dataf <- read.csv("AL_NJ_Income_PCT.csv",na.strings = "?")

# View and summarize the data
View(dataf)
summary(dataf)
table(dataf$State)

# Use the kmeans clustering method to create two clusters for the “AL_NJ_Income_pct” dataset. 
dataf<-dataf[,c(-1)]
View(dataf)
kmeans_data<- kmeans(dataf[,c(3,4,5,6,7,8)],2,nstart = 10)
kmeans_data$cluster
table(kmeans_data$cluster,dataf[,1])

# Use the hierarchical clustering method and single linkage to create 4 clusters for the the “AL_NJ_Income_pct” dataset.  
data_dist<-dist(dataf[,c(3,4,5,6,7,8)])
hclust_results<-hclust(data_dist)
plot(hclust_results)
hclust_2<-cutree(hclust_results,4)
table(hclust_2,dataf[,1])

# Identify the outliers (if any)
boxplot(hclust_2,ylab = "Boxplot")
boxplot(hclust_2,
        main = "Boxplot on hclust")
outl <- boxplot.stats(hclust_2)$out
mtext(paste("Outliers: ", paste(outl, collapse = ", ")))
# No outliers found