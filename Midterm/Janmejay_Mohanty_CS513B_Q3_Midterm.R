#____________ Knowledge Discovery and Data Mining (CS 513 B) __________
#_____________________________ Midterm _____________________________

# Course: CS 513 B
# First Name: JANMEJAY
# Last Name: MOHANTY
# ID: 20009315
# Purpose: Exploratory Data Analysis (EDA)

#___________________________ Problem 3 (I) _____________________________
# Clearing object R-environment.
rm(list=ls())

# Load IBM_Attrition_v3B csv data file.
IBM3 <- read.csv("IBM_Attrition_v3B.csv", header = TRUE, sep =",")
View((IBM3))
# Summarizing each column.
summary(IBM3)

#___________________________ Problem 3 (II) _____________________________
# Identifying missing values.

# Total number of missing values in Breast cancer Wisconsin data set.
sum(is.na(IBM3))

# Number of missing values in a row for each column in the data set.
colSums(is.na(IBM3))

miss<-1
select<-NULL
while(miss<=nrow(IBM3)){
  if(anyNA(IBM3[miss,])) select<-rbind(select, IBM3[miss,])
  miss<-miss+1
}
print(select)

#___________________________ Problem 3 (III) _____________________________

library(modeest)

# Calculating the mode of MonthlyIncome.
Mode <-mlv(IBM3$MonthlyIncome,method='mfv',na.rm=T)
print(Mode)

# Replacing the missing values with "mean" of the column F6.
IBM3[is.na(IBM3$MonthlyIncome), "MonthlyIncome"] <- mean(Mode)

print(IBM3)

#___________________________ Problem 3 (IV) _____________________________

# Displaying the scatter plot of "Age", "MonthlyIncome" and "YearsAtCompany"
pairs(IBM3[c("Age", "MonthlyIncome", "YearsAtCompany")])

#___________________________ Problem 3 (V) _____________________________

# Showing histogram box plot for columns: "Age", "MonthlyIncome" and "YearsAtCompany"
boxplot(IBM3[c("Age", "MonthlyIncome", "YearsAtCompany")])
hist(IBM3[,"Age"],main = "Histogram of Age", xlab = "Age")
hist(IBM3[,"MonthlyIncome"], main = "Histogram of MonthlyIncome", xlab = "MonthlyIncome")
hist(IBM3[,"YearsAtCompany"], main = "Histogram of YearsAtCompany", xlab = "YearsAtCompany")

