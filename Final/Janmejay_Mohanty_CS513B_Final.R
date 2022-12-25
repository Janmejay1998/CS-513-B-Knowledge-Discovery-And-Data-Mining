#____________ Knowledge Discovery and Data Mining (CS 513 B) __________
#_____________________________ Final _____________________________

# Course: CS 513 B
# First Name: JANMEJAY
# Last Name: MOHANTY
# ID: 20009315
# Purpose: Exploratory Data Analysis (EDA)

#______________________________ Problem 1 _____________________________

# Clearing object R-environment.
rm(list=ls())

# Load Breast cancer Wisconsin csv data file.
AI <- read.csv("AL_NJ_Income_PCT.csv", header = TRUE, sep =",", na.strings ="?")

# Display Breast cancer Wisconsin csv data file.
View(AI)

# Summarizing each column.

summary(AI)

# Total number of missing values in Breast cancer Wisconsin data set.
sum(is.na(AI))

# Number of missing values in a row for each column in the data set.
colSums(is.na(AI))

# Calculating the mean of column F6.
MF6 <- mean(bcw_df$F6, na.rm = TRUE)

# Replacing the missing values with "mean" of the column F6.
bcw_df[is.na(bcw_df$F6), "F6"] <- MF6

#Rounding Values to first 2 decimal places for simplicity
bcw_df[,-1] <-round(bcw_df[,-1],2)

View(bcw_df)

# Displaying the frequency table of "Class" vs. F6.
ftable(bcw_df$Class,bcw_df$F6, dnn = c("Class", "F6"))

# Displaying the scatter plot of F1 to F6, one pair at a time.
pairs(bcw_df[2:7],main = "Breast Cancer Wisconsin Data",
pch = 21,bg =c("red","blue")[factor(bcw_df$Class)])

# Displaying histogram and box plot for columns F7 to F9.
boxplot(bcw_df[8:10])
hist(bcw_df$F7, main = "Histogram of F7")
hist(bcw_df$F8, main = "Histogram of F8")
hist(bcw_df$F9, main = "Histogram of F9")

#_____________________________ Problem 2 _______________________________

# Deleting all the objects from R- environment.
rm(list=ls())

# Reloading the Breast cancer Wisconsin csv data file from canvas into R.
bcw1 <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep =",", na.strings ="?") 

View(bcw1)

# Number of rows
nrow(bcw1)

# Removing any row with a missing value in any of the columns.
bcw_missing <- na.omit(bcw1) 

View(bcw_missing)

# Number of rows after removing the missing value rows.
nrow(bcw_missing)

#____________________________ THE END __________________________________
