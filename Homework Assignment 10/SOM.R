# Name   : Guizhi Xu
# CWID   : 20008770
# HW_10_SOM

rm(list=ls())

#Importing csv to r
HW_10 <- read.csv("wisc_bc_ContinuousVar.csv", header = TRUE, sep = ",", na.strings = c("?"))
HW_10
nrow(HW_10)
table(HW_10$diagnosis)\
#Removing missing values
HW_10<-na.omit(HW_10)
View(HW_10)
nrow(HW_10)

HW_10<-HW_10[-1]

#SOM
training<-HW_10[,-1],2,nstart = 10)
HW_10_som<-som(as.matrix(training), grid = somgrid(3,1))

summary(HW_10_som)
str(HW_10_som)
HW_10_som$unit.classif

table(cluster=HW_10_som$unit.classif,HW_10[,1])

plot(HW_10_som)

summary(HW_10_som)

map(HW_10_som,as.matrix(HW_10[,1]))