#Loading The required liabraries for decision tree


library(caTools)

#calling the data from file directory

census <- read.csv("C:/Users/MAVERICK/OneDrive/Desktop/ABHI/2021-SPRING/CSE-4334-DATA_MINING/Project/census.csv")
A <-census

#Setting the seed value and splitting the data

set.seed(8121999)
spl = sample.split(A$salary, SplitRatio = 0.7)
train = subset(A, spl==TRUE)
test = subset(A,spl==FALSE)
train
test

#Summary of the data

summary(census)
head(A)
table(A$salary)

#Splitting the data in the given ratio

ind <- sample(2, nrow(A), replace=TRUE, prob=c(0.7, 0.3))
trainData <- census[ind==1,]
testData <- census[ind==2,]

#Constructing the Naiivee Bayes Model

library(rpart)
library(rpart.plot)
library(naivebayes)
library(e1071)
library(dplyr)
model <- naiveBayes(salary ~ ., trainData)
model

