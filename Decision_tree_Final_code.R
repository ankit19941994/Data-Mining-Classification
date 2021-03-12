#Loading The required liabraries for decision tree

library(caTools)

#calling the data from file directory

census <- read.csv("C:/Users/MAVERICK/OneDrive/Desktop/ABHI/2021-SPRING/CSE-4334-DATA_MINING/Project/census.csv")
A <-census
summary(census)
head(A)
table(A$salary)

#splitting the data for the given ratio

ind <- sample(2, nrow(A), replace=TRUE, prob=c(0.7, 0.3))
trainData <- census[ind==1,]
testData <- census[ind==2,]

#BaseLine Accuracy for Training set
z = table(trainData$salary)
z

#Computing the Accuracy
z[1]/sum(z)

#Baseline Accuracy for Testing set
y = table(testData$salary)
y

#Computing the Accuracy
y[1]/sum(y)


#For plotting the decision tree 

library(rpart)
library(rpart.plot)
rtree <- rpart(salary ~ .,trainData)
rpart.plot(rtree)

#Partitioning the validation index

library(caret)
Val_Ind <- createDataPartition(A$salary, p=0.70, list=FALSE)
Val <- A[-Val_Ind,]
A <- A[Val_Ind,]

#Cross-Validation using K-fold for K=10

C <- trainControl(method="cv", number=10)
metric <- "Accuracy"
set.seed(8121999)

#Comparing Two methods Linear determinant analysis and Knn analysis


fit.lda <- train(salary~., data=A, method="lda" ,metric =metric,trControl=C)
fit.knn <- train(salary~., data=A, method="knn", metric=metric, trControl=C)
results <- resamples(list(lda=fit.lda, knn=fit.knn))
summary(results)
dotplot(results)

