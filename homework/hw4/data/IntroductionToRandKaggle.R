## Read in data ##
train <- read.csv("train.csv")
test <- read.csv("test.csv")

## Let's take a look at the top ##
head(train)

## The variables we are going to use to predict 
## this are class, sex, age, sibsp 
Y <- as.factor(train$survived)

## Set up structures for the independent variables ##
X <- data.frame( age=train$age,
                 fare=train$fare,
                 pclass=train$pclass,
                 sex=as.integer(factor(train$sex)))

X_test  <- data.frame( age=test$age,
                       fare=test$fare,
                       pclass=test$pclass,
                       sex=as.integer(factor(test$sex)))

## Replace missing values ##
X$fare[ is.na( X$fare) ] <- -1
X_test$fare[ is.na( X_test$fare) ] <- -1

X$age[ is.na( X$age) ] <- -1
X_test$age[ is.na( X_test$age) ] <- -1

## Load the random forest library ##
library(randomForest)
rf <- randomForest(x=X, y=Y, ntree=5000,do.trace=TRUE)
predictions <- predict(rf, newdata=X_test)
predictions <- as.numeric(predictions) - 1
                       
## Kaggle wants us to submit a 1 or 0,
## and randomForest gives 
write(predictions, file="rf_prediction.csv", ncolumns=1)


