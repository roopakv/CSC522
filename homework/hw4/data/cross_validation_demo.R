###### Run this part to create the data set for the demo #######
## Make up some data for the demo ##
## I'm setting the random seed so that this demo 
## produces the same results everytime.
set.seed(1234)

## Make a set of predictors ##
X <- as.data.frame(matrix(rnorm(1000),ncol=5))
names(X) <- c("var1","var2","var3","var4","var5")

## Define the relationship between X and Y via a logit link function ##
Y <- exp(.25*X$var1 + X$var2 + 0.5*X$var3 - X$var4 + X$var4*X$var5)/
      (1+exp(.25*X$var1 + X$var2 + 0.5*X$var3 - X$var4 + X$var4*X$var5))

## Make it categorical ##
Y <- as.factor(round(Y))

######### Begin CV demo ##########
## We will use the caret pacakge to help ##
library(caret)
## put it all into a training set ##
train <- cbind(Y,X)

## rpart is a decision tree pacakge ##
library(rpart)
## Type ?rpart to see the list of parameters for rpart ##
## Notice there is a variable labeled control ##
## The variable cp controls the complexity of the tree ##
## and affects how accurate the tree will be ##
## If you type ?rpart.control you can see the documentation ##

## Here is how you provide a value for cp ##
tree <- rpart(Y~.,data=train,control=rpart.control(cp=0.5))

## Now we want to get an idea of how accuracte this value ##
## will be in general, so we will do 5-fold cross validation ##
## To do this we will break the training data into 5 equal parts, ##
## and fit the model to 4 parts, and test on the remaining parts ##
## We will do this 5 times. This is known as 5-fold cross validation ##

## You can break up the data by hand, but I'm going to use caret to help ##
## We will supply the Y values, and caret will give us a "balanced" split ##
folds <- as.data.frame(createFolds(Y,k=5))
## Now folds$Fold1 contains all of the rows to build the model for the first fold ##
## Let's loop through and calculate the test accuracy ##
average.accuracy <- 0
for(i in 1:5) {  
  ## cv.train is what we will use to build the model. ##
  ## cv. test is what we will use to measure accuracy ##
  fold.rows <- folds[,i]
  cv.train <- train[fold.rows,]
  ## -folds$Fold1 gives all the rows not
  ## in Fold1
  cv.test <- train[-fold.rows,]
  
  ## Build a tree using the training data ##
  cv.tree <- rpart(Y~.,data=cv.train,control=rpart.control(cp=0.5))
  ## Predict the classes of the test data and measure the accuracy ##
  pred.classes <- predict(cv.tree,newdata=cv.test,type="class")
  miss.classified <- which(cv.test$Y != pred.classes)
  num.miss.classified <- length(miss.classified)
  accuracy <- 1-(num.miss.classified/length(cv.test$Y))
  ## Add it to the running total ##
  average.accuracy <- average.accuracy + accuracy
}
print("5-fold cross-validated classification accuracy for cp = 0.5 is:")
sum(average.accuracy/5)


## That's the way to do this by hand. Let's use caret to help speed this along ##
## Let's give caret a list of parameters to try ##
## Caret requires the column name to be .var_name ##
cpList <- expand.grid(.cp = c(0.01,0.1,.25,0.5,1))
## This is a list of items to control the training process ##
## method = 'cv' means use cross-validation ##
## number = 5 means use 5-fold CV ##
crtl <- trainControl(method = "cv", number = 5) 
tree <- train(Y~.,data=train,method="rpart",trControl=crtl,tuneGrid=cpList)
## Ignore any warnings ##
## Now let's look at the results ##
tree

## The accuracy column contains the classification accuracy ##
## Notice the value for 0.5 is close to our previous CV estimate ##
## that we did "by hand". The other columns are different measures of accuracy ##
## Now let's get the best model out ##
final.tree <- tree$finalModel
## This model contains the model with the best value of cp = 0.01 ##
## This is how we would predict with the best model on new data##
## if the new data were in a dataframe called test ##
## predict(final.tree,newdata=test) ##
