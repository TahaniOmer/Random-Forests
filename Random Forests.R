#Random Forests

#required packages
library(MASS)
library(ISLR)
library(tree)

names(Boston)
str(Boston)
#summary statistics
attach(Boston)
summary(Boston)
#Check for missing values
sum(is.na(Boston))


# Fitting Regression Trees
#Split data set into 50:50 train and test data
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston1=tree(medv~.,Boston,subset=train)
summary(tree.boston1)
plot(tree.boston1)
text(tree.boston1,plot.new = plot(tree.boston1), pretty=0)

#We will see how it performs in the test dataset.
yhat=predict(tree.boston1,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
mean((yhat-boston.test)^2)

#Pruning
cv.boston=cv.tree(tree.boston1)
cv.boston

plot(cv.boston$size,cv.boston$dev,type='b',xlab = "Tree-Size",ylab = "Deviance_sqrt(MSE)")

prune.boston=prune.tree(tree.boston1 ,best=5)
plot(prune.boston)
text(prune.boston,plot.new=plot(prune.boston), pretty =0)
yhat=predict(prune.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)

mean((boston.test-yhat)^2)


# Random Forests
#Random forests build lots of bushy trees, and then avaerage them to reduce the variance

library(randomForest)

train = sample(1:nrow(Boston),300)

#we will use the respons 'medv',the median housing value(in\$1k dollars)

rf.boston = randomForest(medv~.,data = Boston,subset = train)
rf.boston

#The MSR and % variance explaind are based on OOB or-out-of-bage estimates.
#The model reports that 'mtry=4',which is the number of variable randomly chosen at each split.


yhat=predict(rf.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
mean((boston.test-yhat)^2)

#we get the mean square error of  11.41831 indicating that this model leads to
#test predictions that are within around $3.182059 of true median home value for the suburb 



