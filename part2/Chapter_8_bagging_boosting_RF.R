## BAGGING / BOOSTING/ RANDOM FORESTS

## LAB 1 Decision Trees
### Fitting classification trees

## load dependencies
install.packages("tree")
library(tree)
library(ISLR)
# Recode the variable into a binary

high=ifelse(Carseats$Sales<=8,"No","Yes")

Carseats1<-data.frame(Carseats,high)
attach(Carseats1)

tree.carseats=tree(high~.-Sales,Carseats1)
summary(tree.carseats)
# residual mean deviance is devince /n-t0
#plot the graph
plot(tree.carseats,col="hot pink")
text(tree.carseats,pretty=0,col="purple")
# split criterion, number of observations, deviance and overall prediction for the branch
tree.carseats

#use testing and training datasets
set.seed(2)
train=sample(1:nrow(Carseats1),200)
Carseats.train=Carseats1[train,]
Carseats.test=Carseats1[-train,]
high.test=high[-train]

tree.carseats.train=tree(high~.-Sales,data=Carseats1,subset=train)
tree.predict=predict(tree.carseats,Carseats.test,type="class")
table(tree.predict,high.test)
# test error is (108+77)/200=0.925

# can pruning lead to better results?
# use CV to determing optimal tree complexity 

set.seed(3)
cv.carsearts=cv.tree(tree.carseats,FUN=prune.misclass)
# k is the alpha tuning parameter non negaive
names(cv.carsearts)
cv.carsearts

# plot error rate as a fucntion of size and k
par(mfrow=c(1,2))
plot(cv.carsearts$size,cv.carsearts$dev,type="b")
plot(cv.carsearts$k,cv.carsearts$dev,type="b")

prune.carseats=prune.misclass(tree.carseats,best=14)
plot(prune.carseats)
text(prune.carseats,pretty=0)

# how well does the prunned do on the dataset? 
# use predict
tree.predict=predict(prune.carseats,Carseats.test,type="class")
table(tree.predict,high.test)
# 110+72/200=0.91
# prunning improves the interpretability

####### IF YOU INCREASE NUMBER OF TERMINAL NODES AWAY FROM OPTIMAL
######YOU GET SMALLER CLASSIFICATION ACCURACY
prune.carseats2=prune.misclass(tree.carseats,best=2)

# how well does the prunned do on the dataset? 
# use predict
tree.predict2=predict(prune.carseats2,Carseats.test,type="class")
table(tree.predict2,high.test)
# (106+41)/200=0.735

### Fitting REGRESSION trees
library(MASS)
set.seed(1)
attach(Boston)
train=sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
par(mfrow=c(1,1))
plot(tree.boston)
text(tree.boston,pretty=0)

# will pruning improve performance
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
cv.boston

prune.boston=prune.tree(tree.boston,best=6)
plot(prune.boston)
text(prune.boston,pretty=0)

# check test error
yhat=predict(tree.boston,newdata =Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
# this is the test MSE
mean((yhat-boston.test)^2)



###############################################
##     BAGGING AND RANDOM FORESTS            ##
###############################################
install.packages("randomForest")
library(randomForest)
set.seed(1)
# mtry is how many predictors are considered for each split of the tree
bag.boston=randomForest(medv~.,data=Boston,subset=train, mtry=13, importance=TRUE)
bag.boston

yhat.bag=predict(bag.boston,newdata = Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
# test MSE= 23.59273

# you can change the number of trees
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag=predict(bag.boston,newdata = Boston[-train,])
mean((yhat.bag-boston.test)^2)
# test mse=23.45478

# using random forests so m!=p
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train, mtry=6,iimportance=TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
# test MSE 18.72676

# to see the importance of each variable
importance(rf.boston)
varImpPlot (rf.boston )



###############################################
##              BOOSTING                     ##
###############################################

install.packages("gbm")
library(gbm)

set.seed(1)
# if you had a classificantion problem then you add distribution="bernoulli"
boost.boston=gbm(medv~.,data=Boston[train,],
                 distribution = "gaussian",
                 n.trees=5000,interaction.depth = 4)
summary(boost.boston)

par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

yhat.boost=predict(boost.boston,newdata = Boston[-train,],
                   n.trees=5000)
mean((yhat.boost-boston.test)^2)
# MSE 17.6671

# changing the value of lambda
boost.boston=gbm(medv~.,data=Boston[train,],distribution = "gaussian",
                 n.trees=5000,interaction.depth = 4,shrinkage=0.2,
                 verbose=F)
mean((yhat.boost-boston.test)^2)
# MSE 17.6671


###############################################
##              EXERCISES                    ##
###############################################
# Question 8

# split data into training and testing
set.seed(1)
train=sample(1:nrow(Carseats),nrow(Carseats)/2)
carseats.train=Carseats[train,]
carseats.test=Carseats[-train,]

# fit a regression tree tp the training set
tree.carseats_8=tree(Sales~.,Carseats,subset=train)
summary(tree.carseats_8)
# plot
par(mfrow=c(1,1))
plot(tree.carseats_8)
text(tree.carseats_8,pretty = 0)
# interpret 
#Shefloc and price are important 
# test error
yhat<-predict(tree.carseats_8,newdata=carseats.test)
mean((yhat-carseats.test$Sales)^2)
#MSE 4.922039

# use CV in order to determine optimal level of tree complexity
cv.carseats=cv.tree(tree.carseats_8)
plot(cv.carseats$size,cv.carseats$dev,type="b")
tree.min=which.min(cv.carseats$dev)
points(cv.carseats$size[tree.min],cv.carseats$dev[tree.min],col="red",cex=2,pch=20)
# does pruning inprove test error
prune.car=prune.tree(tree.carseats_8,best=18)
plot(prune.car)
text(prune.car,pretty=0)
yhat<-predict(prune.car,newdata=carseats.test)
mean((yhat-carseats.test$Sales)^2)
#MSE 4.922039


## use bagging

set.seed(1)
# mtry is how many predictors are considered for each split of the tree
bag.car=randomForest(Sales~.,data=Carseats,subset=train, mtry=10, importance=TRUE)
bag.car

yhat.bag=predict(bag.car,newdata = carseats.test)
plot(yhat.bag,carseats.test$Sales)
abline(0,1)
mean((yhat.bag-carseats.test$Sales)^2)
# test MSE= 2.63


# to see the importance of each variable
importance(bag.car)
varImpPlot (bag.car )

## RANDOM FORESTS
set.seed(1)
rf.car=randomForest(Sales~.,data=Carseats,subset=train,importance=TRUE)
yhat.rf=predict(rf.car,newdata=Carseats[-train,])
mean((yhat.rf-Carseats.test$Sales)^2)
# test MSE 10.6746

# to see the importance of each variable
importance(rf.boston)
varImpPlot (rf.boston )


## QUESTION 9
# set 800 for training and the rest test
set.seed(1)
train=sample(1:nrow(OJ),800)
attach(OJ)
OJ.train=OJ[train,]
OJ.test=OJ[-train,]

# fit a tree w purchace as response 
tree.oj=tree(Purchase~.,data=OJ.train)
summary(tree.oj)
# training 127/800=0.1588 and 9 terminal nodes

tree.oj

# plot tree
plot(tree.oj)
text(tree.oj,pretty=0)
# we see most important is Loyal CH

#predict on the test data
tree.pred=predict(tree.oj,OJ.test,type="class")
table(tree.pred,OJ.test$Purchase)
testerror=(8+38)/nrow(OJ.test)
# test error is 0.17

# determine optimal tree size
oj.cv.train=cv.tree(tree.oj,FUN=prune.misclass)
oj.cv.train

plot(oj.cv.train$size,oj.cv.train$dev,type="b")
tree.min=which.min(oj.cv.train$dev)
points(oj.cv.train$size[tree.min],oj.cv.train$dev[tree.min],col="red",cex=2,pch=20)
optimal_tree_size=oj.cv.train$size[tree.min]
# we see that the 7 node tree has lowest classification errror

# prune the tree
prune.oj=prune.misclass(tree.oj,best=optimal_tree_size)
plot(prune.oj)
text(prune.oj,pretty=0)

# compare the training errror for prune and non prune
summary(prune.oj)
# training error for prune 0.1625
# training error for unpruned 0.1588
# misclassification error is higher for the pruned tree

# calculate miscalss error on test of pruned and unpruned
prune.pred <- predict(prune.oj, OJ.test, type = "class")
table(prune.pred, OJ.test$Purchase)

(8+36)/270
#0.162963 for pruned
# 0.17 for unpruned

#Exercisse 10
Hitters=na.omit(Hitters)
Hitters$Salary=log(Hitters$Salary)

train=1:200
Hitters.train=Hitters[train,]
Hitters.test=Hitters[-train,]

# different shrinking values lambda
library(gbm)
set.seed(1)
pows <- seq(-10, -0.2, by = 0.1)
lambdas <- 10^pows
train.err <- rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  boost.hitters <- gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  pred.train <- predict(boost.hitters, Hitters.train, n.trees = 1000)
  train.err[i] <- mean((pred.train - Hitters.train$Salary)^2)
}
plot(lambdas, train.err, type = "b", xlab = "Shrinkage values", ylab = "Training MSE")

# shrinkage and test mse

set.seed(1)
test.err <- rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  boost.hitters <- gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  yhat <- predict(boost.hitters, Hitters.test, n.trees = 1000)
  test.err[i] <- mean((yhat - Hitters.test$Salary)^2)
}
plot(lambdas, test.err, type = "b", xlab = "Shrinkage values", ylab = "Test MSE")

min(test.err)
lambdas[which.min(test.err)]
