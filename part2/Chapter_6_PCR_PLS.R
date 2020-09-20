## Chapter 6: Linear Model Selection and Regularization

## lab 3: PCR and PLS regression

library(ISLR)
library(pls)

attach(Hitters)
set.seed(2)
pcr.fit=pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")

summary(pcr.fit)
# so pcr will compute the root mean squared error so to get the usually MSE we need to square this
# RMSD=352.8
# MSE: 352.8^2

validationplot(pcr.fit,val.type = "MSEP")
# when M=number of predictors then there is no dimension reduction and its a least sqaures

dim(Hitters)

# using test and training data
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)

x=model.matrix(Salary~.,data=Hitters)[,-1]
y=Hitters$Salary
y.test=y[test]
pcr.fit=pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type = "MSEP")

cverr <- RMSEP(pcr.fit)$val[1,,]
imin <- which.min(cverr) - 1
imin

pcr.pred=predict(pcr.fit,x[test,],ncomp = 7)

mean((pcr.pred -y.test)^2)
