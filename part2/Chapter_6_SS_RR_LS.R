## Chapter 6 
### Lab 1 : Subset Selection methods

library(ISLR)

## BEST SUBSET SELECTION
fix(Hitters)
names(Hitters)

sum(is.na(Hitters$Salary))

Hitters=na.omit(Hitters)

library(leaps)

# the funtion regsubsets will give best subset selection (RSS)
regfit.full=regsubsets(Salary~.,data=Hitters)
summary(regfit.full)
# best 2 variable model is row 2 --> Hits and CRBI
reg.summ=summary(regfit.full)

# plot RSS,R2,cp,BIC
par(mfrow=c(2,2))
plot(reg.summ$rss,xlab="Number of variables",ylab="RSS",type="l")
plot(reg.summ$adjr2,xlab="Number of variables",ylab="Adjusted R2q",type="l")

which.max(reg.summ$adjr2)
points (11, reg.summ$adjr2[11], col ="red",cex =2, pch =20)

par(mfrow=c(1,1))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

names(Hitters)

## FORWARD AND BACKWARD STEPWISE

regfit.fwd=regsubsets(Salary~.,data=Hitters,method="forward")
summary(regfit.fwd)

regfit.bck=regsubsets(Salary~.,data=Hitters,method="backward")
summary(regfit.bck)


## Choosing among models useing Cross validation
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test=(!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,], nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
for (i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]= mean(( Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,5)

## LAB 2: Ridge regression and Lasso

x=model.matrix(Salary~.,data=Hitters)[,-1]
y=Hitters$Salary

grid=10^seq(10,-2,length=100)


### RIDGE


ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod ))
# 20 for each predictor + intercept and 100 columns one for each lambda

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

# find ridge regresion coef for a new value of lambda
predict(ridge.mod,s=50,type="coefficients")[1:20,]

# now splot into training and testing to estimate test error
# can split into a vector of true and false 
# can split based on random numbers

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)

y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)


ridge.mod_ls=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred_ls=predict(ridge.mod_ls,s=0,newx=x[test,],exact=T)
mean((ridge.pred_ls-y.test)^2)


# Using cross validation
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlambda=cv.out$lambda.min
bestlambda


# what is the test MSe for this lambda

ridge.pred=predict(ridge.mod,s=bestlambda,newx=x[test,])
mean((ridge.pred-y.test)^2)

# then the coef are
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlambda)[1:20,]

